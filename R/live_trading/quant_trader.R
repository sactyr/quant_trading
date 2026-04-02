# =============================================================================
# quant_trader.R
# Main daily entry point for live trading
#
# Run this script once per trading day at 10:15 AM AEDT via cron.
# It will:
#   1. Confirm the IBKR session is alive
#   2. Resolve ETF conids dynamically
#   3. Load current price from saved history (maintained by quant_fetch_price_hist.R)
#   4. Fetch IBKR positions (used for state initialisation and stop loss)
#   5. Load current state (positions and cash per bucket)
#   6. For each ETF: generate signal, check stop loss, size and place order
#   7. Save updated state
#
# Prerequisites:
#   - Client Portal Gateway must be running and authenticated
#   - IBKR_ACCOUNT_ID must be set in .Renviron
#   - quant_fetch_price_hist.R must have run at least once to create price files
# =============================================================================

suppressPackageStartupMessages({
  library(logger)
  library(here)
})

Sys.setenv(TZ = "Australia/Sydney")

# Sources ----------------------------------------------------------------------

source(here("R", "live_trading", "ibkr_api.R"))
source(here("R", "quant_vars.R"))
source(here("R", "live_trading", "quant_functions.R"))

# Logging setup ----------------------------------------------------------------

log_dir  <- live_trading_log_dir
log_file <- file.path(log_dir, sprintf("quant_trader_%s.log", Sys.Date()))

dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

log_appender(appender_tee(log_file))
log_threshold(DEBUG)
log_formatter(formatter_glue_or_sprintf)
log_layout(get_log_layout())

log_info("=============================================================")
log_info("quant_trader.R started — {Sys.time()}")
log_info("Account: {ibkr_account_id}")
log_info("ETF universe: {paste(etf_symbols, collapse = ', ')}")
log_info("Total capital (fallback): ${total_capital}")
log_info("=============================================================")

# Step 1: Confirm IBKR session -------------------------------------------------

log_info("Step 1: Confirming IBKR session...")

tryCatch({
  ibkr_tickle()
  log_info("IBKR session confirmed.")
}, error = function(e) {
  log_error("IBKR session check failed: {e$message}")
  stop(e)
})

# Step 2: Resolve conids -------------------------------------------------------

log_info("Step 2: Resolving conids for {paste(etf_symbols_ibkr, collapse = ', ')}...")

tryCatch({
  conids <- ibkr_get_conids(etf_symbols_ibkr)
  names(conids) <- etf_symbols
  log_info("Conids resolved: {paste(names(conids), conids, sep = '=', collapse = ', ')}")
}, error = function(e) {
  log_error("Failed to resolve conids: {e$message}")
  stop(e)
})

# Step 3: Load current prices from saved history --------------------------------

log_info("Step 3: Loading current prices from saved price history...")

# Calculate the most recent trading day before today
# If today is Monday, previous trading day is Friday (3 days back)
today <- Sys.Date()
days_back <- switch(weekdays(today),
  "Monday"  = 3L,
  "Sunday"  = 2L,  # shouldn't run on Sunday but just in case
  1L                # Tuesday-Saturday: previous calendar day
)
expected_latest_date <- today - days_back
log_info("Expected latest price date: {expected_latest_date}")

current_prices <- list()

for (symbol in etf_symbols) {
  tryCatch({
    hist_file <- file.path(prices_dir, paste0(symbol, ".rds"))

    if (!file.exists(hist_file)) {
      log_error("No price history file for {symbol} at {hist_file}.")
      log_error("Run quant_fetch_price_hist.R first to initialise price history.")
      stop(sprintf("Missing price history file for %s", symbol))
    }

    price_df     <- readRDS(hist_file)
    latest_close <- tail(price_df$close, 1)
    latest_date  <- tail(price_df$date, 1)

    # Staleness check — abort if price data is more than 1 trading day behind
    if (latest_date < expected_latest_date) {
      log_error(
        "Price history for {symbol} is stale — latest date is {latest_date}, expected {expected_latest_date}."
      )
      log_error(
        "Price fetch likely failed yesterday. Check quant_fetch_price_hist logs and run fetch_prices.sh manually."
      )
      stop(sprintf("Stale price history for %s: %s < %s", symbol, latest_date, expected_latest_date))
    }

    current_prices[[symbol]] <- latest_close
    log_info("  {symbol}: {nrow(price_df)} bars loaded, latest close ${latest_close} ({latest_date})")

  }, error = function(e) {
    log_error("Failed to load price history for {symbol}: {e$message}")
    stop(e)
  })
}

# Step 4: Fetch IBKR positions (needed for state initialisation) ---------------

log_info("Step 4: Fetching IBKR positions...")

ibkr_positions <- tryCatch({
  positions <- ibkr_get_positions(ibkr_account_id)
  if (nrow(positions) > 0) {
    log_info("IBKR positions:")
    for (i in seq_len(nrow(positions))) {
      log_info(
        "  {positions$symbol[i]}: {positions$position[i]} units @ avg cost ${positions$avg_cost[i]}"
      )
    }
  } else {
    log_info("No open positions on IBKR.")
  }
  positions
}, error = function(e) {
  log_warn("Could not fetch IBKR positions: {e$message}. Proceeding with local state only.")
  data.frame()
})

# Step 5: Load state -----------------------------------------------------------

log_info("Step 5: Loading state...")

ibkr_cash <- tryCatch({
  summary <- ibkr_get_summary(ibkr_account_id)
  cash <- as.numeric(summary$totalcashvalue$amount)
  log_info("IBKR total cash balance: ${sprintf('%.2f', cash)}")
  cash
}, error = function(e) {
  log_warn("Could not fetch IBKR cash balance: {e$message}. Will use total_capital fallback if needed.")
  NULL
})

state <- load_state(ibkr_cash = ibkr_cash, ibkr_positions = ibkr_positions)

log_info("Current state:")
for (i in seq_len(nrow(state))) {
  log_info(
    "  {state$symbol[i]}: {state$units_held[i]} units held, ${sprintf('%.2f', state$cash_available[i])} cash available"
  )
}

# Step 6: Generate signals and trade -------------------------------------------

log_info("Step 6: Generating signals and placing orders...")

for (symbol in etf_symbols) {

  log_info("--- {symbol} ---")

  strategy_config <- etf_strategies[[symbol]]
  stop_loss_pct   <- strategy_config$stop_loss

  state_row     <- state[state$symbol == symbol, ]
  units_held    <- state_row$units_held
  cash_avail    <- state_row$cash_available
  current_price <- current_prices[[symbol]]

  if (is.null(current_price) || is.na(current_price) || current_price <= 0) {
    log_warn("Invalid price for {symbol} (${current_price}) — skipping.")
    next
  }

  log_info("Units held: {units_held} | Cash available: ${sprintf('%.2f', cash_avail)} | Current price: ${current_price}")

  # Stop loss check (only if holding a position) ----
  if (units_held > 0 && stop_loss_pct > 0) {

    avg_cost <- NA
    if (nrow(ibkr_positions) > 0) {
      match_row <- ibkr_positions[ibkr_positions$symbol == symbol, ]
      if (nrow(match_row) > 0) avg_cost <- match_row$avg_cost[1]
    }

    if (!is.na(avg_cost) && is_stop_loss_triggered(symbol, current_price, avg_cost, stop_loss_pct)) {
      log_warn("Stop loss triggered for {symbol} — placing SELL order.")

      tryCatch({
        ibkr_place_order(ibkr_account_id, conids[symbol], "SELL", units_held)

        proceeds <- units_held * current_price
        fee      <- estimate_fee(proceeds)
        net_cash <- proceeds - fee

        state <- update_state(state, symbol, -units_held, net_cash)
        log_trade(symbol, "SELL", units_held, current_price, fee, "stop_loss")
        log_info("SELL order placed: {units_held} units of {symbol} @ ${current_price} (fee ${fee})")
      }, error = function(e) {
        log_error("Failed to place SELL order for {symbol}: {e$message}")
      })

      next
    }
  }

  # Signal generation ----
  signal <- tryCatch({
    sig <- generate_signal(symbol, units_held)
    log_info("Signal for {symbol}: {sig}")
    sig
  }, error = function(e) {
    log_error("Failed to generate signal for {symbol}: {e$message}")
    "hold"
  })

  # Execute signal ----
  if (signal == "buy" && units_held == 0) {

    units_to_buy <- calculate_buy_units(cash_avail, current_price)

    if (units_to_buy == 0) {
      log_warn("Insufficient cash to buy even 1 unit of {symbol} — skipping.")
      next
    }

    tryCatch({
      ibkr_place_order(ibkr_account_id, conids[symbol], "BUY", units_to_buy)

      cost           <- units_to_buy * current_price
      fee            <- estimate_fee(cost)
      net_cash_spent <- -(cost + fee)

      state <- update_state(state, symbol, units_to_buy, net_cash_spent)
      log_trade(symbol, "BUY", units_to_buy, current_price, fee, signal)
      log_info("BUY order placed: {units_to_buy} units of {symbol} @ ${current_price} (fee ${sprintf('%.4f', fee)})")
    }, error = function(e) {
      log_error("Failed to place BUY order for {symbol}: {e$message}")
    })

  } else if (signal == "sell" && units_held > 0) {

    tryCatch({
      ibkr_place_order(ibkr_account_id, conids[symbol], "SELL", units_held)

      proceeds <- units_held * current_price
      fee      <- estimate_fee(proceeds)
      net_cash <- proceeds - fee

      state <- update_state(state, symbol, -units_held, net_cash)
      log_trade(symbol, "SELL", units_held, current_price, fee, signal)
      log_info("SELL order placed: {units_held} units of {symbol} @ ${current_price} (fee ${sprintf('%.4f', fee)})")
    }, error = function(e) {
      log_error("Failed to place SELL order for {symbol}: {e$message}")
    })

  } else {
    log_info("No action for {symbol}: signal = '{signal}', units held = {units_held}")
  }
}

# Step 7: Save state -----------------------------------------------------------

log_info("Step 7: Saving state...")
save_state(state)
log_info("State saved.")

log_info("=============================================================")
log_info("quant_trader.R completed — {Sys.time()}")
log_info("=============================================================")
