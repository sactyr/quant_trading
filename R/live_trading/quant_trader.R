# =============================================================================
# quant_trader.R
# Main daily entry point for live trading
#
# Run this script once per trading day after ASX market open (10:00 AM AEST).
# It will:
#   1. Initialise logging for the day
#   2. Confirm the IBKR session is alive
#   3. Resolve ETF conids dynamically
#   4. Fetch daily price history from IBKR (signal generation + order sizing)
#   5. Load current state (positions and cash per bucket)
#   6. Cross-check state against live IBKR positions
#   7. For each ETF: generate signal, check stop loss, size and place order
#   8. Update and save state
#
# Prerequisites:
#   - Client Portal Gateway must be running and authenticated
#   - IBKR_ACCOUNT_ID must be set in .Renviron
# =============================================================================

suppressPackageStartupMessages({
  library(logger)
  library(here)
})

# Set timezone to AEST/AEDT
Sys.setenv(TZ = "Australia/Sydney")

# Sources ----------------------------------------------------------------------

source(here("R", "live_trading", "ibkr_api.R"))
source(here("R", "live_trading", "quant_vars.R"))
source(here("R", "live_trading", "quant_functions.R"))

# Logging setup ----------------------------------------------------------------

log_dir  <- here("outputs", "live_trading", "logs")
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
  names(conids) <- etf_symbols   # re-attach .AX suffix for downstream use
  log_info("Conids resolved: {paste(names(conids), conids, sep = '=', collapse = ', ')}")
}, error = function(e) {
  log_error("Failed to resolve conids: {e$message}")
  stop(e)
})

# Step 3: Fetch price history --------------------------------------------------

log_info("Step 3: Fetching daily price history from IBKR (1 year)...")

price_history <- list()

for (symbol in etf_symbols) {
  tryCatch({
    price_history[[symbol]] <- ibkr_get_price_history(conids[symbol], period = "1y")
    latest_close <- tail(price_history[[symbol]]$close, 1)
    log_info("  {symbol}: {nrow(price_history[[symbol]])} bars fetched, latest close ${latest_close}")
  }, error = function(e) {
    log_error("Failed to fetch price history for {symbol}: {e$message}")
    stop(e)
  })
}

# Step 4: Load state -----------------------------------------------------------

log_info("Step 4: Loading state...")

# Fetch total cash from IBKR to initialise buckets on first run
ibkr_cash <- tryCatch({
  summary <- ibkr_get_summary(ibkr_account_id)
  cash <- as.numeric(summary$totalcashvalue$amount)
  log_info("IBKR total cash balance: ${cash}")
  cash
}, error = function(e) {
  log_warn("Could not fetch IBKR cash balance: {e$message}. Will use total_capital fallback if needed.")
  NULL
})

state <- load_state(ibkr_cash = ibkr_cash)

log_info("Current state:")
for (i in seq_len(nrow(state))) {
  log_info(
    "  {state$symbol[i]}: {state$units_held[i]} units held, ${state$cash_available[i]} cash available"
  )
}

# Step 5: Cross-check against IBKR positions -----------------------------------

log_info("Step 5: Fetching IBKR positions for cross-check...")

tryCatch({
  ibkr_positions <- ibkr_get_positions(ibkr_account_id)

  if (nrow(ibkr_positions) > 0) {
    log_info("IBKR positions:")
    for (i in seq_len(nrow(ibkr_positions))) {
      log_info(
        "  {ibkr_positions$symbol[i]}: {ibkr_positions$position[i]} units @ avg cost ${ibkr_positions$avg_cost[i]}"
      )
    }
  } else {
    log_info("No open positions on IBKR.")
  }
}, error = function(e) {
  log_warn("Could not fetch IBKR positions: {e$message}. Proceeding with local state only.")
  ibkr_positions <- data.frame()
})

# Step 6: Generate signals and trade -------------------------------------------

log_info("Step 6: Generating signals and placing orders...")

for (symbol in etf_symbols) {

  log_info("--- {symbol} ---")

  strategy_config <- etf_strategies[[symbol]]
  stop_loss_pct   <- strategy_config$stop_loss

  state_row     <- state[state$symbol == symbol, ]
  units_held    <- state_row$units_held
  cash_avail    <- state_row$cash_available
  current_price <- tail(price_history[[symbol]]$close, 1)

  if (is.na(current_price) || current_price <= 0) {
    log_warn("Invalid price for {symbol} (${current_price}) — skipping.")
    next
  }

  log_info("Units held: {units_held} | Cash available: ${cash_avail} | Current price: ${current_price}")

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
    sig <- generate_signal(symbol, price_history[[symbol]])
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
      log_warn("Insufficient cash to buy even 1 unit of {symbol} (cash: ${cash_avail}, price: ${current_price}) — skipping.")
      next
    }

    tryCatch({
      ibkr_place_order(ibkr_account_id, conids[symbol], "BUY", units_to_buy)

      cost           <- units_to_buy * current_price
      fee            <- estimate_fee(cost)
      net_cash_spent <- -(cost + fee)

      state <- update_state(state, symbol, units_to_buy, net_cash_spent)
      log_trade(symbol, "BUY", units_to_buy, current_price, fee, signal)
      log_info("BUY order placed: {units_to_buy} units of {symbol} @ ${current_price} (fee ${fee})")
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
      log_info("SELL order placed: {units_held} units of {symbol} @ ${current_price} (fee ${fee})")
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

# Done -------------------------------------------------------------------------

log_info("=============================================================")
log_info("quant_trader.R completed — {Sys.time()}")
log_info("=============================================================")
