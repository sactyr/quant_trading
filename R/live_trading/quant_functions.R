# =============================================================================
# quant_functions.R
# Live trading logic
#
# Provides signal generation, order sizing, state management, and trade
# logging for the live trading system.
#
# Dependencies:
#   - quant_vars.R (sourced by quant_trader.R before this file)
#   - quant_backtesting_functions.R (sourced below for strategy logic)
#   - quantmod, TTR, dplyr, readr, lubridate, logger
# =============================================================================

suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(logger)
})

# Set timezone to AEST/AEDT
Sys.setenv(TZ = "Australia/Sydney")

# Reuse strategy signal logic from backtesting — single source of truth
source(file.path(project_root, "R", "backtesting", "quant_backtesting_functions.R"))

# Logging ----------------------------------------------------------------------

#' Get standard log layout formatter
#'
#' Returns a logger layout formatter that produces consistently formatted log
#' messages with aligned timestamps, log levels, and messages. Used across all
#' scripts to ensure uniform logging output.
#'
#' @return A layout_glue_generator object that formats log messages as:
#'   `[YYYY-MM-DD HH:MM:SS] | LEVEL   | message`
#' @examples
#' \dontrun{
#' log_layout(get_log_layout())
#' # [2026-03-15 10:00:01] | INFO    | Starting process
#' # [2026-03-15 10:00:02] | SUCCESS | Process complete
#' }
get_log_layout <- function() {
  layout_glue_generator(
    format = '[{format(time, "%Y-%m-%d %H:%M:%S")}] | {toupper(sprintf("%-7s", level))} | {msg}'
  )
}

# State management -------------------------------------------------------------

#' Initialise or load the trading state file
#'
#' The state file tracks units held and cash available per ETF bucket across
#' daily runs. On first run (no state file exists), initialises from
#' quant_vars.R bucket sizes with zero units held.
#'
#' State structure (one row per ETF):
#'   symbol  | units_held | cash_available | last_updated
#'   VGS.AX  | 0          | 2800.00        | 2026-03-15
#'   VAS.AX  | 0          | 1500.00        | 2026-03-15
#'   GOLD.AX | 0          | 700.00         | 2026-03-15
#'
#' @return Data frame representing current state
load_state <- function() {
  if (!file.exists(state_file)) {
    message("No state file found — initialising from quant_vars.R bucket sizes.")

    state <- data.frame(
      symbol         = etf_symbols,
      units_held     = 0L,
      cash_available = as.numeric(etf_buckets),
      last_updated   = as.character(Sys.Date()),
      stringsAsFactors = FALSE
    )

    save_state(state)
    return(state)
  }

  readRDS(state_file)
}

#' Save the trading state to disk
#'
#' @param state Data frame as returned by load_state()
save_state <- function(state) {
  state$last_updated <- as.character(Sys.Date())
  dir.create(dirname(state_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(state, state_file)
  message("State saved to: ", state_file)
}

#' Update state for a single ETF after a trade
#'
#' @param state Current state data frame
#' @param symbol ETF symbol (e.g. "VGS.AX")
#' @param units_delta Change in units (positive = bought, negative = sold)
#' @param cash_delta Change in cash (negative = spent, positive = received)
#' @return Updated state data frame
update_state <- function(state, symbol, units_delta, cash_delta) {
  idx <- which(state$symbol == symbol)

  if (length(idx) == 0) {
    stop(sprintf("Symbol %s not found in state.", symbol))
  }

  state$units_held[idx]     <- state$units_held[idx] + units_delta
  state$cash_available[idx] <- state$cash_available[idx] + cash_delta

  if (state$units_held[idx] < 0) {
    stop(sprintf("units_held for %s has gone negative — something is wrong.", symbol))
  }
  if (state$cash_available[idx] < 0) {
    stop(sprintf("cash_available for %s has gone negative — something is wrong.", symbol))
  }

  state
}

# Trade logging ----------------------------------------------------------------

#' Append a trade record to the trade log CSV
#'
#' Creates the log file with a header if it does not exist.
#'
#' @param symbol ETF symbol
#' @param side "BUY" or "SELL"
#' @param units Number of shares traded
#' @param price Execution price per share (AUD)
#' @param fee Estimated fee (AUD)
#' @param signal Signal that triggered the trade (e.g. "rsi_buy", "stop_loss")
log_trade <- function(symbol, side, units, price, fee, signal) {
  record <- data.frame(
    date      = as.character(Sys.Date()),
    symbol    = symbol,
    side      = side,
    units     = units,
    price     = price,
    fee       = fee,
    value     = round(units * price, 2),
    net_value = round(units * price + ifelse(side == "BUY", fee, -fee), 2),
    signal    = signal,
    stringsAsFactors = FALSE
  )

  dir.create(dirname(trade_log_file), showWarnings = FALSE, recursive = TRUE)

  write_csv(
    record,
    trade_log_file,
    append    = file.exists(trade_log_file),
    col_names = !file.exists(trade_log_file)
  )

  message(sprintf(
    "Trade logged: %s %s %d @ $%.2f (fee $%.2f, signal: %s)",
    side, symbol, units, price, fee, signal
  ))
}

# Fee calculation --------------------------------------------------------------

#' Estimate IBKR brokerage fee for a trade
#'
#' @param trade_value Total value of the trade (units * price) in AUD
#' @return Estimated fee in AUD
estimate_fee <- function(trade_value) {
  max(ibkr_min_fee, trade_value * ibkr_fee_rate)
}

# Order sizing -----------------------------------------------------------------

#' Calculate the number of shares to buy given available cash
#'
#' Accounts for brokerage fees so the total cost (shares + fee) does not
#' exceed available cash.
#'
#' @param cash_available Cash available in the bucket (AUD)
#' @param price Current price per share (AUD)
#' @return Integer number of shares to buy (0 if even 1 share is unaffordable)
calculate_buy_units <- function(cash_available, price) {
  if (price <= 0) stop("price must be positive")

  max_units <- floor(cash_available / price)
  if (max_units == 0) return(0L)

  for (n in max_units:1) {
    total_cost <- n * price + estimate_fee(n * price)
    if (total_cost <= cash_available) return(as.integer(n))
  }

  0L
}

# Signal generation ------------------------------------------------------------

#' Generate today's trading signal for a single ETF
#'
#' Applies the ETF's assigned strategy to a vector of daily closing prices
#' and returns the latest signal.
#'
#' @param symbol ETF symbol including .AX suffix (e.g. "VGS.AX")
#' @param close_prices Numeric vector of daily closing prices in chronological
#'   order (as returned by price_history[[symbol]]$close in quant_trader.R)
#' @return Character string: "buy", "sell", or "hold"
generate_signal <- function(symbol, close_prices) {
  strategy_config <- etf_strategies[[symbol]]

  if (is.null(strategy_config)) {
    stop(sprintf("No strategy configured for symbol: %s", symbol))
  }

  strategy <- strategy_config$strategy

  # buy_hold never generates a sell signal
  if (strategy == "buy_hold") return("buy")

  signal <- switch(strategy,

    "rsi" = {
      rsi_values <- TTR::RSI(close_prices, n = rsi_n_period)
      latest_rsi <- tail(rsi_values, 1)
      if (is.na(latest_rsi))       "hold"
      else if (latest_rsi < rsi_lower) "buy"
      else if (latest_rsi > rsi_upper) "sell"
      else                             "hold"
    },

    {
      if (!grepl("macd_vol_dynamic", strategy)) {
        stop(sprintf("Unrecognised strategy '%s' for symbol %s", strategy, symbol))
      }

      signals <- generate_macd_vol_dynamic_signals(
        prices          = close_prices,
        rolling_window  = macd_vol_rolling_window,
        quantile_thresh = macd_vol_quantile
      )

      as.character(tail(signals, 1))
    }
  )

  signal
}

#' Check whether stop loss has been triggered for a held position
#'
#' Compares current price against the average cost from IBKR positions.
#'
#' @param symbol ETF symbol
#' @param current_price Current market price (AUD)
#' @param avg_cost Average cost per unit from IBKR positions
#' @param stop_loss_pct Stop loss threshold as a decimal (e.g. 0.10 for 10%)
#' @return TRUE if stop loss triggered, FALSE otherwise
is_stop_loss_triggered <- function(symbol, current_price, avg_cost, stop_loss_pct) {
  if (stop_loss_pct == 0)            return(FALSE)
  if (is.na(avg_cost) || avg_cost <= 0) return(FALSE)

  drawdown  <- (current_price - avg_cost) / avg_cost
  triggered <- drawdown <= -stop_loss_pct

  if (triggered) {
    message(sprintf(
      "Stop loss triggered for %s: current $%.2f vs avg cost $%.2f (%.1f%% drawdown vs %.0f%% threshold)",
      symbol, current_price, avg_cost, drawdown * 100, stop_loss_pct * 100
    ))
  }

  triggered
}
