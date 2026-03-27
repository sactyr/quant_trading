# =============================================================================
# quant_vars.R
# Shared configuration for all quant trading scripts
#
# Location: R/quant_vars.R
# Sourced by:
#   - R/live_trading/quant_trader.R
#   - R/live_trading/quant_functions.R
#   - R/fetch_historical_prices/quant_fetch_price_hist.R
#   - R/dashboard/quant_dashboard.R
#
# Setup:
#   Add the following to your .Renviron file (run usethis::edit_r_environ()):
#     IBKR_ACCOUNT_ID=U1234567
#   Then restart R.
# =============================================================================

# IBKR connection --------------------------------------------------------------

# Account ID is read from .Renviron — never hardcode this in source files
ibkr_account_id <- Sys.getenv("IBKR_ACCOUNT_ID")

if (ibkr_account_id == "") {
  stop(
    "IBKR_ACCOUNT_ID environment variable is not set. ",
    "Add IBKR_ACCOUNT_ID=U1234567 to your .Renviron file ",
    "(run usethis::edit_r_environ()) then restart R."
  )
}

# ETF universe -----------------------------------------------------------------

etf_symbols <- c("VGS.AX", "VAS.AX", "GOLD.AX")

# Symbols without .AX suffix — used for IBKR conid lookup
etf_symbols_ibkr <- c("VGS", "VAS", "GOLD")

# Best strategy per ETF (from Monte Carlo backtesting)
etf_strategies <- list(
  VGS.AX  = list(strategy = "buy_hold",                stop_loss = 0.00),
  VAS.AX  = list(strategy = "macd_vol_dynamic_10_0.6", stop_loss = 0.10),
  GOLD.AX = list(strategy = "rsi",                     stop_loss = 0.10)
)

# Capital configuration --------------------------------------------------------

# Total capital allocated to live trading (AUD) — used as fallback on first run
# if IBKR cash balance cannot be fetched
total_capital <- 5000

# Capital split per ETF (CAPS-weighted, Decision 9)
etf_splits <- c(
  VGS.AX  = 0.56,
  VAS.AX  = 0.30,
  GOLD.AX = 0.14
)

# Capital bucket per ETF (AUD) — each ETF trades only within its own bucket
etf_buckets <- round(total_capital * etf_splits, 2)

# Fee parameters ---------------------------------------------------------------

ibkr_min_fee  <- 6        # AUD minimum per trade
ibkr_fee_rate <- 0.0008   # 0.08% of trade value

# File paths -------------------------------------------------------------------

# Resolve project root relative to this script's location
project_root <- here::here()

# State file — tracks current positions and cash per bucket across sessions
state_file <- file.path(project_root, "outputs", "live_trading", "state.rds")

# Trade log — append-only record of every order placed
trade_log_file <- file.path(project_root, "outputs", "live_trading", "trade_log.csv")

# Price history folder — one .rds file per ETF, maintained by quant_fetch_price_hist.R
prices_dir <- file.path(project_root, "outputs", "live_trading", "prices")

# Log directories
live_trading_log_dir  <- file.path(project_root, "outputs", "live_trading", "logs")
price_fetch_log_dir   <- file.path(project_root, "outputs", "live_trading", "logs")

# Signal parameters ------------------------------------------------------------

# RSI parameters (GOLD.AX)
rsi_n_period <- 14
rsi_lower    <- 30
rsi_upper    <- 70

# MACD-V dynamic parameters (VAS.AX)
macd_vol_rolling_window <- 10
macd_vol_quantile       <- 0.6
