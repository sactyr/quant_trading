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
#   On the VM: IBKR_ACCOUNT_ID is injected as an environment variable by
#   start.sh and fetch_prices.sh via GCP Secret Manager at runtime.
#   For local development only: add IBKR_ACCOUNT_ID=U1234567 to .Renviron
#   (run usethis::edit_r_environ()) then restart R.
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

etf_symbols <- c("VGS.AX", "VVLU.AX")

# Symbols without .AX suffix — used for IBKR conid lookup
etf_symbols_ibkr <- c("VGS", "VVLU")

# Best strategy per ETF (from Monte Carlo backtesting, post-2027 regime)
# VGS:  macd_vol_fixed_20, SL5  — CAPS 0.467, geo_mean_CAGR 8.59%
# VVLU: rsi, SL10             — CAPS 0.621, geo_mean_CAGR 9.04%
# Portfolio split optimised at 60/40 VGS/VVLU (CAPS 0.559, CAGR 8.79%)
etf_strategies <- list(
  VGS.AX  = list(strategy = "macd_vol_fixed_20", stop_loss = 0.05),
  VVLU.AX = list(strategy = "rsi",               stop_loss = 0.10)
)

# Capital configuration --------------------------------------------------------

# Total capital allocated to live trading (AUD) — used as fallback on first run
# if IBKR cash balance cannot be fetched
total_capital <- 5000

# Capital split per ETF — 60/40 VGS/VVLU (Decision 10, May 2026)
etf_splits <- c(
  VGS.AX  = 0.60,
  VVLU.AX = 0.40
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
live_trading_log_dir  <- file.path(project_root, "outputs", "live_trading", "logs", "quant_trader")
price_fetch_log_dir   <- file.path(project_root, "outputs", "live_trading", "logs", "quant_fetch_price_hist")

# Signal parameters ------------------------------------------------------------

# RSI parameters (VVLU.AX)
rsi_n_period <- 14
rsi_lower    <- 30
rsi_upper    <- 70

# MACD-V fixed parameters (VGS.AX)
# macd_vol_fixed_20 uses strat_macdv() with strength_threshold = 20
macd_vol_fixed_threshold <- 20