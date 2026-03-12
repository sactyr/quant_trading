# 01 INSTRUMENTS ----------------------------------------------------------

# ETF symbols (Yahoo Finance ASX tickers)
# Final universe — see decisions log for full instrument selection rationale
etf_symbols <- c(
  "VGS.AX",   # Vanguard MSCI World ex-Australia — global developed markets
  "VAS.AX",   # Vanguard Australian Shares — ASX 300
  "GOLD.AX"   # Perth Mint Physical Gold — uncorrelated diversifier
)

# Capital split per ETF (CAPS-weighted — see Decision 9 in decisions log)
# VGS: 0.507 / 0.904 = 56%, VAS: 0.275 / 0.904 = 30%, GOLD: 0.122 / 0.904 = 14%
etf_splits <- c(
  VGS.AX  = 0.56,
  VAS.AX  = 0.30,
  GOLD.AX = 0.14
)

# Best strategy per ETF (by CAPS from Monte Carlo — see decisions log)
etf_strategies <- list(
  VGS.AX  = list(strategy = "buy_hold",                stop_loss = 0.00),
  VAS.AX  = list(strategy = "macd_vol_dynamic_10_0.6", stop_loss = 0.10),
  GOLD.AX = list(strategy = "rsi",                     stop_loss = 0.10)
)


# 02 BACKTESTING PARAMETERS -----------------------------------------------

init_equity       <- 10000  # Starting equity for backtesting
min_window_length <- 250    # Minimum Monte Carlo window length (trading days)
n_samples         <- 1000   # Number of Monte Carlo samples

stop_losses     <- c(0, 0.02, 0.05, 0.1, 0.15)  # 0 = no stop loss
stop_losses_pct <- paste0(stop_losses * 100, "%")


# 03 FEE MODEL ------------------------------------------------------------

# IBKR ASX ETF commission: max(AUD $6 flat, 0.08% of trade value)
ibkr_min_fee  <- 6       # AUD minimum per trade
ibkr_fee_rate <- 0.0008  # 0.08% of trade value