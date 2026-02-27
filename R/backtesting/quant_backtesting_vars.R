# 01 INSTRUMENTS ----------------------------------------------------------

# ETF symbols (Yahoo Finance ASX tickers)
etf_symbols <- c(
  "STW.AX",   # SPDR S&P/ASX 200 - Australian equities
  "GOLD.AX",  # Perth Mint Physical Gold
  "SLF.AX",   # SPDR S&P/ASX 200 Listed Property
  "IOO.AX"    # iShares Global 100 ETF AUD
)

etf_end_date <- Sys.Date() - 2  # avoid potential NAs on recent days


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