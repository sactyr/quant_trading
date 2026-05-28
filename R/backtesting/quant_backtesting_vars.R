# 01 INSTRUMENTS ----------------------------------------------------------

# ETF symbols (Yahoo Finance ASX tickers)
# Final universe — see decisions log for full instrument selection rationale
etf_symbols <- c(
  "VGS.AX",   # Vanguard MSCI World ex-Australia — global developed markets
  "VVLU.AX"   # Vanguard Global Value Equity Active ETF — global value factor
)

# Capital split per ETF — 60/40 VGS/VVLU (Decision 10, May 2026)
# Optimised via portfolio-level CAPS sweep; 60/40 chosen over pure CAPS optimum
# (10/90) to account for VVLU's shorter history (7yr) and active management risk
etf_splits <- c(
  VGS.AX  = 0.60,
  VVLU.AX = 0.40
)

# Best strategy per ETF (by CAPS from Monte Carlo, post-2027 regime — see decisions log)
# VGS:  macd_vol_fixed_20, SL5  — CAPS 0.467, geo_mean_CAGR 8.59%
# VVLU: rsi, SL10             — CAPS 0.621, geo_mean_CAGR 9.04%
etf_strategies <- list(
  VGS.AX  = list(strategy = "macd_vol_fixed_20", stop_loss = 0.05),
  VVLU.AX = list(strategy = "rsi",               stop_loss = 0.10)
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


# 04 TAX PARAMETERS -------------------------------------------------------
marginal_tax_rate <- 0.37
cgt_inflation_pa  <- 0.03
tax_regimes       <- c("current", "post_2027")