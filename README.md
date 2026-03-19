# quant_trading

A systematic quantitative trading system for ASX-listed ETFs, built in R and
executed via Interactive Brokers (IBKR).

## Overview

This system uses Monte Carlo backtesting with CAPS scoring to select ETF strategies,
then executes daily signals via the IBKR Client Portal REST API.

**Final ETF universe:**
| Symbol  | Name                          | Strategy                | Split |
|---------|-------------------------------|-------------------------|-------|
| VGS.AX  | Vanguard MSCI World ex-Aus    | buy_hold                | 56%   |
| VAS.AX  | Vanguard Australian Shares    | macd_vol_dynamic_10_0.6 | 30%   |
| GOLD.AX | Perth Mint Physical Gold      | rsi                     | 14%   |

## Repository Structure

```
quant_trading/
├── R/
│   ├── backtesting/
│   │   ├── quant_backtesting.R            # Main backtesting script
│   │   ├── quant_backtesting_functions.R  # Strategy and helper functions
│   │   └── quant_backtesting_vars.R       # Parameters and ETF universe
│   └── live_trading/
│       ├── quant_vars.R                   # Live trading configuration
│       ├── quant_functions.R              # Signal generation and order sizing
│       ├── quant_trader.R                 # Main daily entry point
│       └── ibkr_api.R                     # IBKR Client Portal REST wrapper
├── outputs/
│   ├── backtesting/                       # Monte Carlo results (.rds)
│   └── live_trading/                      # Trade logs and state files
├── tests/
│   ├── test_backtesting.R
│   └── test_live_trading.R
├── quant_trading_decisions.md             # All decisions and findings
└── quant_trading_todo.md                  # Project to-do list
```

## Status

- ✅ Backtesting complete — ETF universe and strategies finalised
- 🔄 Live trading infrastructure — in progress
