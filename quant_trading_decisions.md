# Quant Trading - Decisions & Findings Log

## Project Overview
Systematic backtesting of ASX-listed ETFs using Monte Carlo simulation and CAPS
scoring methodology, with the goal of eventually live trading via Interactive
Brokers (IBKR). Ported and adapted from an earlier crypto trading system
(`crypto_trading` repo).

---

## Instrument Selection

**Final ETF universe (3 ETFs, all AUD-denominated, no FX risk):**
| Symbol  | Name                          | MER   | Inception  | Strategy                | Stop Loss | geo_mean_CAGR | CAPS  |
|---------|-------------------------------|-------|------------|-------------------------|-----------|---------------|-------|
| VGS.AX  | Vanguard MSCI World ex-Aus    | 0.18% | Nov 2014   | buy_hold                | 0%        | 12.6%         | 0.507 |
| VAS.AX  | Vanguard Australian Shares    | 0.07% | May 2009   | macd_vol_dynamic_10_0.6 | 10%       | 6.17%         | 0.275 |
| GOLD.AX | Perth Mint Physical Gold      | 0.15% | Mar 2003   | rsi                     | 10%       | 5.70%         | 0.122 |

**Capital split (CAPS-weighted):**
| Symbol  | Split |
|---------|-------|
| VGS.AX  | 56%   |
| VAS.AX  | 30%   |
| GOLD.AX | 14%   |

**Rejected instruments and reasons:**
- US-listed ETFs (EFA, GLD, SLV) — FX risk, AUD conversion complexity
- VESG.AX — only 7 years history, too short for Monte Carlo
- ETPMAG.AX (silver) — only 14 years history
- Thematic ETFs (RBTZ etc.) — too narrow, not interested in sector concentration
- SLF.AX — worst CAPS (0.080), geo_mean_CAGR 3.23% below 5.5% threshold (Decision 4)
- IOO.AX — replaced by VGS (Decision 3 reversed, Decision 7)
- STW.AX — dropped in favour of VAS (Decision 8); high overlap (ASX 200 ⊂ ASX 300),
  VAS broader and cheaper (MER 0.07% vs STW 0.19%)
- IEM.AX — CAPS 0.034 (worst in universe), geo_mean_CAGR 3.80% below 5.5% threshold
- IAF.AX — geo_mean_CAGR 1.81% well below 5.5% threshold
- VHY.AX — heavy overlap with VAS (both dominated by CBA and BHP), higher MER (0.25%),
  concentrated portfolio (65% in top 10 holdings)
- VEU.AX — overlap with VGS on European/Japanese developed markets; overlap with VAS
  on Australian equities (~4-5% of VEU); hidden tax drag ~0.29%/year due to
  US-domiciled structure; unique exposure (EM ~20-25%) not worth the cost
- VAE.AX — best active strategy (bb) delivers only 3.66% geo_mean_CAGR below 5.5%
  threshold; buy_hold CAGR of 7.23% irrelevant as system uses CAPS-ranked best strategy
- VGAD.AX — same underlying index as VGS but AUD-hedged; higher MER (0.21%);
  not a diversification play, just a currency bet
- VDHG.AX — fund-of-funds, overlaps all existing holdings, only incepted 2017
- VVLU.AX — actively managed, incepted ~2018, too short history

**Rationale for ASX-listed only:** User based in Australia, eliminates currency
conversion risk and complexity. All instruments AUD-denominated.

**CAGR threshold:** Any ETF whose CAPS-ranked best strategy produces geo_mean_CAGR
below 5.5% is excluded from the live portfolio. This eliminated IEM, IAF, SLF, and VAE.

---

## Fee Model

**IBKR ASX fee structure:**
- Minimum flat fee: AUD $6 per trade
- Percentage fee: 0.08% of trade value
- Applied as: `max(ibkr_min_fee, proceeds * ibkr_fee_rate)`
- Break-even point: $7,500 position size (where flat and % fee are equal)

**Implications for small accounts:**
- At $1,000: effective rate = 0.6% per trade (vs 0.08% advertised)
- At $5,000: 0.24% round-trip
- At $10,000: 0.12% round-trip
- Minimum recommended starting capital: $5,000-$10,000

**Two-layer fee structure:**
- Layer 1 — IBKR brokerage fee (per trade): modelled in backtesting as above
- Layer 2 — ETF Management Expense Ratio (MER): charged by fund manager annually,
  silently deducted from NAV, already baked into price data used in backtesting

**Variables in `quant_backtesting_vars.R`:**
```r
ibkr_min_fee  <- 6
ibkr_fee_rate <- 0.0008
init_equity   <- 10000
```

---

## Dividends / Distributions

ASX ETFs pay quarterly distributions (not technically "dividends" but functionally
similar). Through IBKR:
- Distributions are paid automatically into the cash balance on the payment date
  if the ETF is held on the ex-dividend date
- IBKR does not automatically reinvest distributions — cash accumulates in the bucket
- In live trading, distributions land as cash and are reinvested on the next buy signal
- Backtesting uses `Ad()` adjusted prices which already accounts for distributions,
  so CAGR figures implicitly include reinvested distributions — minor timing difference
  in live trading but not material

---

## Data

- **Source:** Yahoo Finance via `quantmod::getSymbols` (backtesting only)
- **Live trading price source:** IBKR Client Portal API via `ibkr_get_price_history()`
  — single data source for live signal generation and order sizing
- **Price column:** `Ad()` (adjusted close) — accounts for ETF distributions/dividends
- **End date:** `Sys.Date() - 2` — hardcoded as default in `get_market_data`, avoids
  Yahoo Finance returning intraday data when `to = NULL` is passed
- **NA handling:** `na.approx()` applied after fetching to interpolate mid-series NAs
  (caused by public holidays, trading halts etc.)
- **No hardcoded start dates** — fetch all available history per instrument

**Known issue fixed:** Passing `to = NULL` to `getSymbols` causes Yahoo Finance to
return intraday tick data instead of daily OHLCV. Fixed by defaulting
`get_market_data(to = Sys.Date() - 2)`.

---

## Backtesting Methodology

**Monte Carlo parameters:**
```r
n_samples         <- 1000
min_window_length <- 250
stop_losses       <- c(0, 0.02, 0.05, 0.1, 0.15)
```

**Strategies tested:**
- buy_hold
- sma (default period_n = 50)
- sma_cross (short_n = 20, long_n = 50)
- rsi (n_period = 14, lower = 30, upper = 70)
- bb (n = 20, sd = 2)
- macd (n_fast = 12, n_slow = 26, n_signal = 9)
- macd_vol / MACD-V (Spiroglou, fixed strength threshold grid: 20-80 by 10)
- macd_vol_dynamic_strength (rolling_window grid: 10-50 by 10, quantile grid: 0.5-0.9 by 0.1)

**CAPS scoring methodology:**
```
robust_calmar = geo_mean_CAGR / cvar_drawdown
prob_score    = win_rate_windows * median_Sharpe
CAPS          = robust_calmar * prob_score
```
- CAPS is a relative ranking metric only — not meaningful as an absolute score
- Designed to penalise high-risk/high-return strategies and reward consistency
- Use for within-instrument strategy ranking only, not cross-instrument comparison
- Cross-instrument comparison: use `geo_mean_CAGR` directly

**File naming convention (clean rerun, no dates):**
- Chunk files: `{SYMBOL}_SL{stop_loss}_monte_carlo_summary.rds`
- Combined files: `{SYMBOL}_monte_carlo_summary.rds`
- `etf_symbols` vector drives all naming — no filename-based extraction

---

## Monte Carlo Results (1000 samples) — Full Universe

### Best strategy per ETF by CAPS (top row of agg_summaries):
| Symbol  | Strategy                | Stop Loss | geo_mean_CAGR | CAPS  | Decision       |
|---------|-------------------------|-----------|---------------|-------|----------------|
| VGS.AX  | buy_hold                | 0%        | 12.6%         | 0.507 | ✅ Keep        |
| VAS.AX  | macd_vol_dynamic_10_0.6 | 10%       | 6.17%         | 0.275 | ✅ Keep        |
| VEU.AX  | macd_vol_fixed_40       | 10%       | 7.67%         | 0.262 | ❌ Dropped     |
| IOO.AX  | macd_vol_fixed_40       | 10%       | 10.7%         | 0.406 | ❌ Dropped     |
| STW.AX  | macd_vol_dynamic_10_0.5 | 10%       | 5.22%         | 0.129 | ❌ Dropped     |
| GOLD.AX | rsi                     | 10%       | 5.70%         | 0.122 | ✅ Keep        |
| IAF.AX  | macd_vol_fixed_40       | 5%        | 1.81%         | 0.207 | ❌ Below 5.5%  |
| SLF.AX  | macd_vol_dynamic_10_0.7 | 2%        | 3.23%         | 0.080 | ❌ Below 5.5%  |
| VAE.AX  | bb                      | 0%        | 3.66%         | 0.175 | ❌ Below 5.5%  |
| IEM.AX  | macd_vol_fixed_20       | 10%       | 3.80%         | 0.034 | ❌ Below 5.5%  |

### Key observations:
- VGS best strategy is buy_hold — reflects persistent secular uptrend of MSCI World;
  any active strategy that exits the market incurs opportunity cost during fast recoveries
- VAS best strategy is active (macd_vol_dynamic) — ASX has more choppiness and
  mean-reversion than global developed markets, making active signals worthwhile
- GOLD responds well to RSI — consistent with gold's mean-reverting/momentum character
- VGS tracks MSCI World ex-Australia — explicitly excludes Australian stocks, so
  VGS and VAS have zero overlap by index construction
- IAF (bonds) has decent CAPS (0.207) relative to its low CAGR — good risk-adjusted
  profile but excluded due to 1.81% CAGR falling well below the 5.5% threshold

---

## Portfolio Decisions

### Decision 1: Static Split ✅
Rather than dynamically adjusting allocations based on volatility or momentum,
capital is split between ETFs using a fixed ratio set upfront and only changed
manually. This keeps the implementation simple and predictable.

- Split ratio finalised as CAPS-weighted (see Decision 9)
- Rebalancing: only rebalance manually (e.g. annually), not automatically

**Rejected alternative — Dynamic Split:** Allocation varies based on recent
volatility (risk parity) or momentum. Rejected due to complexity of rebalancing
logic, additional fee events, and potential tax implications.

### Decision 2: Reserved Splits (Capital Buckets) ✅
Each ETF operates independently within its own dedicated capital bucket. Capital
is never shared between ETFs.

**Example with 56% VGS / 30% VAS / 14% GOLD on $10,000 total capital:**
- VGS bucket: $5,600
- VAS bucket: $3,000
- GOLD bucket: $1,400

Key properties:
- Each ETF only ever trades with its own bucket — no borrowing between buckets
- Idle cash within a bucket just sits until that ETF's own signal fires again
- Simple to implement — each ETF's backtest runs independently

**Rejected alternative — Shared Pool:** One capital pool allocated dynamically
to whichever ETF has an active signal. More capital-efficient but introduces
priority conflicts when multiple signals fire simultaneously and complexity
around pulling capital back when needed.

### Decision 3: Drop VGS.AX in Favour of IOO.AX ✅ (subsequently reversed — see Decision 7)

VGS.AX was initially removed from the instrument list. Analysis conducted 2026-03-06.

**Evidence at the time:**
- Daily return correlation between IOO and VGS: 0.90
- IOO outperformed VGS on equal time basis (post-2014): CAGR 15.0% vs 12.9%
- IOO has 7 additional years of history including the GFC

**Subsequently reversed** after clean Monte Carlo rerun revealed VGS CAPS 0.507 >
IOO CAPS 0.406, and MER comparison showed VGS cheaper (0.18% vs IOO 0.40%).
See Decision 7.

### Decision 4: Drop SLF.AX from Live Portfolio ✅

SLF.AX was removed from the instrument list. Analysis conducted 2026-03-06.

**Evidence:**
- geo_mean_CAGR: 3.23% — below 5.5% threshold
- CAPS: 0.080 — near bottom of universe
- Rolling 12-month correlation with STW/VAS trending to 0.70–0.75
- A-REIT diversification thesis has weakened substantially over the past decade

**Conclusion:** Poor returns AND eroding diversification. Dropped.

### Decision 5: Drop IEM.AX ✅
- geo_mean_CAGR: 3.80% — below 5.5% threshold
- CAPS: 0.034 — worst in universe
- Emerging markets volatile and mean-reverting; no active strategy adds consistent value

### Decision 6: Drop IAF.AX ✅
- geo_mean_CAGR: 1.81% — well below 5.5% threshold
- Despite decent CAPS (0.207) and low drawdowns, the absolute return is too low
  to justify a dedicated capital bucket

### Decision 7: Reinstate VGS, Drop IOO ✅
Decision 3 was reversed after clean Monte Carlo rerun. Analysis 2026-03-12.

**Evidence:**
- VGS CAPS 0.507 > IOO CAPS 0.406
- VGS geo_mean_CAGR 12.6% > IOO 10.7% (on full available history)
- VGS MER 0.18% vs IOO MER 0.40% — significant long-term compounding cost difference
- VGS tracks MSCI World (1,500+ stocks) vs IOO S&P Global 100 (100 stocks) —
  broader diversification at lower cost

**Conclusion:** VGS is superior to IOO on every dimension. IOO dropped.

### Decision 8: Drop STW.AX in Favour of VAS ✅
Analysis 2026-03-12.

**Evidence:**
- STW tracks ASX 200, VAS tracks ASX 300 — top 200 almost entirely contained within 300
- VAS MER 0.07% vs STW MER 0.19%
- VAS geo_mean_CAGR 6.17% > STW 5.22%
- VAS CAPS 0.275 > STW 0.129

**Conclusion:** VAS is strictly superior — broader, cheaper, better performance. STW dropped.

### Decision 9: CAPS-Weighted Capital Split ✅
Analysis 2026-03-12.

**Methodology:** Split ratio proportional to each ETF's best-strategy CAPS score.

| ETF     | CAPS  | Weight        | Split |
|---------|-------|---------------|-------|
| VGS.AX  | 0.507 | 0.507 / 0.904 | 56%   |
| VAS.AX  | 0.275 | 0.275 / 0.904 | 30%   |
| GOLD.AX | 0.122 | 0.122 / 0.904 | 14%   |

**Rationale:** CAPS balances return (geo_mean_CAGR) and risk quality (drawdown,
Sharpe, win rate), making it a more robust weighting basis than raw CAGR alone.
Gold at 14% is consistent with standard uncorrelated asset allocations (e.g.
Ray Dalio All Weather runs gold at 7.5%; many systematic portfolios 10-20%).

### Decision 10: IBKR Client Portal REST API (Option C) ✅
Analysis 2026-03-13.

**Options considered:**
- Option A — IB Gateway on cloud VM: heavy process, needs 24/7 VM, forced daily
  restart at ~11:45pm EST
- Option B — TWS/IB Gateway locally: machine must be on during ASX market hours
  (10am AEST), Windows Task Scheduler triggers R script
- Option C — Client Portal Gateway (REST API): lightweight Java process (~50MB),
  no GUI, exposes REST API on localhost, easier to deploy on cloud VM

**R package options investigated:**
- `IBrokers` (CRAN) — TWS socket only, Options A/B only
- `lbilli/rib` (CRAN/GitHub) — TWS socket only, Options A/B only, requires TWS v10.40+
- `vspinu/rib` (GitHub) — wraps official C++ TWS client, Options A/B only
- No R package exists for the Client Portal REST API

**Decision:** Use Option C (Client Portal REST API) via `httr2` in R.

**Proto-package approach:** Build IBKR Client Portal REST wrapper as a single R script
(`R/live_trading/ibkr_api.R`) within `quant_trading` first. Once stable, extract into
a standalone R package in a separate GitHub repo. Swap `source("R/live_trading/ibkr_api.R")`
for `library(ibkr)` (or chosen package name) at that point.

### Decision 11: quant_trading Folder Structure ✅
Analysis 2026-03-13.

**Agreed structure:**
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
├── quant_trading.Rproj
├── quant_trading_decisions.md
├── quant_trading_todo.md
├── README.md
└── LICENSE
```

Note: `quant_get_price_history.R` dropped — price history is fetched inside
`quant_trader.R` via `ibkr_get_price_history()` on each daily run. No local
price cache needed.

### Decision 12: Cloud Infrastructure — GCP + ib-gateway-service ✅
Analysis 2026-03-15.

**Headless authentication approach selected:** `ib-gateway-service`
(https://github.com/michaeljherrmann/ib-gateway-service)
- Node.js/Docker tool that handles Client Portal Gateway startup and headless login
- Uses TOTP (Time-based One-Time Password) for fully automated 2FA — no phone
  approval or SMS required at runtime
- TOTP secret extracted once from authenticator app (e.g. 2FAS) and stored in
  GCP Secret Manager

**Authentication options considered and rejected:**
- OAuth 1.0a — institutional clients only, not available to retail IBKR Pro accounts
- IBeam + SMS — requires Twilio number routing or Google Messages scraping, fragile
- IBeam + IB Key phone approval — requires phone interaction at each login
- Manual daily login — not viable for fully automated system

**ib-gateway-service authentication flow (discovered during implementation):**
1. Start Docker container (no credentials at startup)
2. `POST http://localhost:5050/api/service` — starts the IB Gateway process
3. Wait ~20 seconds for gateway to initialise
4. `PUT http://localhost:5050/api/service` with JSON body containing username,
   password, and optionally totpSecret — triggers login
5. Poll `/v1/api/tickle` on port 5000 until `authenticated: true`
6. IBKR REST API available on port 5000

**Cloud platform:** GCP (Google Cloud Platform)
- VM: e2-micro (Always Free tier, no expiry, ~$0/month)
- Region: us-central1 (required for Always Free tier)
- OS: Ubuntu 24.04, 30GB standard persistent disk
- Credentials: GCP Secret Manager
- Firewall: port 5000 restricted to localhost only
- Scheduling: cron job on VM (`15 10 * * 1-5`, timezone: Australia/Sydney)
- VM timezone set to Australia/Sydney for correct cron scheduling

**Security mitigations:**
- Dedicated IBKR API username with trading-only permissions (no withdrawal access)
- All credentials stored in GCP Secret Manager — never written to disk or repo
- `get_secret_optional()` for TOTP secret — handles missing secret gracefully
- VM firewall blocks port 5000 from external access

**Note:** GCP previously caused authentication friction (gcloud CLI scope issue).
The `gcloud secrets` CLI command fails due to OAuth scope limitations, but the
Secret Manager REST API works correctly. All secret access uses REST API directly.

### Decision 13: Live Trading Implementation Findings ✅
Analysis 2026-03-19 (first successful end-to-end paper trading run).

**Key findings from implementation and testing:**

**IBKR secdef/search endpoint:**
- Returns `description` and `companyHeader` fields containing "ASX", not the
  `sections` array as initially assumed
- ASX filter in `ibkr_get_conids()` updated to check `description` and
  `companyHeader` fields

**MACD-V strategy function:**
- Backtesting function name: `strat_macdv_dynamic_strength()` (not
  `generate_macd_vol_dynamic_signals()` as originally assumed)
- Takes a full xts object with OHLCV columns in quantmod format, not a numeric
  close price vector
- Parameters: `roll_window` and `strength_quantile` (not `rolling_window` and
  `quantile_thresh`)
- Returns a tibble with `trade_signal` column: 1L = buy, -1L = sell, 0L = hold
- `Ad()` inside the function requires a `.Adjusted` column — added by duplicating
  the close column in `price_df_to_xts()`

**Capital bucket initialisation:**
- On first run (no state file), bucket sizes are derived from IBKR total cash
  balance via `ibkr_get_summary()$totalcashvalue$amount`, not from hardcoded
  `total_capital` in `quant_vars.R`
- Falls back to `total_capital` if IBKR cash fetch fails
- This ensures paper trading uses $1M and live trading uses actual funded balance

**Paper trading account:**
- Paper trading account has no 2FA — TOTP secret not required
- `get_secret_optional()` handles missing `IBKR_TOTP_SECRET` gracefully
- Paper trading credentials stored separately in Secret Manager from live credentials
- Paper trading account ID: `DUP291720`

**Confirmed working conids (as of 2026-03-19):**
| Symbol  | conid     |
|---------|-----------|
| VGS.AX  | 174162945 |
| VAS.AX  | 60009472  |
| GOLD.AX | 45127612  |

**Testing sequence agreed:**
1. Paper trading ($1M simulated) — ongoing, minimum 1 month
2. Live account with $1.00 — confirm real API plumbing works
3. Live account properly funded — go live

**First automated cron run: 2026-03-20 10:15:01 AEDT** ✅
- Authentication completed in ~1 minute
- All 3 ETFs processed correctly
- VGS position held correctly, no duplicate buy
- Total R script runtime: ~11 seconds

### Decision 14: Output File Sync — rclone + Google Drive ✅
Analysis 2026-03-20.

**Problem:** Output files (`state.rds`, `trade_log.csv`, daily logs) live only on the
GCP VM filesystem. Accessing them requires SSH. Also a data loss risk if the VM is
deleted or corrupted.

**Decision:** Use rclone to sync `outputs/live_trading/` to Google Drive after each
daily run, triggered at the end of `start.sh`.

**rclone configuration:**
- Scope: `drive.file` — rclone can only see files it creates, not the entire Drive
- Root folder: `live_trading` folder in Google Drive
  (ID: `1AU604hfCKmlS21C1UaB1v8_bQ2TVDU_S`)
- Config stored at: `~/.config/rclone/rclone.conf` on VM
- Sync command: `rclone sync ~/quant_trading/outputs/live_trading/ gdrive: --log-level INFO`
- Runs at end of each `start.sh` execution after `quant_trader.R` completes

**Why Google Drive (not GCP Storage bucket):**
- Google Drive already syncs to local Windows machine automatically
- Shiny dashboard (planned) will read files from local Google Drive folder
- No additional GCP costs
- `drive.file` scope ensures rclone cannot access any other Drive content

**Trade log format:** CSV (not .rds) — append-friendly via `write_csv(..., append = TRUE)`,
human-readable, openable in Excel. `.rds` would require read-modify-write on each trade.

### Decision 15: sactyr1984-api Security Configuration ✅
Analysis 2026-03-21.

**Username:** `sactyr1984-api` — dedicated IBKR API username on account `U24737217`

**Permissions enabled (minimum required for automated trading):**
- Password — mandatory, cannot disable
- Security Device — mandatory, required for TOTP login
- TWS — required for API order placement
- Market Data — required for price data
- Paper Trading — required for paper trading testing
- Subscriber Status — mandatory
- IP Restrictions — enabled, restricted to `104.154.56.220` (GCP VM static IP)

**All other permissions disabled:**
- All reporting (statements, flex queries, trade confirmations, flex web service)
- All funding (deposits, withdrawals, internal transfers, recurring transactions)
- All account management (profile, financial info, account configuration)
- Position transfers, bank information, transaction history
- PortfolioAnalyst, tax optimizer, tax forms
- Trading Permissions (cannot self-grant new permissions)
- Client management (CAM)

**Note:** `sactyr1984-api` is a secondary username on the same underlying account
(`U24737217`) — it shares the same cash and positions as the primary username.
No funds transfer between usernames is needed or possible.

**TOTP setup:**
- Authenticator app: 2FAS
- TOTP secret stored in GCP Secret Manager as `IBKR_TOTP_SECRET`
- Currently unused (paper trading account has no 2FA)
- Will be activated when switching Secret Manager credentials to `sactyr1984-api`
  for live trading

**Static IP reservation:**
- GCP VM static external IP: `104.154.56.220`
- Reserved via GCP Console → VPC Network → IP Addresses
- Attached to `quant-trader` VM instance

---

## Open Decisions

- [ ] What to name the standalone IBKR Client Portal REST R package?

---

## Technical Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Data source (backtesting) | Yahoo Finance | Free, covers all chosen ETFs |
| Data source (live trading) | IBKR Client Portal API | Single source of truth for live signal generation and order sizing |
| Execution broker | Interactive Brokers (IBKR Pro) | Low fees (0.08%), API available, ASIC regulated |
| IBKR API approach | Client Portal REST API (Option C) | Lightweight, cloud-friendly. See Decision 10. |
| Headless auth | `ib-gateway-service` + TOTP | Fully automated 2FA, no phone required. See Decision 12. |
| Cloud platform | GCP e2-micro (Always Free) | $0/month. See Decision 12. |
| Scheduling | cron job on VM (`15 10 * * 1-5`) | Simpler than Cloud Scheduler for a single VM |
| VM timezone | Australia/Sydney | Ensures cron runs at correct AEST/AEDT time with DST handling |
| VM static IP | `104.154.56.220` | Required for IBKR IP restriction on API username |
| Credentials storage | GCP Secret Manager | Encrypted at rest, never in plain text files |
| Secret access | REST API (not gcloud CLI) | gcloud CLI has OAuth scope bug on e2-micro |
| Capital initialisation | IBKR cash balance on first run | Reflects actual account balance, not hardcoded value |
| Output file sync | rclone → Google Drive (drive.file scope) | Auto-syncs to local machine, feeds Shiny dashboard. See Decision 14. |
| Trade log format | CSV | Append-friendly, human-readable, openable in Excel |
| Language | R | Existing codebase and expertise |
| Parallelism | `furrr` for signal generation, `pmap` for MC backtesting loop | MC loop hits memory limits with `future_pmap` |
| Function naming | `get_market_data` (not `get_etf_data`) | Instrument-agnostic |
| Price column | `Ad()` adjusted close | Accounts for ETF distributions |
| NA handling | `na.approx()` | Linear interpolation, better than dropping rows |
| `get_market_data` default `to` | `Sys.Date() - 2` | Avoids Yahoo Finance returning intraday data |
| File naming | No dates in filenames | Single canonical run; `etf_symbols` drives all naming |
| ETF symbols driver | `etf_symbols` vector | Drives naming, loading, and ordering throughout pipeline |

---

## Repository
- **URL:** https://github.com/sactyr/quant_trading
- **Local path:** `G:/My Drive/Shares and Crypto/quant_trading`
- **Key files:**
  - `R/backtesting/quant_backtesting_vars.R` — parameters and `etf_symbols`
  - `R/backtesting/quant_backtesting_functions.R` — all strategy and helper functions
  - `R/backtesting/quant_backtesting.R` — main backtesting script
  - `outputs/backtesting/` — saved Monte Carlo results (.rds files)
  - `R/live_trading/ibkr_api.R` — IBKR Client Portal REST wrapper (proto-package)
  - `R/live_trading/quant_vars.R` — live trading parameters
  - `R/live_trading/quant_functions.R` — live trading logic
  - `R/live_trading/quant_trader.R` — main daily entry point
  - `outputs/live_trading/` — trade logs, state, position snapshots (synced to Google Drive)
  - `outputs/live_trading/logs/` — daily run logs and cron log
  - `~/ib-gateway/start.sh` — VM startup script (not in repo — contains GCP project ID)
  - `tests/` — unit tests for backtesting and live trading
