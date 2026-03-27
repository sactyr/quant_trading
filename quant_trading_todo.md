# Quant Trading - Project To-Do List

## 1. Backtesting
- [x] Add VAS.AX and VGS.AX to `etf_symbols` in `quant_backtesting_vars.R`
- [x] Run full 1000-sample Monte Carlo for all ETFs (STW, GOLD, SLF, IOO, VAS, VGS, IEM, IAF)
- [x] Run full 1000-sample Monte Carlo for additional candidates (VAE, VEU)
- [x] Review and interpret full results across all 10 ETFs
- [x] Drop SLF.AX — geo_mean_CAGR 3.23%, CAPS 0.080, eroding diversification (Decision 4)
- [x] Drop IEM.AX — geo_mean_CAGR 3.80%, CAPS 0.034 worst in universe (Decision 5)
- [x] Drop IAF.AX — geo_mean_CAGR 1.81%, below 5.5% threshold (Decision 6)
- [x] Drop IOO.AX, reinstate VGS.AX — VGS superior on CAPS, CAGR, and MER (Decision 7)
- [x] Drop STW.AX in favour of VAS.AX — VAS broader, cheaper, better performance (Decision 8)
- [x] Drop VEU.AX — overlap with VGS and VAS, hidden tax drag ~0.29%/year
- [x] Drop VAE.AX — best active strategy CAGR 3.66%, below 5.5% threshold
- [x] Finalise ETF universe: VGS.AX, VAS.AX, GOLD.AX
- [x] Finalise capital split: 56% VGS / 30% VAS / 14% GOLD (CAPS-weighted, Decision 9)
- [x] Clean rerun of Monte Carlo with no dates in filenames, etf_symbols as driver
- [x] Update `quant_backtesting_vars.R` — set `etf_symbols` to final three (VGS, VAS, GOLD)
- [x] Commit all backtesting files to repo

## 2. IBKR Account
- [x] Open IBKR Pro account at ibkr.com.au (individual account, ASIC regulated)
- [x] Complete identity verification (passport/driver's licence + proof of address)
- [x] Account approved and accessible
- [x] Fund live account with $1.00 (for initial API testing)
- [x] Create dedicated IBKR API username (`sactyr1984-api`)
- [x] Username approved
- [x] Tighten permissions on `sactyr1984-api` — reporting, funding, account settings
  all disabled; TWS, market data, paper trading, IP restrictions kept
- [x] Reserve static IP `104.154.56.220` for GCP VM
- [x] Add IP restriction on `sactyr1984-api` — only `104.154.56.220` can log in
- [x] Configure IBKR Mobile Authenticator (TOTP) on `sactyr1984-api` via 2FAS
- [x] Extract TOTP secret and add to GCP Secret Manager (`IBKR_TOTP_SECRET`)
- [x] Set up paper trading account — credentials confirmed working
- [ ] Paper trade for minimum 1 month before switching to live credentials
- [ ] Fund live account properly when ready to go live

## 3. Live Trading Scripts
- [x] Agree on repo folder structure (Decision 11, 2026-03-13)
- [x] Create `R/live_trading/ibkr_api.R` — IBKR Client Portal REST wrapper
- [x] Create `R/live_trading/quant_vars.R` — live trading configuration
- [x] Create `R/live_trading/quant_functions.R` — signal generation, order sizing, state management
- [x] Create `R/live_trading/quant_trader.R` — main daily entry point
- [x] Fix conid ASX filter (description/companyHeader fields)
- [x] Fix MACD-V signal generation (correct function name, xts object, parameter names)
- [x] Fix order confirmation handler (atomic vector response)
- [x] Fix Ad() column not found (add .Adjusted column to xts object)
- [x] Initialise state buckets from IBKR cash balance on first run
- [x] Suppress package startup messages, set AEST timezone
- [x] Commit all live trading scripts to repo
- [x] Test `quant_trader.R` end-to-end on paper trading account ✅
- [x] First automated cron run confirmed successful (2026-03-20) ✅

## 4. GCP Infrastructure
- [x] Create GCP project (`quant-trading-490603`)
- [x] Provision e2-micro VM (us-central1-a, Ubuntu 24.04, 30GB standard disk)
- [x] Enable GCP Secret Manager API
- [x] Store credentials in Secret Manager (IBKR username, password, account ID)
- [x] Store TOTP secret in Secret Manager (`IBKR_TOTP_SECRET`) — ready for live switch
- [x] Grant VM service account Secret Manager Secret Accessor role
- [x] Install Docker on VM
- [x] Deploy `ib-gateway-service` Docker container
- [x] Create `start.sh` — fetches credentials, starts gateway, authenticates, runs trader
- [x] Install R 4.5.3 and all required packages on VM
- [x] Install system library dependencies on VM
- [x] Clone `quant_trading` repo onto VM
- [x] Set VM timezone to Australia/Sydney
- [x] Reserve static external IP `104.154.56.220` for VM
- [x] Set up cron job: `15 10 * * 1-5` — runs `start.sh` at 10:15 AM AEDT weekdays
- [x] Install rclone and configure with Google Drive (drive.file scope, live_trading folder)
- [x] Add rclone sync to `start.sh` — syncs outputs to Google Drive after each run
- [x] Test headless authentication on paper trading account ✅
- [x] Test cron job fires correctly during market hours ✅ (2026-03-20 10:15:01 AEDT)
- [x] Confirmed Google Drive sync working after each run ✅
- [ ] Set up email alerting for failures and trade executions

## 5. Testing & Robustness
- [ ] Paper trade for minimum 1 month before going live
- [ ] Verify orders fill correctly during market hours (check IBKR paper account)
- [ ] Write unit tests for backtesting functions (`tests/test_backtesting.R`)
- [ ] Write unit tests for live trading functions (`tests/test_live_trading.R`)
- [ ] Validate live trading signal generation matches backtesting signals on historical data

## 6. Shiny Dashboard
- [ ] Design dashboard layout (P&L, positions, signals, log viewer)
- [ ] Build `R/dashboard/quant_dashboard.R` — reads from Google Drive synced files
- [ ] P&L tracking: unrealised P&L per position, realised P&L from closed trades,
  total portfolio value
- [ ] Daily log viewer — browse and read log files without SSH
- [ ] Trade history table — filterable view of trade_log.csv
- [ ] Deploy and test locally in RStudio

## 7. Going Live
- [ ] Switch Secret Manager credentials to `sactyr1984-api` + live account ID
- [ ] Fund live account to desired capital amount
- [ ] Run one final paper trading test after credential switch
- [ ] Go live

## 8. Repository Hygiene
- [x] Fix `get_market_data` to = NULL bug (now defaults to Sys.Date() - 2)
- [x] Switch Monte Carlo backtesting loop from future_pmap to pmap
- [x] Fix resume logic to use etf_symbols-driven file naming (no dates)
- [x] Fix visualisation to use names(agg_summaries) not etf_symbols directly
- [x] Update `README.md` with project overview and repo structure
- [x] Commit all files to repo
