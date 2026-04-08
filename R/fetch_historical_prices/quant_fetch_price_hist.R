# quant_fetch_price_hist.R
# Fetches and maintains cumulative daily price history for all ETFs
#
# Runs daily at 9:45 AM AEST (before ASX open) via cron:
#   45 9 * * 1-5 bash /home/sactyr/ib-gateway/fetch_prices.sh >> /home/sactyr/quant_trading/outputs/live_trading/logs/cron.log 2>&1
#
# For each ETF:
#   - Checks if today is a trading day — exits early if not (public holiday)
#   - Fetches 1yr of daily OHLCV bars from IBKR
#   - Computes MD5 hash per row for deduplication
#   - Merges new rows into cumulative history file
#   - Validates no date gaps in merged dataset
#   - Saves updated history to outputs/live_trading/prices/{SYMBOL}.rds
#
# On first run (no history file exists):
#   - Saves full 1yr history as baseline
#
# Price history files are synced to Google Drive by fetch_prices.sh after
# each run. The dashboard reads from the Google Drive synced copy.

suppressPackageStartupMessages({
  library(here)
  library(logger)
  library(dplyr)
  library(lubridate)
  library(openssl)
  library(httr2)
})

Sys.setenv(TZ = "Australia/Sydney")


# Source shared config and IBKR API wrapper -------------------------------

source(here("R", "quant_vars.R"))
source(here("R", "live_trading", "ibkr_api.R"))


# Logging setup -----------------------------------------------------------

dir.create(price_fetch_log_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(
  price_fetch_log_dir,
  sprintf("quant_fetch_price_hist_%s.log", Sys.Date())
)

log_appender(appender_tee(log_file))
log_layout(layout_glue_generator(
  format = '[{format(time, "%Y-%m-%d %H:%M:%S")}] | {toupper(sprintf("%-7s", level))} | {msg}'
))


# Helper functions --------------------------------------------------------

#' Compute MD5 hash for a price history row
#'
#' Concatenates date+OHLC fields (excluding volume and dttm_updated)into a 
#' single string and computes its MD5 hash. Volume is excluded because
#' IBKR occasionally revises volume slightly between fetches for the same bar.
#' Used for deduplication when merging new rows into the cumulative history.
#'
#' @param df Data frame with columns: date, open, high, low, close
#' @return Character vector of MD5 hashes, one per row
compute_md5 <- function(df) {
  concat <- paste(
    df$date, df$open, df$high, df$low, df$close,
    sep = "|"
  )
  as.character(openssl::md5(concat))
}

#' Merge new price bars into cumulative history
#'
#' Fetches latest 1yr of OHLCV data from IBKR, identifies new rows via MD5
#' hash comparison, appends them to the base history, and validates there are
#' no missing dates in the merged dataset.
#'
#' @param symbol ETF symbol including .AX suffix (e.g. "VGS.AX")
#' @param conid IBKR contract ID for the symbol
#' @param base_df Existing price history data frame, or NULL on first run
#' @param non_trading_dates Character vector of known non-trading dates in
#'   "YYYY-MM-DD" format, used to suppress false gap warnings
#' @return Updated data frame with new rows merged, or NULL if no new rows
merge_price_history <- function(symbol, conid, base_df, non_trading_dates = character(0)) {

  # Fetch latest 1yr from IBKR
  log_info("Fetching price history from IBKR for {symbol}...")
  delta_df <- ibkr_get_price_history(conid, period = "1y")

  # Add metadata columns
  delta_df <- delta_df |>
    mutate(
      dttm_updated = Sys.time(),
      md5_hash     = compute_md5(delta_df)
    )

  # First run — no base history exists
  if (is.null(base_df)) {
    log_info("{symbol}: No existing history — saving {nrow(delta_df)} bars as baseline.")
    return(delta_df)
  }

  # Find new rows by MD5 hash comparison
  new_hashes <- setdiff(delta_df$md5_hash, base_df$md5_hash)

  if (length(new_hashes) == 0) {
    log_info("{symbol}: No new rows to add.")
    return(NULL)
  }

  log_info("{symbol}: Found {length(new_hashes)} new row(s) to append.")

  new_rows  <- delta_df |> filter(md5_hash %in% new_hashes)
  merged_df <- bind_rows(base_df, new_rows) |> arrange(date)

  # Validate no missing trading dates
  # Generate expected weekdays (Mon-Fri) then exclude known non-trading dates
  all_dates     <- seq.Date(min(merged_df$date), max(merged_df$date), by = "day")
  weekdays_only <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]
  trading_days  <- weekdays_only[!as.character(weekdays_only) %in% non_trading_dates]
  missing_dates <- setdiff(as.character(trading_days), as.character(merged_df$date))

  if (length(missing_dates) > 0) {
    log_warn("{symbol}: {length(missing_dates)} missing trading date(s) detected.")
    log_warn("{symbol}: Missing: {paste(head(missing_dates, 5), collapse = ', ')}")
  }

  log_info("{symbol}: Merge complete — {nrow(merged_df)} total bars.")
  merged_df
}


# Main --------------------------------------------------------------------

log_info("=============================================================")
log_info("quant_fetch_price_hist.R started — {Sys.time()}")
log_info("ETF universe: {paste(etf_symbols, collapse = ', ')}")
log_info("=============================================================")

## Step 1: Authenticate with IBKR -----------------------------------------

log_info("Step 1: Confirming IBKR session...")

tryCatch({
  ibkr_tickle()
  log_info("IBKR session confirmed.")
}, error = function(e) {
  log_error("IBKR session check failed: {e$message}")
  stop(e)
})

## Step 2: Check if today is a trading day ---------------------------------

log_info("Step 2: Checking if today is a trading day...")

non_trading_dates <- tryCatch({
  dates <- ibkr_get_non_trading_dates()
  log_info("Fetched {length(dates)} upcoming non-trading date(s) from IBKR.")
  dates
}, error = function(e) {
  log_warn("Could not fetch trading schedule: {e$message}. Proceeding without holiday check.")
  character(0)
})

today_str <- format(Sys.Date(), "%Y-%m-%d")

if (today_str %in% non_trading_dates) {
  log_info("Today ({today_str}) is a non-trading day. No prices to fetch.")
  log_success("quant_fetch_price_hist.R completed — {Sys.time()}")
  quit(save = "no", status = 0)
}

if (weekdays(Sys.Date()) %in% c("Saturday", "Sunday")) {
  log_info("Today ({today_str}) is a weekend. No prices to fetch.")
  log_success("quant_fetch_price_hist.R completed — {Sys.time()}")
  quit(save = "no", status = 0)
}

log_info("Today ({today_str}) is a trading day. Proceeding.")

## Step 3: Resolve conids -------------------------------------------------

log_info("Step 3: Resolving conids for {paste(etf_symbols_ibkr, collapse = ', ')}...")

conids <- tryCatch({
  ids <- ibkr_get_conids(etf_symbols_ibkr)
  log_info("Conids resolved: {paste(names(ids), ids, sep = '=', collapse = ', ')}")
  ids
}, error = function(e) {
  log_error("Failed to resolve conids: {e$message}")
  stop(e)
})

## Step 4: Fetch and merge price history per ETF --------------------------

log_info("Step 4: Fetching and merging price history...")

dir.create(prices_dir, showWarnings = FALSE, recursive = TRUE)

for (symbol in etf_symbols) {

  tryCatch({

    ibkr_sym  <- sub("\\.AX$", "", symbol)
    conid     <- conids[ibkr_sym]
    hist_file <- file.path(prices_dir, paste0(symbol, ".rds"))

    # Load existing history if available
    base_df <- if (file.exists(hist_file)) {
      log_info("{symbol}: Loading existing history from {hist_file}")
      existing <- readRDS(hist_file)
      log_info("{symbol}: Existing history has {nrow(existing)} bars ({min(existing$date)} to {max(existing$date)})")
      existing
    } else {
      log_info("{symbol}: No existing history file found — will create baseline.")
      NULL
    }

    # Merge new rows — pass non_trading_dates for accurate gap validation
    updated_df <- merge_price_history(symbol, conid, base_df, non_trading_dates)

    # Save if updated
    if (!is.null(updated_df)) {
      saveRDS(updated_df, hist_file)
      log_info("{symbol}: Saved {nrow(updated_df)} bars to {hist_file}")
    } else {
      log_info("{symbol}: No update needed.")
    }

  }, error = function(e) {
    log_error("{symbol}: Failed — {e$message}")
    # Continue to next symbol rather than stopping entire run
  })
}

## Step 5: Summary --------------------------------------------------------

log_info("Step 5: Summary")

for (symbol in etf_symbols) {
  hist_file <- file.path(prices_dir, paste0(symbol, ".rds"))
  if (file.exists(hist_file)) {
    df <- readRDS(hist_file)
    log_info("  {symbol}: {nrow(df)} bars | {min(df$date)} to {max(df$date)} | latest close ${tail(df$close, 1)}")
  } else {
    log_warn("  {symbol}: No history file found.")
  }
}

log_info("=============================================================")
log_success("quant_fetch_price_hist.R completed — {Sys.time()}")
log_info("=============================================================")
