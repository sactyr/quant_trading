# Assumptions:
# Only enter and exit long positions. Short positions (leveraged positions) 
# are ignored here.
# Risk exposure: Losses are limited to investment amount, never leveraged.


# 01 LOAD LIBS ------------------------------------------------------------

required_packages <- c(
  "quantmod"
  ,"TTR"
  ,"dplyr"
  ,"tidyr"
  ,"tibble"
  ,"purrr"
  ,"stringr"
  ,"xts"
  ,"PerformanceAnalytics"
  ,"lubridate"
  ,"furrr"
  ,"ggplot2"
)

# Load the packages and suppress startup messages
suppressPackageStartupMessages({
  purrr::walk(required_packages, library, character.only = TRUE)
})

# Load all packages except furrr, lubridate and ggplot2 for parallel worker function
worker_packages <- required_packages[!required_packages %in% c("furrr", "lubridate", "ggplot2")]


# 02 LOAD DEPENDABLES -----------------------------------------------------

quant_dir <- Sys.getenv("QUANT_TRADING_FOLDER")

# furrr options
plan(multisession, workers = availableCores() - 1)
options(future.globals.maxSize = +Inf)
options(future.packages = worker_packages) # Only load worker packages for workers

source(file.path(quant_dir, "R", "backtesting", "quant_backtesting_vars.R"))
source(file.path(quant_dir, "R", "backtesting", "quant_backtesting_functions.R"))


# 03 GET TRADING DATA -----------------------------------------------------

# Fetch all available history for each ETF
etf_data <- map(
  .x = etf_symbols
  ,~ get_market_data(symbol = .x)
) %>% 
  set_names(etf_symbols)

# Linear interpolation of data where there were NAs (data unavailable due to 
# public holidays, stock halt trading etc)
etf_data <- map(etf_data, na.approx)


# 04 BACKTESTING SINGLE WINDOW --------------------------------------------

strategy_fns <- list(
  buy_hold                   = strat_buy_hold
  ,sma                       = strat_sma
  ,sma_cross                 = strat_sma_cross
  ,rsi                       = strat_rsi
  ,bb                        = strat_bb
  ,macd                      = strat_macd
  ,macd_vol                  = strat_macdv
  ,macd_vol_dynamic_strength = strat_macdv_dynamic_strength
)

# Run single-window backtest for each ETF
single_window_results <- map(etf_symbols, function(symbol) {
  
  message("Single window backtest: ", symbol)
  
  prices_xts <- etf_data[[symbol]]
  
  # Generate all signals
  strategies <- future_map(strategy_fns, ~.x(prices_xts))
  
  # Create combinations of strategy name + stop-loss options
  param_grid <- expand_grid(
    name       = names(strategies)
    ,stop_loss = stop_losses
  )
  
  # Simulate trades and equity progression
  results_list <- future_pmap(param_grid, function(name, stop_loss) {
    
    res <- backtest_strategy(
      signal_tbl       = strategies[[name]]
      ,ibkr_min_fee    = ibkr_min_fee
      ,ibkr_fee_rate   = ibkr_fee_rate
      ,initial_equity  = init_equity
      ,stop_loss       = stop_loss
      ,min_trade_value = 10
      ,force_close_final = TRUE      
    )
    
    res$strategy_nm <- name
    
    return(res)
  })
  
  # Name each result meaningfully for clarity
  names(results_list) <- paste0(param_grid$name, "_SL", param_grid$stop_loss * 100)
  
  # Summarise performance metrics
  summary_metrics <- get_performance_metrics(results_list, initial_equity = init_equity)
  
  summary_metrics
  
}) %>% 
  set_names(etf_symbols)

# Show sorted results per ETF
walk(etf_symbols, function(symbol) {
  message("\n--- ", symbol, " ---")
  single_window_results[[symbol]] %>% 
    arrange(desc(total_return)) %>% 
    print(n = 40)
})


# 05 MONTE CARLO SIMULATION -----------------------------------------------

## Monte Carlo parameters -------------------------------------------------

strength_threshold_grid  <- seq(from = 20, to = 80, by = 10) # 50 is default for strat_macdv
rolling_window_grid      <- seq(from = 10, to = 50, by = 10) # 20 is default
strength_quantile_grid   <- seq(from = 0.5, to = 0.9, by = 0.1) # 0.8 is default

macd_vol_dynamic_params <- expand_grid(
  rolling_window     = rolling_window_grid
  ,strength_quantile = strength_quantile_grid
)

set.seed(42)

## Run Monte Carlo per ETF ------------------------------------------------

all_etf_monte_carlo <- map(etf_symbols, function(symbol) {
  
  message("\n========== Monte Carlo: ", symbol, " ==========")
  
  prices_xts <- etf_data[[symbol]]
  
  ### Generate sample windows ---------------------------------------------
  
  sample_windows <- sample_xts_window(
    prices_xts         = prices_xts
    ,sample_size       = n_samples
    ,min_window_length = min_window_length
  )
  
  # Create window metadata lookup table
  window_metadata <- tibble(
    window_id          = names(sample_windows$sampled_windows)
    ,window_length     = sample_windows$window_lengths
    ,window_start_date = sample_windows$start_dates
    ,window_end_date   = sample_windows$end_dates
  )
  
  ### Generate trade signals -----------------------------------------------
  
  monte_carlo_signals <- future_map(
    .x = seq_along(sample_windows$sampled_windows)
    ,function(sample_index) {
      
      win_xts <- sample_windows$sampled_windows[[sample_index]]
      
      message(
        "Window ", sample_index, "/", length(sample_windows$sampled_windows)
        ," | Length: ", sample_windows$window_lengths[sample_index]
        ," | Dates: ", sample_windows$start_dates[sample_index]
        ," to ", sample_windows$end_dates[sample_index]
      )
      
      # Base strategies (default parameters)
      base_strategies <- list(
        buy_hold   = strat_buy_hold(win_xts)
        ,sma       = strat_sma(win_xts)
        ,sma_cross = strat_sma_cross(win_xts)
        ,rsi       = strat_rsi(win_xts)
        ,bb        = strat_bb(win_xts)
        ,macd      = strat_macd(win_xts)
      )
      
      # MACD-V with varying fixed strength thresholds
      macd_vol_fixed <- map(
        .x = strength_threshold_grid
        ,~ strat_macdv(
          prices_xts          = win_xts
          ,strength_threshold = .x
        )
      ) %>% 
        set_names(paste0("macd_vol_fixed_", strength_threshold_grid))
      
      # MACD-V with dynamic strength (parameter grid)
      macd_vol_dynamic <- pmap(
        .l = macd_vol_dynamic_params
        ,function(rolling_window, strength_quantile) {
          strat_macdv_dynamic_strength(
            prices_xts         = win_xts
            ,roll_window       = rolling_window
            ,strength_quantile = strength_quantile
          )
        }
      ) %>% 
        set_names(
          paste(
            "macd_vol_dynamic"
            ,macd_vol_dynamic_params$rolling_window
            ,macd_vol_dynamic_params$strength_quantile
            ,sep = "_"
          )
        )
      
      # Combine all strategies
      c(base_strategies, macd_vol_fixed, macd_vol_dynamic)
    }
    ,.progress = TRUE
    ,.options = furrr_options(seed = TRUE)
  ) %>% 
    set_names(names(sample_windows$sampled_windows))
  
  # Get all unique strategy names
  all_strategy_names <- names(monte_carlo_signals[[1]])
  
  ### Run Monte Carlo backtesting in chunks by Stop Loss ------------------
  
  all_monte_carlo_summaries <- list()
  
  message("Total samples (windows): ", length(sample_windows$sampled_windows))
  
  for (sl in stop_losses) {
    
    # Skip if this stop-loss chunk was already saved (resume support)
    chunk_file <- file.path(
      quant_dir
      ,"outputs"
      ,"backtesting"
      ,paste0(symbol, "_SL", sl * 100, "_monte_carlo_summary.rds")
    )
    
    if (file.exists(chunk_file)) {
      message("Skipping SL ", sl * 100, "% for ", symbol, " - already saved, loading from disk")
      all_monte_carlo_summaries[[paste0("SL", sl * 100)]] <- readRDS(chunk_file)
      next
    }
    
    message("Starting Monte Carlo for Stop Loss: ", sl * 100, "%")
    
    #### In loop setup -----------------------------------------------------
    mc_param_grid_sl <- expand_grid(
      window_id      = names(sample_windows$sampled_windows)
      ,strategy_name = all_strategy_names
      ,stop_loss     = sl
    ) %>% 
      filter(!(strategy_name == "buy_hold" & sl > 0)) %>%
      unite(
        col       = "backtest_id"
        ,window_id, strategy_name, stop_loss
        ,sep      = "_SL"
        ,remove   = FALSE
      )
    
    message("Total backtests to run in this chunk: ", nrow(mc_param_grid_sl))
    
    #### Backtesting -------------------------------------------------------
    # NOTE: Using pmap (not future_pmap) to avoid serialising the large
    # monte_carlo_signals object (~4GB) to each parallel worker, which causes
    # memory errors. backtest_strategy is fast enough that single-threaded
    # execution is acceptable.
    monte_carlo_results_sl <- pmap(
      .l = mc_param_grid_sl
      ,function(window_id, strategy_name, stop_loss, backtest_id) {
        
        res <- backtest_strategy(
          signal_tbl         = monte_carlo_signals[[window_id]][[strategy_name]]
          ,ibkr_min_fee      = ibkr_min_fee
          ,ibkr_fee_rate     = ibkr_fee_rate
          ,initial_equity    = init_equity
          ,stop_loss         = stop_loss
          ,min_trade_value   = 10
          ,force_close_final = TRUE
        )
        
        # Add metadata
        window_index <- as.integer(str_extract(window_id, "\\d+"))
        
        res$strategy_nm       <- strategy_name
        res$window_id         <- window_id
        res$window_length     <- sample_windows$window_lengths[window_index]
        res$window_start_date <- sample_windows$start_dates[window_index]
        res$window_end_date   <- sample_windows$end_dates[window_index]
        
        return(res)
      }
    ) %>% 
      set_names(nm = mc_param_grid_sl$backtest_id)
    
    #### Summarise performance metrics -------------------------------------
    monte_carlo_summary_sl <- get_performance_metrics(
      monte_carlo_results_sl
      ,initial_equity = init_equity
    )
    
    #### Add window metadata -----------------------------------------------
    monte_carlo_summary_sl <- monte_carlo_summary_sl %>%
      left_join(
        mc_param_grid_sl %>% select(backtest_id, window_id)
        ,by = c("strategy" = "backtest_id")
      ) %>% 
      left_join(
        window_metadata
        ,by = "window_id"
      )
    
    #### Save chunk to disk ------------------------------------------------
    saveRDS(
      object = monte_carlo_summary_sl
      ,file  = chunk_file
    )
    message("Saved chunk: SL", sl * 100, "% for ", symbol)
    
    #### Store results in memory -------------------------------------------
    all_monte_carlo_summaries[[paste0("SL", sl * 100)]] <- monte_carlo_summary_sl
    
    #### Memory management -------------------------------------------------
    rm(monte_carlo_results_sl)
    gc(verbose = FALSE) 
  }
  
  ### Combine stop-loss chunks --------------------------------------------
  bind_rows(all_monte_carlo_summaries)
  
}) %>% 
  set_names(etf_symbols)


## Clean up ---------------------------------------------------------------

plan(sequential) # Explicitly close multisession workers by switching plan


## Save combined results --------------------------------------------------

walk(etf_symbols, function(symbol) {
  saveRDS(
    object = all_etf_monte_carlo[[symbol]]
    ,file  = file.path(
      quant_dir
      ,"outputs"
      ,"backtesting"
      ,paste0(symbol, "_monte_carlo_summary.rds")
    )
  )
})


## Load saved results -----------------------------------------------------

backtesting_files <- list.files(
  path       = file.path(quant_dir, "outputs", "backtesting")
  ,pattern   = "_monte_carlo_summary\\.rds$"
  ,full.names = TRUE
)

# Combined files have no _SL in the name
backtesting_combined_files <- backtesting_files[!grepl("_SL[0-9]", backtesting_files)]

# etf_symbols drives the names - load in etf_symbols order
all_etf_monte_carlo <- map(etf_symbols, function(symbol) {
  
  target_file <- backtesting_combined_files[grepl(symbol, backtesting_combined_files)]
  
  if (length(target_file) == 0) {
    warning("No combined file found for ", symbol, " - skipping")
    return(NULL)
  }
  
  readRDS(target_file)
  
}) %>% 
  set_names(etf_symbols) %>% 
  compact() # Drop any NULLs (symbols with no combined file)


## Aggregate results ------------------------------------------------------

agg_summaries <- map(names(all_etf_monte_carlo), function(symbol) {
  
  monte_carlo_summary <- all_etf_monte_carlo[[symbol]]
  
  agg_summary <- monte_carlo_summary %>%
    mutate(CAGR = ifelse(num_trades == 0, 0, CAGR)) %>%
    group_by(strategy_type, stop_loss) %>%
    summarise(
      # METRIC 1: Geometric Mean of Return / Expected log-returns
      # Formula: exp(mean(ln(1 + R))) - 1
      geo_mean_CAGR = exp(mean(log(1 + pmax(CAGR, -0.99)), na.rm = TRUE)) - 1
      
      # METRIC 2: Stability (Percent of windows with Positive Return)
      ,win_rate_windows = mean(total_return > 0, na.rm = TRUE)
      
      # METRIC 3: Risk-Adjusted Stability (Mean Sharpe is noisy, Median is robust)
      ,median_Sharpe = median(sharpe_ratio, na.rm = TRUE)
      
      # METRIC 4: Tail Risk (The average of the worst 5% of outcomes)
      ,cvar_drawdown = mean(sort(max_drawdown, decreasing = TRUE)[1:ceiling(n() * 0.05)], na.rm = TRUE)
      
      # Context
      ,n_samples = n()
      ,.groups = "drop"
    )
  
  ranked_summary <- agg_summary %>%
    mutate(
      # Robust Calmar: Return per unit of extreme tail risk
      robust_calmar = geo_mean_CAGR / cvar_drawdown
      
      # Probability Score: reliability (win rate) × quality (median Sharpe)
      ,prob_score = win_rate_windows * median_Sharpe
      
      # CAPS (Calmar-adjusted Probability Score): composite ranking metric
      # - penalises high-risk high-return strategies (e.g. buy & hold)
      # - rewards strategies that manage downside and maintain consistency
      ,CAPS = robust_calmar * prob_score
    ) %>%
    arrange(desc(CAPS))
  
  ranked_summary
  
}) %>% 
  set_names(names(all_etf_monte_carlo))


agg_summaries %>% 
  map(slice_head, n = 1) %>% 
  bind_rows(.id = "etf") 

## Visualisation ----------------------------------------------------------

### The Efficient Frontier (one plot per ETF) -----------------------------

walk(names(agg_summaries), function(symbol) {
  
  ranked_summary <- agg_summaries[[symbol]]
  
  p <- ggplot(
    data     = ranked_summary
    ,mapping = aes(x = cvar_drawdown, y = geo_mean_CAGR)
  ) +
    
    # Plot all points in grey
    geom_point(
      mapping = aes(size = CAPS)
      ,color  = "grey"
      ,alpha  = 0.5
    ) +
    
    # Highlight Top 5 by CAPS Score
    geom_point(
      data     = head(ranked_summary, 5)
      ,mapping = aes(color = strategy_type, size = CAPS)
      ,alpha   = 0.5
    ) +
    geom_text(
      data     = head(ranked_summary, 5)
      ,mapping = aes(label = paste(strategy_type, stop_loss, sep = "_"))
      ,vjust   = -1.5
      ,size    = 3
    ) +
    
    labs(
      title     = paste("The Efficient Frontier:", symbol)
      ,subtitle = "Top 5 CAPS highlighted"
      ,x        = "Tail Risk (CVaR Drawdown)"
      ,y        = "Geometric Mean CAGR"
    ) +
    theme_minimal()
  
  print(p)
  
})