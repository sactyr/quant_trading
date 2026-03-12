# 01 HELPER FUNCTIONS -----------------------------------------------------

#' get_market_data
#' 
#' @description
#' Fetches trading data from Yahoo Finance. Wrapper function for
#' quantmod::getSymbols. Fetches all available history for the given symbol
#' (no start date required).
#' 
#' @param symbol Valid symbol string to pass onto quantmod::getSymbols
#' @param to End of trading data to fetch, yyyy-mm-dd format
#'
#' @returns xts-style data frame

get_market_data <- function(
    symbol
    ,to = Sys.Date() - 2
) {
  
  getSymbols(
    Symbols = symbol
    ,src = "yahoo"
    ,to = to
    ,auto.assign = FALSE
  )
  
}


#' backtest_strategy
#' 
#' @description
#' Simulates trading performance by executing buy/sell signals on historical
#' price data. Tracks position entries/exits, calculates transaction fees,
#' applies optional stop-loss risk management, and generates an equity curve
#' showing portfolio value over time.
#' 
#' The function enforces long-only positions (no shorting) and includes
#' realistic constraints like minimum trade values. Stop-loss checks take
#' priority over signal-based exits to ensure risk management is enforced
#' first.
#' 
#' Fees are modelled using IBKR's ASX fee structure:
#' max(ibkr_min_fee, proceeds * ibkr_fee_rate), ensuring that the $6 minimum
#' commission is applied on small trades.
#' 
#' @param signal_tbl tibble containing dates, adjusted close prices and trade
#' signals
#' @param ibkr_min_fee IBKR minimum flat fee per trade, defaults to $6 AUD
#' @param ibkr_fee_rate IBKR percentage fee per trade, defaults to 0.08%
#' @param initial_equity initial cash outlay for trading
#' @param stop_loss stop loss value to apply, defaults to 0 i.e. no stop loss
#' @param min_trade_value minimum cash required to buy
#' @param force_close_final close open positions at end of window
#'
#' @returns A list with trade metadata, trades tibble, equity curve tibble and
#' final equity amount

backtest_strategy <- function(
    signal_tbl
    ,ibkr_min_fee = 6
    ,ibkr_fee_rate = 0.0008
    ,initial_equity = 10000
    ,stop_loss = 0
    ,min_trade_value = 10
    ,force_close_final = TRUE # If a strategy ends with an open position, need to force-close it at the last price for accurate P&L
) {
  
  # 1. Validation & Setup
  stopifnot(
    "signal_tbl must contain required columns" = 
      all(c("date", "price", "trade_signal") %in% names(signal_tbl))
  )
  
  # Extract vectors for speed
  dates   <- as.Date(signal_tbl$date)
  prices  <- as.numeric(signal_tbl$price)
  signals <- tidyr::replace_na(as.integer(signal_tbl$trade_signal), 0L)
  n       <- length(prices)
  
  # 2. Pre-allocate Output Vectors
  equity_curve <- numeric(n)
  equity_curve[1] <- initial_equity
  
  trade_list  <- vector("list", length = n) 
  trade_count <- 0
  
  # 3. State Variables
  cash  <- initial_equity
  units <- 0
  pos   <- 0L        # 0 = flat, 1 = long
  entry <- NA_real_
  
  # 4. The Loop
  for (i in 1:n) {
    
    price_i  <- prices[i]
    date_i   <- dates[i]
    signal_i <- signals[i]
    
    # Mark current equity (Market Value)
    current_equity <- cash + (units * price_i)
    
    # Skip logic if price is missing
    if (is.na(price_i)) {
      equity_curve[i] <- current_equity
      next
    }
    
    action_taken <- NA_character_
    trade_price  <- NA_real_
    trade_fee    <- 0
    trade_units  <- NA_real_
    exit_reason  <- NA_character_
    
    # --- LOGIC START ---
    
    # A. Check Stop Loss (Priority: Risk checks happen before Signals)
    if (pos == 1L && stop_loss > 0) {
      
      if (price_i <= entry * (1 - stop_loss)) {
        
        # Calculate Exit
        proceeds    <- units * price_i
        trade_fee   <- max(ibkr_min_fee, proceeds * ibkr_fee_rate)
        cash        <- cash + proceeds - trade_fee
        trade_units <- units
        units       <- 0
        pos         <- 0L
        entry       <- NA_real_
        
        action_taken <- "SELL"
        exit_reason  <- "STOP_LOSS"
        trade_price  <- price_i
      }
    }
    
    # B. Check Sell Signal (Only if we are still Long)
    if (pos == 1L && signal_i == -1L) {
      
      proceeds    <- units * price_i
      trade_fee   <- max(ibkr_min_fee, proceeds * ibkr_fee_rate)
      cash        <- cash + proceeds - trade_fee
      trade_units <- units
      units       <- 0
      pos         <- 0L
      entry       <- NA_real_
      
      action_taken <- "SELL"
      exit_reason  <- "SIGNAL"
      trade_price  <- price_i
    }
    
    # C. Check Buy Signal (Only if we are flat/holding cash position)
    if (pos == 0L && signal_i == 1L) {
      
      # Check minimum trade value requirement
      if (cash >= min_trade_value) {
        
        # Position sizing: allocate all cash, back-solve for units after fee
        # Fee = max(ibkr_min_fee, cost * ibkr_fee_rate)
        # If ibkr_fee_rate applies: units = cash / (price * (1 + ibkr_fee_rate))
        # If flat fee applies:      units = (cash - ibkr_min_fee) / price
        # We use whichever fee is larger (consistent with IBKR structure)
        
        trade_price <- price_i
        
        cost_if_pct  <- cash / (1 + ibkr_fee_rate) # max spend if % fee binds
        fee_if_pct   <- cost_if_pct * ibkr_fee_rate
        fee_if_flat  <- ibkr_min_fee
        
        trade_fee   <- max(fee_if_flat, fee_if_pct)
        cost        <- cash - trade_fee
        units       <- cost / price_i
        trade_units <- units
        
        cash  <- cash - cost - trade_fee
        pos   <- 1L
        entry <- price_i
        
        action_taken <- "BUY"
        exit_reason  <- NA_character_  # N/A for entries
      }
    }
    
    # --- LOGIC END ---
    
    # Update Equity Curve after potential trades
    current_equity  <- cash + (units * price_i)
    equity_curve[i] <- current_equity
    
    # Record Trade if one happened
    if (!is.na(action_taken)) {
      
      trade_count <- trade_count + 1
      
      trade_list[[trade_count]] <- list(
        date         = date_i
        ,action      = action_taken
        ,exit_reason = exit_reason
        ,price       = trade_price
        ,units       = trade_units
        ,fee         = trade_fee
        ,equity_after = current_equity
      )
      
    }
  }
  
  # 5. Force Close Final Position (if enabled and position is open)
  if (force_close_final && pos == 1L && !is.na(prices[n])) {
    
    final_proceeds <- units * prices[n]
    final_fee      <- max(ibkr_min_fee, final_proceeds * ibkr_fee_rate)
    cash           <- cash + final_proceeds - final_fee
    
    trade_count <- trade_count + 1
    
    trade_list[[trade_count]] <- list(
      date         = dates[n]
      ,action      = "SELL"
      ,exit_reason = "FORCE_CLOSE"
      ,price       = prices[n]
      ,units       = units
      ,fee         = final_fee
      ,equity_after = cash
    )
    
    # Update final equity
    equity_curve[n] <- cash
    
    # Reset position
    units <- 0
    pos   <- 0L
  }
  
  # 6. Final Output Construction
  trades_final <- if (trade_count > 0) {
    bind_rows(trade_list[1:trade_count])
  } else {
    tibble()
  }
  
  list(
    parameters = list(
      stop_loss         = stop_loss
      ,ibkr_min_fee    = ibkr_min_fee
      ,ibkr_fee_rate   = ibkr_fee_rate
      ,min_trade_value = min_trade_value
      ,force_close_final = force_close_final
    )
    ,trades       = trades_final
    ,equity_curve = tibble(date = dates, equity = equity_curve)
    ,final_equity = tail(equity_curve, 1)
  )
}


#' calculate_summary_metrics_worker
#'
#' @description
#' Calculates comprehensive performance statistics for a single backtest result.
#'
#' This is a worker function designed to be used inside a parallel mapping 
#' function (like `furrr::future_map`). It processes a single result object, 
#' extracting trade-level metrics (number of trades, win rate) and portfolio-
#' level metrics (Total Return, CAGR, Max Drawdown, Sharpe, Sortino, and Calmar 
#' ratios) using the `PerformanceAnalytics` package.
#' 
#' **NOTE:** Win rate (`pct_profitable`) is strictly calculated based on complete
#' round-trip trades (BUY → SELL pairs) to ensure an unbiased statistic.
#'
#' @param res A single backtest result object (a list) from `backtest_strategy()`
#' containing `trades`, `equity_curve`, `final_equity`, and `parameters`.
#' @param initial_equity The initial cash outlay used for the trading simulation.
#'
#' @returns A single row tibble containing performance metrics for the given 
#' backtest.

calculate_summary_metrics_worker <- function(res, initial_equity) {
  
  message("Running: ", res$strategy_nm)
  
  strategy_type <- res$strategy_nm
  
  # 1. Extract and Calculate Basic Metrics
  stop_loss    <- res$parameters$stop_loss
  trades       <- res$trades
  equity_curve <- res$equity_curve
  final_equity <- res$final_equity
  
  # Ensure initial_equity is numeric for calculation
  initial_equity <- as.numeric(initial_equity) 
  total_return   <- (final_equity - initial_equity) / initial_equity
  
  # number of trades (rows in trades table)
  num_trades <- nrow(trades)
  
  # 2. Profitable Pairs Calculation
  pct_profitable        <- NA_real_
  num_complete_trades   <- 0L
  num_profitable_trades <- 0L
  
  if (num_trades >= 2) {
    trade_pairs <- trades %>%
      dplyr::mutate(pair_id = cumsum(action == "BUY")) %>%
      dplyr::group_by(pair_id) %>%
      dplyr::mutate(is_complete = n() == 2) %>%
      dplyr::filter(is_complete) %>%
      dplyr::summarise(
        units_traded = first(units)
        ,buy_price   = first(price)
        ,sell_price  = last(price)
        ,buy_fee     = first(fee)
        ,sell_fee    = last(fee)
        ,exit_reason = last(exit_reason)
        ,.groups = "drop"
      ) %>%
      dplyr::mutate(
        profit_loss = (sell_price - buy_price) * units_traded - (buy_fee + sell_fee)
        ,profitable = profit_loss > 0
      )
    
    if (nrow(trade_pairs) > 0) {
      num_complete_trades   <- nrow(trade_pairs)
      num_profitable_trades <- sum(trade_pairs$profitable, na.rm = TRUE)
      pct_profitable        <- num_profitable_trades / num_complete_trades
    }
  }
  
  # 3. PerformanceAnalytics Metrics
  eq_xts     <- xts::xts(equity_curve$equity, order.by = as.Date(equity_curve$date))
  daily_rets <- na.omit(PerformanceAnalytics::Return.calculate(eq_xts, method = "discrete"))
  
  if (NROW(daily_rets) >= 2) {
    years <- as.numeric(difftime(max(index(eq_xts)), min(index(eq_xts)), units = "days")) / 365.25
    
    CAGR <- ifelse(
      years > 0
      ,(as.numeric(final_equity) / initial_equity)^(1/years) - 1
      ,NA_real_
    )
    max_dd <- as.numeric(PerformanceAnalytics::maxDrawdown(daily_rets))
    
    sharpe <- tryCatch(
      as.numeric(PerformanceAnalytics::SharpeRatio.annualized(daily_rets, Rf = 0, scale = 252))
      ,error = function(e) NA_real_
    )
    sortino <- tryCatch(
      as.numeric(PerformanceAnalytics::SortinoRatio(daily_rets, MAR = 0))
      ,error = function(e) NA_real_
    )
    calmar <- ifelse(
      !is.na(max_dd) && max_dd > 0
      ,CAGR / max_dd
      ,NA_real_
    )
  } else {
    CAGR    <- NA_real_
    max_dd  <- NA_real_
    sharpe  <- NA_real_
    sortino <- NA_real_
    calmar  <- NA_real_
  }
  
  # 4. Final Tibble Output
  tibble::tibble(
    strategy_type         = strategy_type
    ,stop_loss            = stop_loss
    ,final_equity         = round(as.numeric(final_equity), 2)
    ,total_return         = round(total_return, 4)
    ,num_trades           = num_trades
    ,num_complete_trades  = num_complete_trades
    ,num_profitable_trades = num_profitable_trades
    ,pct_profitable       = ifelse(is.na(pct_profitable), NA_real_, round(pct_profitable, 4))
    ,CAGR                 = ifelse(is.na(CAGR), NA_real_, round(CAGR, 4))
    ,max_drawdown         = ifelse(is.na(max_dd), NA_real_, round(max_dd, 4))
    ,sharpe_ratio         = ifelse(is.na(sharpe), NA_real_, round(sharpe, 4))
    ,sortino_ratio        = ifelse(is.na(sortino), NA_real_, round(sortino, 4))
    ,calmar_ratio         = ifelse(is.na(calmar), NA_real_, round(calmar, 4))
  )
}


#' get_performance_metrics
#'
#' @description
#' Calculates comprehensive performance statistics across multiple backtest
#' results using parallel processing.
#'
#' This function iterates over a list of backtest results, delegating the heavy
#' calculation work to the `calculate_summary_metrics_worker` function in
#' parallel via `furrr::future_map_dfr`. It efficiently aggregates trade and
#' portfolio metrics across all strategies and Monte Carlo windows.
#'
#' @param results Named list of backtest results from `backtest_strategy()`,
#' where each element contains the output structure expected by the worker
#' function.
#' @param initial_equity Initial cash outlay for trading. Defaults to 10000.
#'
#' @returns A single tibble with combined performance metrics for all backtest
#' strategies, with each row representing a unique strategy/window combination,
#' identified by the `.id = "strategy"` column.

get_performance_metrics <- function(results, initial_equity = 10000) {
  
  furrr::future_map_dfr(
    .x = results 
    ,function(res) {
      calculate_summary_metrics_worker(res = res, initial_equity = initial_equity)
    }
    ,.id = "strategy"
    ,.progress = TRUE
    ,.options = furrr_options(globals = c("initial_equity", "calculate_summary_metrics_worker")) 
  )
  
}


#' sample_xts_window
#' 
#' @description
#' Randomly samples multiple time windows from an xts object for Monte Carlo
#' simulation. Each window has a randomly chosen length between
#' `min_window_length` and the full dataset length, starting at a random valid
#' index.
#'
#' @param prices_xts xts dataframe containing price data
#' @param sample_size Number of random windows to generate, defaults to 3
#' @param min_window_length Minimum number of periods in each window, defaults
#' to 250
#'
#' @returns A list with:
#'   - sampled_windows: list of xts subsets
#'   - window_lengths: integer vector of window sizes
#'   - start_dates: Date vector of window start dates
#'   - end_dates: Date vector of window end dates

sample_xts_window <- function(prices_xts, sample_size = 3, min_window_length = 250) {
  
  n <- NROW(prices_xts)
  
  # Validation
  stopifnot(min_window_length < n)
  
  # Randomly sample multiple window lengths
  win_lengths <- sample(min_window_length:(n - 1), size = sample_size, replace = TRUE)
  
  # For each window length, sample a random valid start index
  start_indices <- map_int(win_lengths, ~ sample(1:(n - .x + 1), 1))
  end_indices   <- start_indices + win_lengths - 1
  
  # Extract each window of data
  sampled_windows <- map2(start_indices, end_indices, ~ prices_xts[.x:.y]) %>% 
    set_names(nm = paste0("window_", seq_along(win_lengths)))
  
  # Extract metadata for tracking
  start_dates <- unname(map_chr(sampled_windows, ~ as.character(as.Date(index(.x)[1]))))
  end_dates   <- unname(map_chr(sampled_windows, ~ as.character(as.Date(index(.x)[nrow(.x)]))))
  
  message("Generated ", sample_size, " time windows")
  message("Window length range: ", min(win_lengths), " to ", max(win_lengths), " days")
  
  list(
    sampled_windows = sampled_windows
    ,window_lengths = win_lengths
    ,start_dates    = start_dates
    ,end_dates      = end_dates
  )
}


# 02 TRADING STRATEGY FUNCTIONS -------------------------------------------

#' strat_buy_hold
#' 
#' @description
#' Simple strategy of buying on the first day of trading and holding forever.
#' 
#' @param prices_xts xts dataframe containing trading data, in particular
#' dates and adjusted close prices
#'
#' @returns tibble with dates, adjusted close prices, and trade signals
#' (buy, sell, hold)

strat_buy_hold <- function(prices_xts) {
  
  n <- nrow(prices_xts)
  
  tibble(
    date         = index(prices_xts)
    ,price       = as.numeric(Ad(prices_xts))
    ,trade_signal = c(1L, rep(0L, n - 1))
  )
  
}


#' strat_sma
#' 
#' @description
#' Simple moving average - a trend-following strategy that detects trend
#' direction changes. Works best in trending markets and underperforms in
#' sideways/choppy/whipsaw markets.
#' 
#' Buy signals are triggered when price moves above the moving average.
#' Sell signals are triggered when price moves below the moving average.
#'
#' @param prices_xts xts dataframe containing trading data, in particular
#' dates and adjusted close prices
#' @param period_n SMA period to pass onto TTR::SMA, defaults to 50 periods
#'
#' @returns tibble with dates, adjusted close prices, SMA, and trade signals
#' (buy, sell, hold)

strat_sma <- function(prices_xts, period_n = 50) {
  
  if (nrow(prices_xts) < period_n) {
    stop("Insufficient data: need at least ", period_n, " periods")
  }
  
  tibble(
    date = index(prices_xts)
  ) %>% 
    mutate(
      price     = as.numeric(Ad(prices_xts))
      ,sma      = as.numeric(SMA(price, n = period_n))
      ,diff_now = price - sma
      ,diff_prev = dplyr::lag(diff_now)
      ,trade_signal = case_when(
        !is.na(diff_prev) & (diff_prev <= 0) & (diff_now > 0) ~  1L  # up-cross; buy
        ,!is.na(diff_prev) & (diff_prev >= 0) & (diff_now < 0) ~ -1L  # down-cross; sell
        ,.default = 0L # hold
      )
    ) %>% 
    select(
      -diff_now
      ,-diff_prev
    )
  
}


#' strat_sma_cross
#' 
#' @description
#' Dual simple moving average crossover - a trend-following strategy that
#' detects trend direction changes. Works best in trending markets and
#' underperforms in sideways/choppy/whipsaw markets.
#' 
#' Uses a short-term and a long-term SMA. Buy signals are triggered when the
#' short SMA crosses above the long SMA; sell signals when the short SMA
#' crosses below the long SMA.
#' 
#' @param prices_xts xts dataframe containing trading data, in particular
#' dates and adjusted close prices
#' @param short_n Short SMA period to pass onto TTR::SMA, defaults to 20
#' periods
#' @param long_n Long SMA period to pass onto TTR::SMA, defaults to 50 periods
#'
#' @returns tibble with dates, adjusted close prices, short SMA, long SMA, and
#' trade signals (buy, sell, hold)

strat_sma_cross <- function(prices_xts, short_n = 20, long_n = 50) {
  
  tibble(
    date = index(prices_xts)
  ) %>% 
    mutate(
      price      = as.numeric(Ad(prices_xts))
      ,sma_short = as.numeric(SMA(price, n = short_n))
      ,sma_long  = as.numeric(SMA(price, n = long_n))
      ,diff_now  = sma_short - sma_long
      ,diff_prev = dplyr::lag(diff_now)
      ,trade_signal = case_when(
        !is.na(diff_prev) & (diff_prev <= 0) & (diff_now > 0) ~  1L # buy
        ,!is.na(diff_prev) & (diff_prev >= 0) & (diff_now < 0) ~ -1L # sell 
        ,.default = 0L # hold
      )
    ) %>% 
    select(
      -diff_now
      ,-diff_prev
    )
  
}


#' strat_rsi
#' 
#' @description
#' Relative Strength Index - measures momentum as the ratio of recent upward
#' price movements to absolute price movement, ranging from 0 to 100.
#' 
#' RSI < 30 may indicate the market is oversold (price may rise → buy signal).
#' RSI > 70 may indicate the market is overbought (price may drop → sell signal).
#'
#' @param prices_xts xts dataframe containing trading data, in particular
#' dates and adjusted close prices
#' @param n_period Number of RSI periods to pass onto TTR::RSI, defaults to 14
#' @param lower Lower RSI band, defaults to 30
#' @param upper Upper RSI band, defaults to 70
#'
#' @returns tibble with dates, adjusted close prices, RSI, and trade signals
#' (buy, sell, hold)

strat_rsi <- function(prices_xts, n_period = 14, lower = 30, upper = 70) {
  
  tibble(
    date = index(prices_xts)
  ) %>% 
    mutate(
      price    = as.numeric(Ad(prices_xts))
      ,rsi     = as.numeric(RSI(price, n = n_period))
      ,prev_rsi = dplyr::lag(rsi)
      ,trade_signal = case_when(
        !is.na(prev_rsi) & (prev_rsi <= lower) & (rsi > lower) ~  1L # buy
        ,!is.na(prev_rsi) & (prev_rsi >= upper) & (rsi < upper) ~ -1L # sell
        ,.default = 0L # hold
      )
    ) %>% 
    select(-prev_rsi)
  
}


#' strat_bb
#' 
#' @description
#' Bollinger Bands - tracks volatility by measuring how much price disperses
#' around a moving average (default 20-day). When returns are normally
#' distributed, prices will rarely exceed the upper or lower band.
#' 
#' Price above the upper band suggests overbought → sell signal.
#' Price below the lower band suggests oversold → buy signal.
#'
#' @param prices_xts xts dataframe containing trading data, in particular
#' dates and High, Low, and Close prices
#' @param n Number of moving average periods to pass onto TTR::BBands,
#' defaults to 20
#' @param sd Standard deviation multiplier to pass onto TTR::BBands,
#' defaults to 2
#'
#' @returns tibble with dates, adjusted close prices, upper band, middle band,
#' lower band, and trade signals (buy, sell, hold)

strat_bb <- function(prices_xts, n = 20, sd = 2) {
  
  hlc_xts <- HLC(prices_xts)
  
  bb <- BBands(
    HLC    = hlc_xts
    ,n     = n
    ,sd    = sd
    ,maType = "SMA"
  )
  
  tibble(
    date = index(prices_xts)
  ) %>% 
    mutate(
      price      = as.numeric(Ad(prices_xts))
      ,bb_up     = as.numeric(bb$up)
      ,bb_down   = as.numeric(bb$dn)
      ,bb_mavg   = as.numeric(bb$mavg)
      ,prev_price   = dplyr::lag(price)
      ,prev_bb_down = dplyr::lag(bb_down)
      ,prev_bb_up   = dplyr::lag(bb_up)
      ,trade_signal = case_when(
        # Cross below lower band: price was at/above lower band yesterday, now below it
        !is.na(prev_price) & (prev_price >= prev_bb_down) & (price < bb_down) ~  1L  # Buy
        # Cross above upper band: price was at/below upper band yesterday, now above it
        ,!is.na(prev_price) & (prev_price <= prev_bb_up)  & (price > bb_up)  ~ -1L  # Sell
        ,.default = 0L # Hold
      )
    ) %>%
    select(-prev_price, -prev_bb_down, -prev_bb_up)
  
}


#' strat_macd
#' 
#' @description
#' Moving Average Convergence Divergence - a momentum tracking indicator with
#' three components:
#' 
#' - **MACD Line**: difference between a fast EMA (12-period) and slow EMA
#'   (26-period), representing short-term vs long-term momentum.
#' - **Signal Line**: EMA of the MACD line.
#' - **Histogram**: difference between the MACD line and signal line. Positive
#'   values indicate increasing momentum; negative values indicate decreasing
#'   momentum.
#' 
#' Trade signals are generated when the histogram crosses zero: buy when it
#' crosses from negative to positive, sell when it crosses from positive to
#' negative.
#' 
#' MACD responds quicker to recent price changes than SMA-based indicators and
#' excels in trending markets, but performs poorly in sideways/whipsaw markets
#' and may produce false breakouts during high-volatility periods.
#'
#' @param prices_xts xts dataframe containing trading data, in particular
#' dates and adjusted close prices
#' @param n_fast Number of periods for fast MA to pass onto TTR::MACD,
#' defaults to 12 periods
#' @param n_slow Number of periods for slow MA to pass onto TTR::MACD,
#' defaults to 26 periods
#' @param n_signal Number of periods for signal MA to pass onto TTR::MACD,
#' defaults to 9 periods
#'
#' @returns tibble with dates, adjusted close prices, MACD line, signal line,
#' histogram, and trade signals (buy, sell, hold)

strat_macd <- function(prices_xts, n_fast = 12, n_slow = 26, n_signal = 9) {
  
  close_xts <- Ad(prices_xts)
  
  macd_obj <- MACD(
    x       = close_xts
    ,nFast  = n_fast
    ,nSlow  = n_slow
    ,nSig   = n_signal
    ,maType = EMA
    ,percent = FALSE
  )
  
  tibble(
    date = index(prices_xts)
  ) %>% 
    mutate(
      price        = as.numeric(close_xts)
      ,macd_line   = as.numeric(macd_obj[, "macd"])
      ,signal_line = as.numeric(macd_obj[, "signal"])
      ,hist        = macd_line - signal_line
      ,prev_hist   = dplyr::lag(hist)
      ,trade_signal = case_when(
        (!is.na(prev_hist)) & (prev_hist <= 0) & (hist > 0) ~  1L # Buy
        ,(!is.na(prev_hist)) & (prev_hist >= 0) & (hist < 0) ~ -1L # Sell
        ,.default = 0L # Hold
      )
    ) %>% 
    select(-prev_hist)
  
}


#' strat_macdv
#' 
#' @description
#' MACD-V is a volatility-normalised variant of MACD, authored by Alex
#' Spiroglou, that addresses MACD's sensitivity to high-volatility periods.
#' See the original paper:
#' https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4099617
#' 
#' The MACD line is normalised by dividing by ATR (Average True Range),
#' transforming it from a raw to a relative momentum indicator. With the
#' default threshold of 50, the histogram is divided into four zones:
#' 
#' - `hist > +150` → overbought → HOLD
#' - `hist < -150` → oversold → HOLD
#' - `-50 < hist < +50` → no signal → HOLD
#' - `+50 < hist < +150` → SELL zone
#' - `-150 < hist < -50` → BUY zone
#'
#' @param prices_xts xts dataframe containing trading data, in particular
#' dates and High, Low, and Close prices
#' @param n_fast Number of periods for fast MA to pass onto TTR::MACD,
#' defaults to 12 periods
#' @param n_slow Number of periods for slow MA to pass onto TTR::MACD,
#' defaults to 26 periods
#' @param n_signal Number of periods for signal MA to pass onto TTR::MACD,
#' defaults to 9 periods
#' @param atr_period Number of periods for ATR to pass onto TTR::ATR,
#' defaults to 26 periods
#' @param strength_threshold Spiroglou's fixed strength threshold, defaults
#' to 50
#'
#' @returns tibble with dates, adjusted close prices, MACD-V line, MACD-V
#' signal line, histogram, and trade signals (buy, sell, hold)

strat_macdv <- function(
    prices_xts
    ,n_fast = 12
    ,n_slow = 26
    ,n_signal = 9
    ,atr_period = 26
    ,strength_threshold = 50 # Spiroglou's version is fixed at 50
) {
  
  close_xts <- Ad(prices_xts)
  
  macd_obj <- MACD(
    x       = close_xts
    ,nFast  = n_fast
    ,nSlow  = n_slow
    ,nSig   = n_signal
    ,maType = EMA
    ,percent = FALSE
  )
  
  macd_line <- as.numeric(macd_obj[, "macd"])
  
  atr_xts  <- ATR(HLC(prices_xts), n = atr_period) # Compute ATR for volatility normalization
  atr_vec  <- as.numeric(atr_xts[, "atr"])
  safe_atr <- pmax(atr_vec, 1e-8) # Avoid divide-by-zero errors
  
  macdv_line   <- (macd_line / safe_atr) * 100        # Compute MACD-V line (ATR-normalised MACD)
  macdv_signal <- EMA(macdv_line, n = n_signal)        # Compute MACD-V signal and histogram
  
  tibble(
    date = index(prices_xts)
  ) %>% 
    mutate(
      price         = as.numeric(close_xts)
      ,macdv_line   = as.numeric(macdv_line)
      ,macdv_signal = as.numeric(macdv_signal)
      ,hist         = macdv_line - macdv_signal
      ,trade_signal = case_when(
        (hist <= -strength_threshold) & (hist > (-3 * strength_threshold)) ~  1L # BUY zone: histogram between -150 and -50
        ,(hist >=  strength_threshold) & (hist < ( 3 * strength_threshold)) ~ -1L # SELL zone: histogram between +50 and +150
        ,(abs(hist) <=     strength_threshold) ~ 0L # Everything between -50 and +50 → HOLD
        ,(abs(hist) >= 3 * strength_threshold) ~ 0L # Everything < -150 and > +150   → HOLD
        ,.default = 0L # HOLD
      )
    )
}


#' strat_macdv_dynamic_strength
#' 
#' @description
#' An adaptive extension of Spiroglou's MACD-V that replaces the fixed strength
#' threshold with a dynamic one derived from a rolling quantile of recent
#' histogram values.
#' 
#' Signals are first generated identically to `strat_macdv`, then filtered by
#' the rolling quantile threshold. If the absolute histogram value falls below
#' the rolling quantile, the signal is suppressed to HOLD; if it exceeds it,
#' the original buy/sell signal is kept.
#' 
#' **Effect of `roll_window`:**
#' - Smaller (e.g. 10): threshold adapts quickly, more responsive to volatility
#'   spikes but prone to noise.
#' - Larger (e.g. 50): smoother, more stable threshold but may lag recent
#'   market conditions.
#' - Default of 20 balances reactivity with stability.
#' 
#' While Spiroglou's version trades only within fixed absolute zones, this
#' version trades when price movements are in the top (1 - `strength_quantile`)
#' of recent momentum.
#'
#' @param prices_xts xts dataframe containing trading data, in particular
#' dates and High, Low, and Close prices
#' @param n_fast Number of periods for fast MA to pass onto TTR::MACD,
#' defaults to 12 periods
#' @param n_slow Number of periods for slow MA to pass onto TTR::MACD,
#' defaults to 26 periods
#' @param n_signal Number of periods for signal MA to pass onto TTR::MACD,
#' defaults to 9 periods
#' @param atr_period Number of periods for ATR to pass onto TTR::ATR,
#' defaults to 26 periods
#' @param roll_window Rolling window for adaptive threshold, defaults to 20
#' periods
#' @param strength_quantile Percentile for adaptive threshold, defaults to 0.80
#'
#' @returns tibble with dates, adjusted close prices, MACD-V line, MACD-V
#' signal line, histogram, rolling threshold, and trade signals (buy, sell,
#' hold)

strat_macdv_dynamic_strength <- function(
    prices_xts
    ,n_fast = 12
    ,n_slow = 26
    ,n_signal = 9
    ,atr_period = 26
    ,roll_window = 20         
    ,strength_quantile = 0.8   
) {
  
  close_xts <- Ad(prices_xts)
  
  macd_obj <- MACD(
    x       = close_xts
    ,nFast  = n_fast
    ,nSlow  = n_slow
    ,nSig   = n_signal
    ,maType = EMA
    ,percent = FALSE
  )
  
  macd_line <- as.numeric(macd_obj[, "macd"])
  
  atr_xts  <- ATR(HLC(prices_xts), n = atr_period) # Compute ATR for volatility normalization
  atr_vec  <- as.numeric(atr_xts[, "atr"])
  safe_atr <- pmax(atr_vec, 1e-8) # Avoid divide-by-zero errors
  
  macdv_line   <- (macd_line / safe_atr) * 100        # Compute MACD-V line (ATR-normalised MACD)
  macdv_signal <- EMA(macdv_line, n = n_signal)        # Compute MACD-V signal and histogram
  
  tibble(
    date = index(prices_xts)
  ) %>% 
    mutate(
      price        = as.numeric(close_xts)
      ,macdv       = as.numeric(macdv_line)
      ,macdv_signal = as.numeric(macdv_signal)
      ,hist        = macdv_line - macdv_signal
      ,prev_hist   = dplyr::lag(hist)
      ,trade_signal_original = case_when( # Raw signals based on histogram crossovers
        (!is.na(prev_hist)) & (prev_hist <= 0) & (hist > 0) ~  1L # Buy
        ,(!is.na(prev_hist)) & (prev_hist >= 0) & (hist < 0) ~ -1L # Sell
        ,.default = 0L # Hold
      ) 
      ,abs_hist = abs(hist)
      ,roll_threshold = as.numeric(
        rollapply(
          data   = abs_hist
          ,width = roll_window
          ,FUN   = function(x) quantile(x, probs = strength_quantile, na.rm = TRUE)
          ,fill  = NA
          ,align = "right"
        )
      )
      ,trade_signal = ifelse(
        test = abs_hist >= roll_threshold
        ,yes  = trade_signal_original
        ,no   = 0L
      )
    ) %>% 
    select(
      -prev_hist
      ,-abs_hist
    )
  
}
