# =============================================================================
# ibkr_api.R
# IBKR Client Portal REST API wrapper (proto-package)
#
# Wraps the IBKR Client Portal Gateway REST API for use in live trading.
# Requires the Client Portal Gateway to be running locally before use.
#
# Usage:
#   source("R/live_trading/ibkr_api.R")
#
# Dependencies:
#   httr2, jsonlite
#
# Notes:
#   - The Client Portal Gateway must be running before calling any function.
#   - All requests disable SSL verification (gateway uses a self-signed cert).
#   - Call ibkr_tickle() at the start of each trading session to confirm the
#     session is alive. Sessions time out after ~5 minutes without a request.
#   - conids (IBKR contract IDs) are resolved dynamically via ibkr_get_conids().
#   - ibkr_get_price_history() returns daily OHLCV bars and is the sole price
#     data source for both signal generation and order sizing in live trading.
# =============================================================================

library(httr2)
library(jsonlite)

# Configuration ----------------------------------------------------------------

IBKR_BASE_URL <- "https://localhost:5000/v1/api"

# Internal helpers -------------------------------------------------------------

#' Make a GET request to the IBKR Client Portal API
#'
#' @param endpoint API endpoint path (e.g. "/tickle")
#' @param params Named list of query parameters (optional)
#' @return Parsed JSON response as an R list
#' @noRd
ibkr_get <- function(endpoint, params = NULL) {
  url <- paste0(IBKR_BASE_URL, endpoint)

  req <- request(url) |>
    req_options(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE) |>
    req_headers(
      "User-Agent" = "R/ibkr_api",
      "Accept"     = "*/*",
      "Connection" = "keep-alive"
    )

  if (!is.null(params)) {
    req <- req |> req_url_query(!!!params)
  }

  resp <- req |> req_perform()

  if (resp_status(resp) != 200) {
    stop(sprintf(
      "IBKR API GET %s failed with status %d: %s",
      endpoint, resp_status(resp), resp_body_string(resp)
    ))
  }

  resp |> resp_body_json()
}

#' Make a POST request to the IBKR Client Portal API
#'
#' @param endpoint API endpoint path
#' @param body Named list to be serialised as JSON body
#' @return Parsed JSON response as an R list
#' @noRd
ibkr_post <- function(endpoint, body = NULL) {
  url <- paste0(IBKR_BASE_URL, endpoint)

  req <- request(url) |>
    req_options(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE) |>
    req_headers(
      "User-Agent"   = "R/ibkr_api",
      "Accept"       = "*/*",
      "Connection"   = "keep-alive",
      "Content-Type" = "application/json"
    )

  if (!is.null(body)) {
    req <- req |> req_body_json(body)
  }

  resp <- req |> req_perform()

  if (!resp_status(resp) %in% c(200, 201)) {
    stop(sprintf(
      "IBKR API POST %s failed with status %d: %s",
      endpoint, resp_status(resp), resp_body_string(resp)
    ))
  }

  resp |> resp_body_json()
}

#' Make a DELETE request to the IBKR Client Portal API
#'
#' @param endpoint API endpoint path
#' @return Parsed JSON response as an R list
#' @noRd
ibkr_delete <- function(endpoint) {
  url <- paste0(IBKR_BASE_URL, endpoint)

  req <- request(url) |>
    req_options(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE) |>
    req_headers(
      "User-Agent" = "R/ibkr_api",
      "Accept"     = "*/*",
      "Connection" = "keep-alive"
    )

  resp <- req |> req_perform()

  if (resp_status(resp) != 200) {
    stop(sprintf(
      "IBKR API DELETE %s failed with status %d: %s",
      endpoint, resp_status(resp), resp_body_string(resp)
    ))
  }

  resp |> resp_body_json()
}

# Session ----------------------------------------------------------------------

#' Tickle the session to confirm it is alive
#'
#' Should be called at the start of each trading session. Sessions time out
#' after approximately 5 minutes without a request.
#'
#' @return Invisibly returns the response list. Stops with an error if the
#'   session is not authenticated.
ibkr_tickle <- function() {
  resp <- ibkr_get("/tickle")

  if (!isTRUE(resp$iserver$authStatus$authenticated)) {
    stop(
      "IBKR session is not authenticated. ",
      "Please log in via the Client Portal Gateway before trading."
    )
  }

  message("IBKR session is alive and authenticated.")
  invisible(resp)
}

#' Get the current session authentication status
#'
#' @return Named list with session status fields including `authenticated`,
#'   `connected`, and `competing`
ibkr_auth_status <- function() {
  ibkr_get("/iserver/auth/status")
}

#' Reauthenticate the session
#'
#' Call this if the session has timed out but the gateway is still running.
#'
#' @return Invisibly returns the response list
ibkr_reauthenticate <- function() {
  resp <- ibkr_post("/iserver/reauthenticate")
  message("Reauthentication requested. Wait a few seconds then call ibkr_tickle().")
  invisible(resp)
}

# Account ----------------------------------------------------------------------

#' Get all accounts associated with the authenticated user
#'
#' @return Data frame with account details (accountId, type, currency, etc.)
ibkr_get_accounts <- function() {
  resp <- ibkr_get("/portfolio/accounts")
  do.call(rbind, lapply(resp, as.data.frame, stringsAsFactors = FALSE))
}

#' Get portfolio summary (cash balances etc.) for an account
#'
#' @param account_id IBKR account ID string (e.g. "U1234567")
#' @return Named list of summary fields including cash balance
ibkr_get_summary <- function(account_id) {
  ibkr_get(sprintf("/portfolio/%s/summary", account_id))
}

#' Get current positions for an account
#'
#' @param account_id IBKR account ID string
#' @return Data frame of current positions with columns: conid, symbol,
#'   position, mkt_price, mkt_value, avg_cost, unrealised_pnl, currency
ibkr_get_positions <- function(account_id) {
  resp <- ibkr_get(sprintf("/portfolio/%s/positions/0", account_id))

  if (length(resp) == 0) {
    message("No open positions found for account ", account_id)
    return(data.frame())
  }

  positions <- lapply(resp, function(pos) {
    data.frame(
      conid          = pos$conid,
      symbol         = pos$contractDesc,
      position       = pos$position,
      mkt_price      = pos$mktPrice,
      mkt_value      = pos$mktValue,
      avg_cost       = pos$avgCost,
      unrealised_pnl = pos$unrealizedPnl,
      currency       = pos$currency,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, positions)
}

# Market data ------------------------------------------------------------------

#' Look up conids (IBKR contract IDs) for a vector of ASX ticker symbols
#'
#' Conids are IBKR's internal instrument identifiers, required for market data
#' and order placement. This function queries the search endpoint dynamically.
#'
#' @param symbols Character vector of ASX ticker symbols without the .AX suffix
#'   (e.g. c("VGS", "VAS", "GOLD"))
#' @return Named integer vector of conids, named by symbol. Stops with an error
#'   if any symbol cannot be resolved.
ibkr_get_conids <- function(symbols) {
  conids <- vapply(symbols, function(sym) {
    resp <- ibkr_get("/iserver/secdef/search", params = list(
      symbol  = sym,
      secType = "STK"
    ))

    if (length(resp) == 0) {
      stop(sprintf("Could not resolve conid for symbol: %s", sym))
    }

    asx_matches <- Filter(function(x) {
      any(grepl("ASX", unlist(x$sections), ignore.case = TRUE))
    }, resp)

    if (length(asx_matches) == 0) {
      stop(sprintf(
        "No ASX-listed contract found for symbol: %s. ",
        "Check the symbol or inspect the secdef/search endpoint manually.", sym
      ))
    }

    as.integer(asx_matches[[1]]$conid)
  }, integer(1))

  message(sprintf(
    "Resolved conids: %s",
    paste(names(conids), conids, sep = "=", collapse = ", ")
  ))

  conids
}

#' Get daily price history for a single conid
#'
#' Fetches daily OHLCV bars from IBKR for use in signal generation and order
#' sizing. This is the sole price data source for live trading — Yahoo Finance
#' is used for backtesting only.
#'
#' @param conid Integer conid of the instrument
#' @param period History period string (default: "1y" for ~252 trading days).
#'   Other valid values: "3m", "6m", "2y", "3y", "5y"
#' @return Data frame with columns: date, open, high, low, close, volume
ibkr_get_price_history <- function(conid, period = "1y") {
  resp <- ibkr_get("/iserver/marketdata/history", params = list(
    conid  = conid,
    period = period,
    bar    = "1d",
    outsideRth = FALSE   # Regular trading hours only
  ))

  if (is.null(resp$data) || length(resp$data) == 0) {
    stop(sprintf(
      "No price history returned for conid %d. ",
      "Ensure the brokerage session is active (not just read-only).", conid
    ))
  }

  bars <- lapply(resp$data, function(bar) {
    data.frame(
      date   = as.Date(as.POSIXct(bar$t / 1000, origin = "1970-01-01", tz = "Australia/Sydney")),
      open   = bar$o,
      high   = bar$h,
      low    = bar$l,
      close  = bar$c,
      volume = bar$v,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, bars)
  result[order(result$date), ]   # ensure chronological order
}

# Orders -----------------------------------------------------------------------

#' Get all live/open orders
#'
#' @return Data frame of open orders, or an empty data frame if none exist
ibkr_get_orders <- function() {
  resp <- ibkr_get("/iserver/orders")
  orders <- resp$orders

  if (is.null(orders) || length(orders) == 0) {
    message("No open orders found.")
    return(data.frame())
  }

  do.call(rbind, lapply(orders, function(o) {
    data.frame(
      order_id   = o$orderId,
      conid      = o$conid,
      symbol     = o$ticker,
      side       = o$side,
      order_type = o$orderType,
      quantity   = o$totalSize,
      status     = o$status,
      stringsAsFactors = FALSE
    )
  }))
}

#' Place a market order
#'
#' Places a DAY market order for a single instrument. The API may return
#' confirmation prompts — these are handled automatically.
#'
#' @param account_id IBKR account ID string
#' @param conid Integer conid of the instrument
#' @param side "BUY" or "SELL"
#' @param quantity Number of shares (positive integer)
#' @return Invisibly returns the final order response list
ibkr_place_order <- function(account_id, conid, side, quantity) {
  side     <- toupper(side)
  quantity <- as.integer(quantity)

  if (!side %in% c("BUY", "SELL")) stop("side must be 'BUY' or 'SELL'")
  if (quantity <= 0)               stop("quantity must be a positive integer")

  order_body <- list(
    orders = list(
      list(
        conid     = conid,
        orderType = "MKT",
        side      = side,
        quantity  = quantity,
        tif       = "DAY"
      )
    )
  )

  resp <- ibkr_post(
    sprintf("/iserver/account/%s/orders", account_id),
    body = order_body
  )

  resp <- ibkr_confirm_order_messages(resp)

  message(sprintf(
    "Order placed: %s %d shares (conid %d) for account %s",
    side, quantity, conid, account_id
  ))

  invisible(resp)
}

#' Confirm any order reply messages returned by IBKR
#'
#' IBKR may return confirmation prompts after placing an order (e.g. price
#' deviation warnings). This function automatically confirms them.
#' Called internally by ibkr_place_order().
#'
#' @param resp Response list from the place order POST
#' @return Final response after all messages are confirmed
#' @noRd
ibkr_confirm_order_messages <- function(resp) {
  while (!is.null(resp[[1]]$id) && !is.null(resp[[1]]$message)) {
    message_id <- resp[[1]]$id
    message(sprintf("Confirming order message: %s", resp[[1]]$message[[1]]))
    resp <- ibkr_post(
      sprintf("/iserver/reply/%s", message_id),
      body = list(confirmed = TRUE)
    )
  }
  resp
}

#' Cancel an open order
#'
#' @param account_id IBKR account ID string
#' @param order_id Order ID to cancel (as returned by ibkr_get_orders)
#' @return Invisibly returns the response list
ibkr_cancel_order <- function(account_id, order_id) {
  resp <- ibkr_delete(sprintf("/iserver/account/%s/order/%s", account_id, order_id))
  message(sprintf("Order %s cancelled for account %s", order_id, account_id))
  invisible(resp)
}
