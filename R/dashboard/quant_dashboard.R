# =============================================================================
# quant_dashboard.R
# Shiny dashboard for monitoring the quant trading system
#
# Reads data from Google Drive synced files:
#   - outputs/live_trading/state.rds              — current positions and cash
#   - outputs/live_trading/trade_log.csv          — trade history
#   - outputs/live_trading/prices/{SYMBOL}.rds    — cumulative price history
#   - outputs/live_trading/logs/                  — daily run logs
#
# Config is sourced from R/quant_vars.R
#
# Run locally in RStudio:
#   shiny::runApp("R/dashboard/quant_dashboard.R")
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(lubridate)
library(DT)
library(plotly)
library(TTR)
library(zoo)

# =============================================================================
# Configuration — sourced from quant_vars.R
# =============================================================================

PROJECT_ROOT      <- "G:/My Drive/Shares and Crypto/quant_trading"
LIVE_TRADING_PATH <- file.path(PROJECT_ROOT, "outputs", "live_trading")

if (Sys.getenv("IBKR_ACCOUNT_ID") == "") Sys.setenv(IBKR_ACCOUNT_ID = "DASHBOARD")
source(file.path(PROJECT_ROOT, "R", "quant_vars.R"))

# =============================================================================
# Helper functions
# =============================================================================

load_state <- function() {
  path <- file.path(LIVE_TRADING_PATH, "state.rds")
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

load_trade_log <- function() {
  path <- file.path(LIVE_TRADING_PATH, "trade_log.csv")
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE) |> mutate(date = as.Date(date))
}

load_price_history <- function(symbol) {
  path <- file.path(LIVE_TRADING_PATH, "prices", paste0(symbol, ".rds"))
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

load_log_files <- function(pattern, subfolder) {
  log_dir <- file.path(LIVE_TRADING_PATH, "logs", subfolder)
  if (!dir.exists(log_dir)) return(character(0))
  files <- list.files(log_dir, pattern = pattern, full.names = FALSE)
  sort(files, decreasing = TRUE)
}

read_log_file <- function(filename, subfolder) {
  path <- file.path(LIVE_TRADING_PATH, "logs", subfolder, filename)
  if (!file.exists(path)) return("")
  paste(readLines(path), collapse = "\n")
}

check_fetch_status <- function() {
  today_log <- sprintf("quant_fetch_price_hist_%s.log", Sys.Date())
  path      <- file.path(LIVE_TRADING_PATH, "logs", "quant_fetch_price_hist", today_log)

  if (!file.exists(path)) {
    logs <- load_log_files("quant_fetch_price_hist_.*\\.log$", "quant_fetch_price_hist")
    if (length(logs) == 0) return(list(status = "unknown", label = "No fetch logs found", date = NA))
    path     <- file.path(LIVE_TRADING_PATH, "logs", "quant_fetch_price_hist", logs[1])
    log_date <- sub("quant_fetch_price_hist_(.*)\\.log", "\\1", logs[1])
  } else {
    log_date <- as.character(Sys.Date())
  }

  lines     <- readLines(path)
  last_line <- tail(lines[nzchar(lines)], 1)
  success   <- grepl("SUCCESS", last_line, ignore.case = TRUE)

  list(
    status = if (success) "success" else "error",
    label  = if (success) paste("OK —", log_date) else paste("FAILED —", log_date),
    date   = log_date
  )
}

colour_log_lines <- function(contents) {
  if (nchar(contents) == 0) return('<div class="log-viewer">Empty log file.</div>')
  lines    <- strsplit(contents, "\n")[[1]]
  coloured <- sapply(lines, function(line) {
    col <- if (grepl("SUCCESS|log_success", line, ignore.case = TRUE)) "#26a69a" else
           if (grepl("\\| ERROR", line))  "#ef5350" else
           if (grepl("\\| WARN",  line))  "#ffa726" else
           if (grepl("\\| INFO",  line))  "#b0bec5" else "#546e7a"
    sprintf('<span style="color:%s;">%s</span>', col, htmltools::htmlEscape(line))
  })
  sprintf('<div class="log-viewer">%s</div>', paste(coloured, collapse = "<br>"))
}

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = tags$span(
    tags$span("QUANT", style = "font-weight:900; letter-spacing:0.15em;"),
    tags$span(" TRADER", style = "font-weight:300; letter-spacing:0.1em; opacity:0.7;")
  ),
  window_title = "Quant Trader Dashboard",
  theme = bs_theme(
    bg           = "#0a0e1a",
    fg           = "#e8eaf0",
    primary      = "#4fc3f7",
    secondary    = "#546e7a",
    success      = "#26a69a",
    danger       = "#ef5350",
    warning      = "#ffa726",
    base_font    = font_google("JetBrains Mono"),
    heading_font = font_google("Space Mono"),
    font_scale   = 0.9,
    `border-radius` = "4px",
    `navbar-bg`  = "#060912"
  ),
  tags$head(tags$style(HTML("
    body { background: #0a0e1a; }
    .navbar { border-bottom: 1px solid #1e2d3d; }
    .card { background: #0d1526; border: 1px solid #1e2d3d; border-radius: 6px; }
    .card-header {
      background: #111d2e; border-bottom: 1px solid #1e2d3d;
      font-family: 'Space Mono', monospace; font-size: 0.75rem;
      letter-spacing: 0.12em; text-transform: uppercase; color: #4fc3f7; padding: 10px 16px;
    }
    .card-body { padding: 16px; }
    .metric-value { font-family: 'Space Mono', monospace; font-size: 1.8rem; font-weight: 700; line-height: 1.1; }
    .metric-label { font-size: 0.7rem; letter-spacing: 0.1em; text-transform: uppercase; color: #546e7a; margin-top: 4px; }
    .positive { color: #26a69a; }
    .negative { color: #ef5350; }
    .neutral  { color: #e8eaf0; }
    .log-viewer {
      background: #060912; border: 1px solid #1e2d3d; border-radius: 4px; padding: 12px;
      font-family: 'JetBrains Mono', monospace; font-size: 0.72rem; line-height: 1.6;
      color: #b0bec5; white-space: pre-wrap; overflow-y: auto; max-height: 500px;
    }
    .etf-badge {
      display: inline-block; padding: 2px 8px; border-radius: 3px;
      font-size: 0.7rem; font-weight: 700; letter-spacing: 0.08em;
      background: #1e2d3d; color: #4fc3f7;
    }
    .status-badge {
      display: inline-block; padding: 3px 10px; border-radius: 3px;
      font-size: 0.7rem; font-weight: 700; letter-spacing: 0.08em;
      font-family: 'Space Mono', monospace;
    }
    .status-success { background: #1a3a2a; color: #26a69a; border: 1px solid #26a69a; }
    .status-error   { background: #3a1a1a; color: #ef5350; border: 1px solid #ef5350; }
    .status-unknown { background: #1e2d3d; color: #546e7a; border: 1px solid #546e7a; }
    .last-updated { font-size: 0.65rem; color: #37474f; font-family: 'JetBrains Mono', monospace; }
    .dataTables_wrapper { color: #b0bec5; }
    table.dataTable thead th {
      background: #111d2e !important; color: #4fc3f7 !important;
      border-bottom: 1px solid #1e2d3d !important;
      font-family: 'Space Mono', monospace; font-size: 0.7rem;
      letter-spacing: 0.08em; text-transform: uppercase;
    }
    table.dataTable tbody tr { background: #0d1526 !important; }
    table.dataTable tbody tr:hover { background: #111d2e !important; }
    table.dataTable tbody td { border-top: 1px solid #1a2535 !important; color: #b0bec5; }
    .dataTables_info, .dataTables_paginate { color: #546e7a !important; font-size: 0.75rem; }
    .page-item .page-link { background: #111d2e; border-color: #1e2d3d; color: #4fc3f7; }
    .selectize-input { background: #111d2e !important; border-color: #1e2d3d !important; color: #e8eaf0 !important; }
    .selectize-dropdown { background: #111d2e !important; border-color: #1e2d3d !important; color: #e8eaf0 !important; }
    .form-select, .form-control { background: #111d2e !important; border-color: #1e2d3d !important; color: #e8eaf0 !important; }
    select[size] { background: #111d2e !important; border: 1px solid #1e2d3d !important; color: #e8eaf0 !important; border-radius: 4px; padding: 4px; }
    select[size] option:hover, select[size] option:checked { background: #1e2d3d !important; }
  "))),

  # --- Portfolio tab ---
  nav_panel("Portfolio",
    icon = icon("chart-line"),
    br(),
    fluidRow(
      column(3, card(card_header("Total Portfolio Value"),
        card_body(uiOutput("total_value_ui"), div(class="last-updated", textOutput("last_updated"))))),
      column(3, card(card_header("Unrealised P&L"),   card_body(uiOutput("unrealised_pnl_ui")))),
      column(3, card(card_header("Realised P&L"),     card_body(uiOutput("realised_pnl_ui")))),
      column(3, card(card_header("Total P&L"),        card_body(uiOutput("total_pnl_ui"))))
    ),
    br(),
    card(card_header("Open Positions"), card_body(uiOutput("positions_ui")))
  ),

  # --- Signals tab ---
  nav_panel("Signals",
    icon = icon("signal"),
    br(),
    card(
      card_header("Price & Signal Chart"),
      card_body(
        fluidRow(
          column(3, selectInput("signal_symbol", "Symbol",
            choices = etf_symbols, selected = etf_symbols[1], width = "100%")),
          column(3, selectInput("signal_overlay", "Overlay",
            choices  = c("None", "MACD-V", "RSI"),
            selected = "None", width = "100%"))
        ),
        plotlyOutput("signal_chart", height = "500px")
      )
    ),
    br(),
    card(card_header("Signal History"), card_body(DTOutput("signals_table")))
  ),

  # --- Trades tab ---
  nav_panel("Trades",
    icon = icon("exchange-alt"),
    br(),
    card(card_header("Trade History"), card_body(DTOutput("trades_table")))
  ),

  # --- Logs tab ---
  nav_panel("Logs",
    icon = icon("terminal"),
    br(),
    fluidRow(
      column(3,
        card(
          card_header("Price Fetch Status"),
          card_body(uiOutput("fetch_status_ui"))
        ),
        br(),
        card(
          card_header("Trading Logs"),
          card_body(
            selectInput("trade_log_select", NULL,
              choices = character(0), size = 10,
              selectize = FALSE, width = "100%")
          )
        ),
        br(),
        card(
          card_header("Price Fetch Logs"),
          card_body(
            selectInput("fetch_log_select", NULL,
              choices = character(0), size = 10,
              selectize = FALSE, width = "100%")
          )
        )
      ),
      column(9,
        card(
          card_header("Log Contents"),
          card_body(
            actionButton("refresh_logs", "Refresh",
              class = "btn-sm btn-outline-info mb-2", icon = icon("sync")),
            htmlOutput("log_contents")
          )
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(actionButton("refresh_all", "Refresh Data",
    class = "btn-sm btn-outline-info", icon = icon("sync")))
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  state_data <- reactive({ input$refresh_all; load_state() })
  trade_data <- reactive({ input$refresh_all; load_trade_log() })

  trade_log_files <- reactive({
    input$refresh_all; input$refresh_logs
    load_log_files("quant_trader_.*\\.log$", "quant_trader")
  })

  fetch_log_files <- reactive({
    input$refresh_all; input$refresh_logs
    load_log_files("quant_fetch_price_hist_.*\\.log$", "quant_fetch_price_hist")
  })

  observe({
    updateSelectInput(session, "trade_log_select", choices = trade_log_files(),
      selected = if (length(trade_log_files()) > 0) trade_log_files()[1] else NULL)
  })

  observe({
    updateSelectInput(session, "fetch_log_select", choices = fetch_log_files(),
      selected = if (length(fetch_log_files()) > 0) fetch_log_files()[1] else NULL)
  })

  # --- Portfolio ---

  output$last_updated <- renderText({
    state <- state_data()
    if (is.null(state)) return("No data")
    paste("Last updated:", state$last_updated[1])
  })

  get_realised_pnl <- reactive({
    trade_log <- trade_data()
    if (is.null(trade_log) || nrow(trade_log) == 0) return(0)
    buys  <- trade_log |> filter(side == "BUY") |> group_by(symbol) |>
      summarise(total_units = sum(units), avg_cost = sum(net_value)/sum(units), .groups = "drop")
    sells <- trade_log |> filter(side == "SELL") |> group_by(symbol) |>
      summarise(sold_units = sum(units), sell_proceeds = sum(net_value), .groups = "drop")
    realised <- buys |> inner_join(sells, by = "symbol") |>
      mutate(realised_pnl = sell_proceeds - (avg_cost * sold_units))
    sum(realised$realised_pnl, na.rm = TRUE)
  })

  output$total_value_ui <- renderUI({
    state <- state_data(); trade_log <- trade_data()
    if (is.null(state)) return(div(class="metric-value neutral","—"))
    total_cash <- sum(state$cash_available)
    position_value <- 0
    if (!is.null(trade_log) && nrow(trade_log) > 0) {
      buys  <- trade_log |> filter(side=="BUY") |> group_by(symbol) |>
        summarise(total_units=sum(units), avg_cost=sum(net_value)/sum(units), .groups="drop")
      sells <- trade_log |> filter(side=="SELL") |> group_by(symbol) |>
        summarise(sold_units=sum(units), .groups="drop")
      held  <- buys |> left_join(sells, by="symbol") |>
        mutate(sold_units=coalesce(sold_units,0), held_units=total_units-sold_units,
               est_value=held_units*avg_cost)
      position_value <- sum(held$est_value, na.rm=TRUE)
    }
    total <- total_cash + position_value
    div(div(class="metric-value neutral", sprintf("$%s", format(round(total,0), big.mark=","))),
        div(class="metric-label", "cash + positions (at avg cost)"))
  })

  output$unrealised_pnl_ui <- renderUI({
    div(div(class="metric-value neutral","—"), div(class="metric-label","requires live prices"))
  })

  output$realised_pnl_ui <- renderUI({
    val <- get_realised_pnl()
    cls <- if(val>0)"positive" else if(val<0)"negative" else "neutral"
    div(div(class=paste("metric-value",cls),
      sprintf("%s$%s", if(val>=0)"+" else "-", format(round(abs(val),2),big.mark=","))),
        div(class="metric-label","from closed positions"))
  })

  output$total_pnl_ui <- renderUI({
    val <- get_realised_pnl()
    cls <- if(val>0)"positive" else if(val<0)"negative" else "neutral"
    div(div(class=paste("metric-value",cls),
      sprintf("%s$%s", if(val>=0)"+" else "-", format(round(abs(val),2),big.mark=","))),
        div(class="metric-label","realised only"))
  })

  output$positions_ui <- renderUI({
    state <- state_data(); trade_log <- trade_data()
    if (is.null(state)) return(p("No state file found.", style="color:#546e7a;"))
    buys <- if (!is.null(trade_log) && nrow(trade_log) > 0) {
      trade_log |> filter(side=="BUY") |> group_by(symbol) |>
        summarise(avg_cost=sum(net_value)/sum(units), .groups="drop")
    } else data.frame(symbol=character(0), avg_cost=numeric(0))

    rows <- lapply(seq_len(nrow(state)), function(i) {
      row  <- state[i,]; sym <- row$symbol; units <- row$units_held; cash <- row$cash_available
      avg_cost <- buys$avg_cost[buys$symbol == sym]
      avg_cost <- if (length(avg_cost) > 0) avg_cost else NA
      pos_val  <- if (!is.na(avg_cost) && units > 0) units * avg_cost else 0
      div(style="display:flex;align-items:center;padding:12px 0;border-bottom:1px solid #1a2535;",
        div(style="width:120px;", span(class="etf-badge", sym)),
        div(style="flex:1;display:grid;grid-template-columns:repeat(4,1fr);gap:8px;",
          div(div(style="font-size:0.65rem;color:#546e7a;text-transform:uppercase;","Units Held"),
              div(style="font-family:'Space Mono',monospace;font-size:0.95rem;", format(units,big.mark=","))),
          div(div(style="font-size:0.65rem;color:#546e7a;text-transform:uppercase;","Avg Cost"),
              div(style="font-family:'Space Mono',monospace;font-size:0.95rem;",
                  if(!is.na(avg_cost)) sprintf("$%.2f",avg_cost) else "—")),
          div(div(style="font-size:0.65rem;color:#546e7a;text-transform:uppercase;","Position Value"),
              div(style="font-family:'Space Mono',monospace;font-size:0.95rem;",
                  if(pos_val>0) sprintf("$%s",format(round(pos_val,0),big.mark=",")) else "—")),
          div(div(style="font-size:0.65rem;color:#546e7a;text-transform:uppercase;","Cash Available"),
              div(style="font-family:'Space Mono',monospace;font-size:0.95rem;color:#4fc3f7;",
                  sprintf("$%s",format(round(cash,2),big.mark=","))))
        )
      )
    })
    div(rows)
  })

  # --- Signal Chart ---

  output$signal_chart <- renderPlotly({
    symbol    <- input$signal_symbol
    overlay   <- input$signal_overlay
    trade_log <- trade_data()
    req(symbol)

    price_df <- load_price_history(symbol)

    empty_chart <- function(msg) {
      plot_ly() |> layout(paper_bgcolor="#0a0e1a", plot_bgcolor="#0d1526",
        font=list(color="#546e7a", family="JetBrains Mono"),
        annotations=list(list(text=msg, showarrow=FALSE,
          font=list(color="#546e7a",size=14), xref="paper", yref="paper", x=0.5, y=0.5)))
    }

    if (is.null(price_df)) return(empty_chart(paste("No price history for", symbol)))

    price_df <- price_df |> mutate(date = as.Date(date))

    # Filter trades for this symbol
    sym_trades <- if (!is.null(trade_log)) trade_log |> filter(symbol == !!symbol) else data.frame()
    buys  <- if (nrow(sym_trades) > 0) sym_trades |> filter(side == "BUY")  else data.frame()
    sells <- if (nrow(sym_trades) > 0) sym_trades |> filter(side == "SELL") else data.frame()

    # Build subplot based on overlay selection
    if (overlay == "MACD-V") {

      # Calculate MACD-V
      atr_obj    <- TTR::ATR(as.matrix(price_df[, c("high","low","close")]), n = 26)
      atr_vec    <- as.numeric(atr_obj[, "atr"])
      safe_atr   <- pmax(atr_vec, 1e-8)
      macd_obj   <- TTR::MACD(price_df$close, nFast=12, nSlow=26, nSig=9, maType=TTR::EMA, percent=FALSE)
      macd_line  <- as.numeric(macd_obj[,"macd"])
      macdv_line <- (macd_line / safe_atr) * 100
      macdv_sig  <- as.numeric(TTR::EMA(macdv_line, n=9))
      hist_vals  <- macdv_line - macdv_sig
      hist_col   <- ifelse(hist_vals >= 0, "#26a69a", "#ef5350")

      p1 <- plot_ly(data=price_df, x=~date) |>
        add_lines(y=~close, name="Price", line=list(color="#4fc3f7", width=1.5)) |>
        layout(yaxis=list(title="Price (AUD)", gridcolor="#1a2535", tickprefix="$",
          linecolor="#1e2d3d", tickfont=list(color="#546e7a")))

      if (nrow(buys) > 0) {
        p1 <- p1 |> add_markers(data=buys, x=~date, y=~price, name="BUY",
          marker=list(symbol="triangle-up", size=12, color="#26a69a"),
          text=~sprintf("<b>BUY</b><br>%s<br>$%.2f<br>%s units<br>Signal: %s",
            date, price, format(units,big.mark=","), signal), hoverinfo="text")
      }
      if (nrow(sells) > 0) {
        p1 <- p1 |> add_markers(data=sells, x=~date, y=~price, name="SELL",
          marker=list(symbol="triangle-down", size=12, color="#ef5350"),
          text=~sprintf("<b>SELL</b><br>%s<br>$%.2f<br>%s units<br>Signal: %s",
            date, price, format(units,big.mark=","), signal), hoverinfo="text")
      }

      p2 <- plot_ly(x=price_df$date) |>
        add_bars(y=hist_vals, name="MACD-V Hist",
          marker=list(color=hist_col, line=list(width=0))) |>
        add_lines(y=macdv_line, name="MACD-V", line=list(color="#4fc3f7", width=1)) |>
        add_lines(y=macdv_sig,  name="Signal",  line=list(color="#ffa726", width=1, dash="dot")) |>
        layout(yaxis=list(title="MACD-V", gridcolor="#1a2535",
          linecolor="#1e2d3d", tickfont=list(color="#546e7a")))

      subplot(p1, p2, nrows=2, shareX=TRUE, heights=c(0.65, 0.35), titleY=TRUE) |>
        layout(paper_bgcolor="#0a0e1a", plot_bgcolor="#0d1526",
          font=list(color="#b0bec5", family="JetBrains Mono", size=11),
          title=list(text=paste(symbol,"— Price & MACD-V"),
            font=list(color="#4fc3f7", family="Space Mono", size=13)),
          xaxis=list(gridcolor="#1a2535", linecolor="#1e2d3d", tickfont=list(color="#546e7a")),
          legend=list(bgcolor="#111d2e", bordercolor="#1e2d3d", borderwidth=1,
            font=list(color="#b0bec5")),
          hoverlabel=list(bgcolor="#111d2e", bordercolor="#4fc3f7",
            font=list(family="JetBrains Mono", size=11)),
          margin=list(t=50, b=40, l=60, r=20))

    } else if (overlay == "RSI") {

      rsi_vals <- as.numeric(TTR::RSI(price_df$close, n=14))

      p1 <- plot_ly(data=price_df, x=~date) |>
        add_lines(y=~close, name="Price", line=list(color="#4fc3f7", width=1.5)) |>
        layout(yaxis=list(title="Price (AUD)", gridcolor="#1a2535", tickprefix="$",
          linecolor="#1e2d3d", tickfont=list(color="#546e7a")))

      if (nrow(buys) > 0) {
        p1 <- p1 |> add_markers(data=buys, x=~date, y=~price, name="BUY",
          marker=list(symbol="triangle-up", size=12, color="#26a69a"),
          text=~sprintf("<b>BUY</b><br>%s<br>$%.2f<br>%s units<br>Signal: %s",
            date, price, format(units,big.mark=","), signal), hoverinfo="text")
      }
      if (nrow(sells) > 0) {
        p1 <- p1 |> add_markers(data=sells, x=~date, y=~price, name="SELL",
          marker=list(symbol="triangle-down", size=12, color="#ef5350"),
          text=~sprintf("<b>SELL</b><br>%s<br>$%.2f<br>%s units<br>Signal: %s",
            date, price, format(units,big.mark=","), signal), hoverinfo="text")
      }

      p2 <- plot_ly(x=price_df$date) |>
        add_lines(y=rsi_vals, name="RSI", line=list(color="#4fc3f7", width=1.5)) |>
        add_lines(y=rep(70, nrow(price_df)), name="Overbought",
          line=list(color="#ef5350", width=1, dash="dash"), showlegend=FALSE) |>
        add_lines(y=rep(30, nrow(price_df)), name="Oversold",
          line=list(color="#26a69a", width=1, dash="dash"), showlegend=FALSE) |>
        layout(yaxis=list(title="RSI", range=c(0,100), gridcolor="#1a2535",
          linecolor="#1e2d3d", tickfont=list(color="#546e7a")))

      subplot(p1, p2, nrows=2, shareX=TRUE, heights=c(0.65, 0.35), titleY=TRUE) |>
        layout(paper_bgcolor="#0a0e1a", plot_bgcolor="#0d1526",
          font=list(color="#b0bec5", family="JetBrains Mono", size=11),
          title=list(text=paste(symbol,"— Price & RSI"),
            font=list(color="#4fc3f7", family="Space Mono", size=13)),
          xaxis=list(gridcolor="#1a2535", linecolor="#1e2d3d", tickfont=list(color="#546e7a")),
          legend=list(bgcolor="#111d2e", bordercolor="#1e2d3d", borderwidth=1,
            font=list(color="#b0bec5")),
          hoverlabel=list(bgcolor="#111d2e", bordercolor="#4fc3f7",
            font=list(family="JetBrains Mono", size=11)),
          margin=list(t=50, b=40, l=60, r=20))

    } else {

      # No overlay — price only
      p <- plot_ly(data=price_df, x=~date) |>
        add_lines(y=~close, name="Price", line=list(color="#4fc3f7", width=1.5))

      if (nrow(buys) > 0) {
        p <- p |> add_markers(data=buys, x=~date, y=~price, name="BUY",
          marker=list(symbol="triangle-up", size=12, color="#26a69a"),
          text=~sprintf("<b>BUY</b><br>%s<br>$%.2f<br>%s units<br>Signal: %s",
            date, price, format(units,big.mark=","), signal), hoverinfo="text")
      }
      if (nrow(sells) > 0) {
        p <- p |> add_markers(data=sells, x=~date, y=~price, name="SELL",
          marker=list(symbol="triangle-down", size=12, color="#ef5350"),
          text=~sprintf("<b>SELL</b><br>%s<br>$%.2f<br>%s units<br>Signal: %s",
            date, price, format(units,big.mark=","), signal), hoverinfo="text")
      }

      p |> layout(paper_bgcolor="#0a0e1a", plot_bgcolor="#0d1526",
        font=list(color="#b0bec5", family="JetBrains Mono", size=11),
        title=list(text=paste(symbol,"— Price"),
          font=list(color="#4fc3f7", family="Space Mono", size=13)),
        xaxis=list(title="", gridcolor="#1a2535", linecolor="#1e2d3d",
          tickfont=list(color="#546e7a")),
        yaxis=list(title="Price (AUD)", gridcolor="#1a2535", linecolor="#1e2d3d",
          tickprefix="$", tickfont=list(color="#546e7a")),
        legend=list(bgcolor="#111d2e", bordercolor="#1e2d3d", borderwidth=1,
          font=list(color="#b0bec5")),
        hoverlabel=list(bgcolor="#111d2e", bordercolor="#4fc3f7",
          font=list(family="JetBrains Mono", size=11)),
        margin=list(t=50, b=40, l=60, r=20))
    }
  })

  # --- Signal History Table ---

  output$signals_table <- renderDT({
    trade_log <- trade_data()
    if (is.null(trade_log)) return(datatable(data.frame()))
    display <- trade_log |> arrange(desc(date)) |>
      mutate(price=sprintf("$%.2f",price), fee=sprintf("$%.2f",fee),
             units=format(units,big.mark=",")) |>
      select(date, symbol, side, units, price, fee, signal)
    datatable(display, options=list(pageLength=15, dom="frtip"), rownames=FALSE,
      colnames=c("Date","Symbol","Side","Units","Price","Fee","Signal")) |>
      formatStyle("side", color=styleEqual(c("BUY","SELL"),c("#26a69a","#ef5350")),
        fontWeight="bold") |>
      formatStyle("signal", color=styleEqual(
        c("buy","sell","hold","stop_loss"), c("#26a69a","#ef5350","#546e7a","#ffa726")))
  })

  # --- Trade History Table ---

  output$trades_table <- renderDT({
    trade_log <- trade_data()
    if (is.null(trade_log)) return(datatable(data.frame(Message="No trades yet."), rownames=FALSE))
    display <- trade_log |> arrange(desc(date)) |>
      mutate(price=sprintf("$%.2f",price), fee=sprintf("$%.2f",fee),
             value=sprintf("$%s",format(round(value,0),big.mark=",")),
             net_value=sprintf("$%s",format(round(net_value,0),big.mark=",")),
             units=format(units,big.mark=","))
    datatable(display, options=list(pageLength=20, dom="frtip"), rownames=FALSE,
      colnames=c("Date","Symbol","Side","Units","Price","Fee","Value","Net Value","Signal")) |>
      formatStyle("side", color=styleEqual(c("BUY","SELL"),c("#26a69a","#ef5350")), fontWeight="bold")
  })

  # --- Price Fetch Status ---

  output$fetch_status_ui <- renderUI({
    input$refresh_all; input$refresh_logs
    status <- check_fetch_status()
    css_class <- paste0("status-", status$status)
    icon_sym  <- if (status$status == "success") "✓" else if (status$status == "error") "✗" else "?"
    div(
      div(style="margin-bottom:8px;",
        span(class=paste("status-badge", css_class),
          paste(icon_sym, toupper(status$status)))),
      div(class="last-updated", status$label)
    )
  })

  # --- Log Viewer ---

  output$log_contents <- renderUI({
    req(input$trade_log_select)
    contents <- read_log_file(input$trade_log_select, "quant_trader")
    HTML(colour_log_lines(contents))
  })

  observeEvent(input$trade_log_select, {
    contents <- read_log_file(input$trade_log_select, "quant_trader")
    output$log_contents <- renderUI({ HTML(colour_log_lines(contents)) })
  })

  observeEvent(input$fetch_log_select, {
    contents <- read_log_file(input$fetch_log_select, "quant_fetch_price_hist")
    output$log_contents <- renderUI({ HTML(colour_log_lines(contents)) })
  })
}

# =============================================================================
# Run
# =============================================================================

shinyApp(ui = ui, server = server)
