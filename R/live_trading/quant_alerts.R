# =============================================================================
# quant_alerts.R
# Email alerting for live trading system
#
# Sends email alerts via Gmail SMTP when critical failures occur in
# quant_trader.R or quant_fetch_price_hist.R.
#
# Prerequisites:
#   - GMAIL_APP_PASSWORD must be set as environment variable before sourcing
#     (fetched from GCP Secret Manager in start.sh / fetch_prices.sh)
#   - sendmailR package must be installed
#
# Usage:
#   source("R/live_trading/quant_alerts.R")
#   send_alert(subject = "...", body = "...")
# =============================================================================

suppressPackageStartupMessages({
  library(sendmailR)
})

# Configuration ----------------------------------------------------------------

ALERT_FROM  <- Sys.getenv("ALERT_EMAIL")
ALERT_TO    <- Sys.getenv("ALERT_EMAIL")
SMTP_SERVER <- "smtp.gmail.com"
SMTP_PORT   <- 587

# Public API -------------------------------------------------------------------

#' Send an email alert via Gmail SMTP
#'
#' Reads the Gmail App Password from the GMAIL_APP_PASSWORD environment
#' variable (set by start.sh / fetch_prices.sh from GCP Secret Manager).
#' Failures are logged as warnings rather than errors — alerting should
#' never cause the main script to crash.
#'
#' @param subject Character string — email subject line
#' @param body Character string — plain text email body
#' @return Invisibly returns TRUE on success, FALSE on failure
send_alert <- function(subject, body) {
  tryCatch({

    password <- Sys.getenv("GMAIL_APP_PASSWORD")

    if (nchar(password) == 0) {
      warning("GMAIL_APP_PASSWORD not set — cannot send alert.")
      return(invisible(FALSE))
    }

    sendmail(
      from    = ALERT_FROM,
      to      = ALERT_TO,
      subject = subject,
      msg     = body,
      control = list(
        smtpServer = SMTP_SERVER,
        smtpPort   = SMTP_PORT,
        smtpAuth   = "LOGIN",
        smtpUser   = ALERT_FROM,
        smtpPasswd = password
      )
    )

    message(sprintf("Alert sent: %s", subject))
    invisible(TRUE)

  }, error = function(e) {
    warning(sprintf("Failed to send alert '%s': %s", subject, e$message))
    invisible(FALSE)
  })
}
