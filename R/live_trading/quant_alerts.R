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
#   - ALERT_EMAIL must be set as environment variable before sourcing
#   - curl must be available on the system (standard on Linux)
#
# Usage:
#   source("R/live_trading/quant_alerts.R")
#   send_alert(subject = "...", body = "...")
# =============================================================================

# Public API -------------------------------------------------------------------

#' Send an email alert via Gmail SMTP using system curl
#'
#' Reads GMAIL_APP_PASSWORD and ALERT_EMAIL from environment variables
#' (set by start.sh / fetch_prices.sh from GCP Secret Manager).
#' Uses system curl with STARTTLS for Gmail SMTP compatibility.
#' Failures are logged as warnings rather than errors — alerting should
#' never cause the main script to crash.
#'
#' @param subject Character string — email subject line
#' @param body Character string — plain text email body
#' @return Invisibly returns TRUE on success, FALSE on failure
send_alert <- function(subject, body) {
  tryCatch({

    password <- Sys.getenv("GMAIL_APP_PASSWORD")
    email    <- Sys.getenv("ALERT_EMAIL")

    if (nchar(password) == 0) {
      warning("GMAIL_APP_PASSWORD not set — cannot send alert.")
      return(invisible(FALSE))
    }

    if (nchar(email) == 0) {
      warning("ALERT_EMAIL not set — cannot send alert.")
      return(invisible(FALSE))
    }

    # Write email body to temp file
    tmp <- tempfile(fileext = ".txt")
    writeLines(c(
      sprintf("From: %s", email),
      sprintf("To: %s", email),
      sprintf("Subject: %s", subject),
      "",
      body
    ), tmp)

    # Send via curl with STARTTLS
    cmd <- sprintf(
      'curl --silent --ssl-reqd --url "smtps://smtp.gmail.com:465" --user "%s:%s" --mail-from "%s" --mail-rcpt "%s" --upload-file "%s"',
      email, password, email, email, tmp
    )

    result <- system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    unlink(tmp)

    if (result == 0) {
      message(sprintf("Alert sent: %s", subject))
      invisible(TRUE)
    } else {
      warning(sprintf("Failed to send alert '%s': curl exited with code %d", subject, result))
      invisible(FALSE)
    }

  }, error = function(e) {
    warning(sprintf("Failed to send alert '%s': %s", subject, e$message))
    invisible(FALSE)
  })
}
