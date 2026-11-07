# Stochastic RSI (StochRSI) - %K and %D lines
# BDA400 - Assignment 5 | May Elmady
# Base R only - no external libraries. Depends on rsi() and sma().

if (!exists("rsi")) source("rsi.R")
if (!exists("sma")) source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI (normalize RSI between 0 and 1)
  valid <- rsi_values[!is.na(rsi_values)]
  if (length(valid) == 0) {
    k_values <- rep(NA_real_, length(rsi_values))
  } else {
    min_rsi <- min(valid)
    max_rsi <- max(valid)
    if (max_rsi == min_rsi) {
      k_values <- ifelse(is.na(rsi_values), NA_real_, 0)  # flat RSI: avoid divide by zero
    } else {
      k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
    }
  }

  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}

# ---- Example (Toronto Cakes) ----
# Weekly orders over 12 weeks:
# weekly_orders <- c(14, 17, 15, 21, 19, 24, 28, 26, 23, 27, 31, 29)
# stoch_rsi(weekly_orders, period = 5, k_period = 3, d_period = 2)
