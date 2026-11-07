# Relative Strength Index (RSI) with Wilder's smoothing
# BDA400 - Assignment 5 | May Elmady
# Base R only - no external libraries.

rsi <- function(data, period) {
  n <- length(data)

  # Initialize the RSI vector with NA values (returned as-is if data is too short)
  rsi_values <- rep(NA_real_, n)
  if (n <= period) {
    warning("Data length must be greater than the period to compute RSI; returning NA values")
    return(rsi_values)
  }

  # Calculate the differences between consecutive data points
  diff_values <- data[2:n] - data[1:(n - 1)]

  # Initialize two vectors to store the gains and losses
  gains  <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Calculate the average gains and average losses for the first 'period' elements
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period

  # Calculate RSI values using the Wilder's smoothing method
  for (i in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    if (avg_loss == 0) {
      # No losses in the window: RSI is 100 (or 50 if there was no movement at all)
      rsi_values[i] <- if (avg_gain == 0) 50 else 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}

# ---- Example (Toronto Cakes) ----
# Average price per cake (CAD) over 10 weeks:
# avg_cake_price <- c(85, 90, 87, 95, 93, 88, 97, 101, 104, 100)
# rsi(avg_cake_price, period = 5)
