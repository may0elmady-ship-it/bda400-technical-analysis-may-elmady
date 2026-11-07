# Simple Moving Average (SMA)
# BDA400 - Assignment 5 | May Elmady
# Base R only - no external libraries.

sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  sma_values <- numeric(0)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    mean_value <- sum(current_window) / period
    sma_values[i] <- mean_value
  }

  return(sma_values)
}

# ---- Example (Toronto Cakes) ----
# Weekly custom-cake orders from Instagram DMs over 9 weeks:
# weekly_orders <- c(14, 17, 15, 21, 19, 24, 28, 26, 23)
# sma(weekly_orders, period = 3)   # 3-week average smooths out slow / busy weeks
