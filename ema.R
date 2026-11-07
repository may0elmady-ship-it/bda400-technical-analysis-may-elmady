# Exponential Moving Average (EMA)
# BDA400 - Assignment 5 | May Elmady
# Base R only - no external libraries.

ema <- function(data, period) {
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))

  # Loop through the data array
  for (i in seq_along(data)) {
    if (i == 1) {
      # Calculate EMA for the first data point
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}

# ---- Example (Toronto Cakes) ----
# Butter price per kg (CAD) paid to my supplier over 9 purchases:
# butter_price <- c(9.80, 10.10, 10.40, 10.20, 10.90, 11.30, 11.10, 11.60, 11.90)
# ema(butter_price, period = 3)    # reacts faster to recent price jumps than SMA
