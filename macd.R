# Moving Average Convergence Divergence (MACD)
# BDA400 - Assignment 5 | May Elmady
# Base R only - no external libraries. Depends on ema().

if (!exists("ema")) source("ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema  <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(macd_line = macd_line,
                 signal_line = signal_line,
                 histogram = histogram)
  return(result)
}

# ---- Example (Toronto Cakes) ----
# Daily Instagram follower count (thousands) during a wedding-season promo:
# followers <- c(40.0, 40.2, 40.5, 40.9, 41.4, 41.8, 42.1)
# macd(followers, short_period = 3, long_period = 5, signal_period = 2)
