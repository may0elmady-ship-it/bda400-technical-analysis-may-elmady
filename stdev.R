# Standard Deviation (population formula, divides by n)
# BDA400 - Assignment 5 | May Elmady
# Base R only - no external libraries.

stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- numeric(0)
  for (element in data) {
    diff_values <- c(diff_values, element - mean_value)
  }

  # Calculate the squared differences
  squared_diff <- numeric(0)
  for (element in diff_values) {
    squared_diff <- c(squared_diff, element * element)
  }

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}

# ---- Example (Toronto Cakes) ----
# Number of servings per cake for 9 orders (birthdays, baby showers, weddings):
# servings <- c(12, 20, 15, 30, 24, 40, 18, 60, 25)
# stdev(servings)   # how spread out my order sizes are
