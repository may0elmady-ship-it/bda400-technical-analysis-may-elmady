# Crossover - detects when arr1 crosses above ("Up") or below ("Down") arr2
# BDA400 - Assignment 5 | May Elmady
# Base R only - no external libraries.

crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossover signals
  crossover_signals <- character(length(arr1))
  crossover_signals[1] <- "None"

  # Check for crossovers at each data point
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}

# ---- Example (Toronto Cakes) ----
# Wedding orders vs birthday orders per month - when do weddings overtake birthdays?
# wedding  <- c(2, 3, 4, 7, 9, 11, 12, 8, 5)
# birthday <- c(9, 9, 8, 7, 8, 8, 9, 9, 10)
# crossover(wedding, birthday)   # "Up" = wedding season starts
