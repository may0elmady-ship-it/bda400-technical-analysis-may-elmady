# Crossunder - detects when arr1 crosses below arr2
# BDA400 - Assignment 5 | May Elmady
# Base R only - no external libraries.

crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- character(length(arr1))
  crossunder_signals[1] <- "None"

  # Check for crossunder signals at each data point
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }

  return(crossunder_signals)
}

# ---- Example (Toronto Cakes) ----
# Same data - when do wedding orders fall back below birthday orders?
# wedding  <- c(2, 3, 4, 7, 9, 11, 12, 8, 5)
# birthday <- c(9, 9, 8, 7, 8, 8, 9, 9, 10)
# crossunder(wedding, birthday)  # "True" = wedding season is over
