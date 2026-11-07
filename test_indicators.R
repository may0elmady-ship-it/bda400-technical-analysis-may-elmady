# Test file - BDA400 Assignment 5 | May Elmady
# Runs each indicator on (1) the brief's sample data and (2) my own
# Toronto Cakes business data, and checks results against hand-calculated values.
# Run from this folder:  Rscript test_indicators.R   or   source("test_indicators.R")

for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
            "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")) source(f)

passed <- 0; failed <- 0
check <- function(name, actual, expected, tol = 1e-6) {
  ok <- length(actual) == length(expected) &&
        all(abs(actual - expected) < tol | (is.na(actual) & is.na(expected)))
  if (isTRUE(ok)) { passed <<- passed + 1; cat("PASS:", name, "\n") }
  else { failed <<- failed + 1; cat("FAIL:", name, "\n"); print(actual) }
}
check_equal <- function(name, actual, expected) {
  if (identical(actual, expected)) { passed <<- passed + 1; cat("PASS:", name, "\n") }
  else { failed <<- failed + 1; cat("FAIL:", name, "\n"); print(actual) }
}
expect_error <- function(name, expr) {
  r <- tryCatch({ expr; FALSE }, error = function(e) TRUE)
  check_equal(name, r, TRUE)
}

# ---------------- Toronto Cakes data ----------------
weekly_orders  <- c(14, 17, 15, 21, 19, 24, 28, 26, 23)          # Instagram orders per week
butter_price   <- c(9.80, 10.10, 10.40, 10.20, 10.90, 11.30, 11.10, 11.60, 11.90)  # CAD/kg
followers      <- c(40.0, 40.2, 40.5, 40.9, 41.4, 41.8, 42.1)     # thousands
servings       <- c(12, 20, 15, 30, 24, 40, 18, 60, 25)          # servings per cake
revenue        <- c(32, 35, 34, 39, 42, 45, 47, 51)              # monthly revenue, CAD hundreds
avg_cake_price <- c(85, 90, 87, 95, 93, 88, 97, 101, 104, 100)   # CAD per cake, weekly
wedding        <- c(2, 3, 4, 7, 9, 11, 12, 8, 5)                 # wedding orders per month
birthday       <- c(9, 9, 8, 7, 8, 8, 9, 9, 10)                  # birthday orders per month

brief <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)                   # sample data from the brief

cat("\n--- SMA: 3-week average of weekly orders ---\n")
print(sma(weekly_orders, period = 3))
check("sma weekly orders", sma(weekly_orders, 3),
      c(46/3, 53/3, 55/3, 64/3, 71/3, 78/3, 77/3))
check("sma brief example", sma(brief, 3), c(37/3, 47/3, 53/3, 20, 65/3, 71/3, 70/3))
expect_error("sma error when fewer weeks than period", sma(c(14, 17), 3))

cat("\n--- EMA: butter price trend ---\n")
print(round(ema(butter_price, period = 3), 4))
check("ema starts at first butter price", ema(butter_price, 3)[1], 9.80)
check("ema 2nd value (10.10-9.80)*0.5+9.80", ema(butter_price, 3)[2], 9.95)
check("ema 3rd value (10.40-9.95)*0.5+9.95", ema(butter_price, 3)[3], 10.175)

cat("\n--- MACD: follower growth during promo ---\n")
m <- macd(followers, short_period = 3, long_period = 5, signal_period = 2)
print(m)
check("macd line = short ema - long ema", m$macd_line, ema(followers, 3) - ema(followers, 5))
check("histogram = macd - signal", m$histogram, m$macd_line - m$signal_line)
check_equal("macd positive = bullish follower momentum", all(m$macd_line[-1] > 0), TRUE)

cat("\n--- STDEV: spread of servings per cake ---\n")
print(stdev(servings))
check("stdev servings (population formula)",
      stdev(servings), sqrt(sum((servings - sum(servings)/9)^2) / 9))
check("stdev of identical orders is 0", stdev(c(20, 20, 20)), 0)

cat("\n--- LINREG: revenue trend ---\n")
lr <- linreg(revenue, regressionLength = 6, regressionOffset = 0)
print(lr)
check("linreg slope on perfect line", linreg(c(2, 4, 6, 8, 10, 12), 4, 0)$slope, 2)
check_equal("revenue trend is growing", lr$slope > 0, TRUE)
expect_error("linreg error: length > months", linreg(revenue, 20, 0))
expect_error("linreg error: offset >= length", linreg(revenue, 3, 3))

cat("\n--- RSI: average cake price ---\n")
r <- rsi(avg_cake_price, period = 5)
print(r)
check("rsi first 'period' values are NA", r[1:5], rep(NA_real_, 5))
check_equal("rsi values between 0 and 100", all(r[6:10] >= 0 & r[6:10] <= 100), TRUE)
check("rsi of prices that only go up is 100", rsi(1:10, 3)[4:10], rep(100, 7))

cat("\n--- STOCH RSI: weekly orders ---\n")
orders12 <- c(weekly_orders, 27, 31, 29)
s <- stoch_rsi(orders12, period = 5, k_period = 3, d_period = 2)
print(s)
k <- s$k_line[!is.na(s$k_line)]
check_equal("stoch %K within 0-1", all(k >= 0 & k <= 1), TRUE)
print(suppressWarnings(stoch_rsi(c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62), 14, 3, 3)))
# ^ brief example: period 14 > 10 points, so RSI and StochRSI are NA

cat("\n--- CROSSOVER / CROSSUNDER: wedding vs birthday season ---\n")
print(crossover(wedding, birthday))
print(crossunder(wedding, birthday))
check_equal("weddings overtake birthdays in month 5", crossover(wedding, birthday),
            c("None", "None", "None", "None", "Up", "None", "None", "Down", "None"))
check_equal("weddings fall below birthdays in month 8", crossunder(wedding, birthday),
            c("None", "False", "False", "False", "False", "False", "False", "True", "False"))
expect_error("crossover length mismatch", crossover(1:3, 1:4))
expect_error("crossunder length mismatch", crossunder(1:3, 1:4))

cat("\n========================\n")
cat("Passed:", passed, " Failed:", failed, "\n")
