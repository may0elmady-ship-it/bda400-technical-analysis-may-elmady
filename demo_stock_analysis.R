# Demo - builds a stock data frame and applies the indicators + trading rules.
# Usage:  Rscript demo_stock_analysis.R            (uses the built-in sample)
#         Rscript demo_stock_analysis.R AAPL.csv   (CSV with Date,Open,High,Low,Close,Volume)
# BDA400 - Assignment 5 | May Elmady

for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
            "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")) source(f)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  stock <- read.csv(args[1], stringsAsFactors = FALSE)
  stock$Date <- as.Date(stock$Date)
} else {
  # Illustrative sample only (not real market data)
  close <- c(50.0, 50.8, 51.5, 51.1, 52.3, 53.0, 52.4, 51.8, 52.9, 54.1,
             55.0, 54.6, 53.9, 54.8, 56.2, 57.0, 56.4, 55.7, 56.9, 58.3,
             59.1, 58.4, 57.6, 56.8, 57.9, 59.4, 60.2, 59.7, 61.0, 62.1)
  stock <- data.frame(
    Date   = seq(as.Date("2026-09-01"), by = "day", length.out = length(close)),
    Open   = close - 0.3, High = close + 0.6, Low = close - 0.7,
    Close  = close, Volume = 1e6 + seq_along(close) * 1e4
  )
}

# Filter by time period (example: keep everything from a start date onwards)
stock <- stock[stock$Date >= min(stock$Date), ]
n <- nrow(stock)

# Indicators aligned to the data frame (shorter vectors padded with NA at the start)
pad <- function(x) c(rep(NA, n - length(x)), x)
stock$SMA5  <- pad(sma(stock$Close, 5))
stock$SMA10 <- pad(sma(stock$Close, 10))
stock$EMA5  <- ema(stock$Close, 5)
stock$RSI14 <- rsi(stock$Close, 14)
m <- macd(stock$Close, 12, 26, 9)
stock$MACD <- m$macd_line; stock$Signal <- m$signal_line

# Trading rules: buy when fast SMA crosses above slow SMA, sell when it crosses below
ok <- !is.na(stock$SMA10)
stock$Crossover  <- NA; stock$Crossunder <- NA
stock$Crossover[ok]  <- crossover(stock$SMA5[ok], stock$SMA10[ok])
stock$Crossunder[ok] <- crossunder(stock$SMA5[ok], stock$SMA10[ok])
stock$Signal_Rule <- ifelse(stock$Crossover %in% "Up", "BUY",
                     ifelse(stock$Crossunder %in% "True", "SELL", ""))

print(stock[, c("Date", "Close", "SMA5", "SMA10", "RSI14", "Signal_Rule")])
cat("\nVolatility (stdev of Close):", round(stdev(stock$Close), 4), "\n")
lr <- linreg(stock$Close, 10, 0)
cat("Trend slope over last window:", round(lr$slope, 4), "per day\n")

# Filter stocks/days by indicator criteria: RSI below 30 = oversold
cat("\nOversold days (RSI < 30):", sum(stock$RSI14 < 30, na.rm = TRUE), "\n")
