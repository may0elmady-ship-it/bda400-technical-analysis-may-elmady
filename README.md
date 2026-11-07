# BDA400 – Assignment 5: Technical Analysis using R (Development Phase)

**Student:** May Elmady · CDI College · BDA400 Data Science Tools and Techniques

Technical-analysis indicators implemented in **base R only** (no packages), following the course templates and pseudocode.

## Files
| File | Function | Returns |
|---|---|---|
| `sma.R` | `sma(data, period)` | numeric vector, length `n - period + 1` |
| `ema.R` | `ema(data, period)` | numeric vector, length `n` |
| `macd.R` | `macd(data, short_period, long_period, signal_period)` | list: `macd_line`, `signal_line`, `histogram` |
| `stdev.R` | `stdev(data)` | single number (population SD) |
| `linreg.R` | `linreg(regressionSource, regressionLength, regressionOffset)` | list: `slope`, `intercept`, `predicted_values` |
| `rsi.R` | `rsi(data, period)` | numeric vector, length `n` (first `period` values are NA) |
| `stoch_rsi.R` | `stoch_rsi(data, period, k_period, d_period)` | list: `k_line`, `d_line` |
| `crossover.R` | `crossover(arr1, arr2)` | character vector: "Up" / "Down" / "None" |
| `crossunder.R` | `crossunder(arr1, arr2)` | character vector: "None" (first), then "True" / "False" |
| `test_indicators.R` | runs the brief's examples + 23 checks | |
| `demo_stock_analysis.R` | stock data frame + indicators + buy/sell rules | |

## How to run
1. Install R (and optionally RStudio). No packages needed.
2. Set the working directory to this folder: `setwd("path/to/bda400-technical-analysis")`
3. Run the tests: `source("test_indicators.R")` → should end with `Passed: 23  Failed: 0`
4. Run the demo: `source("demo_stock_analysis.R")`, or from a terminal `Rscript demo_stock_analysis.R your_stock.csv` (CSV columns: Date, Open, High, Low, Close, Volume).

`macd.R` loads `ema.R`, and `stoch_rsi.R` loads `rsi.R` and `sma.R` automatically if they are not already loaded.

## My examples
Every script ends with a commented example using my own business, Toronto Cakes (custom cakes ordered through Instagram): weekly orders, butter prices from my supplier, follower growth, servings per cake, monthly revenue, average cake price, and wedding vs birthday orders. `test_indicators.R` runs and checks all of them.
