# R/07_window_sensitivity.R
# Purpose: explore how walk‐forward Sharpe depends on train/test window lengths

library(data.table)
library(xts)
library(PerformanceAnalytics)
library(ggplot2)

# 1) Read baseline trades and build xts of net returns
base_dt  <- fread("output/results/baseline_trades.csv")
rets_xts <- xts(base_dt$ret_net, order.by = as.POSIXct(base_dt$ts))

# 2) Define grid of windows (train & test in seconds)
train_secs <- c(10, 30, 60, 120)   # e.g. 10s, 30s, 1m, 2m
test_secs  <- c(5, 15, 30, 60)     # e.g. 5s, 15s, 30s, 1m

grid <- CJ(train = train_secs, test = test_secs)[
  , step := test  # roll every 'test' seconds
]

# helper: compute average Sharpe for one train/test pair
avg_sharpe <- function(train, test, step, rets_xts, scale_secs) {
  all_secs  <- as.numeric(index(rets_xts))
  start_sec <- all_secs[1]
  end_sec   <- all_secs[length(all_secs)]
  max_start <- end_sec - train - test
  
  # if we can’t fit one window, return NA
  if (max_start <= start_sec) return(NA_real_)
  
  # sequence of window‐start times
  starts <- seq(start_sec, max_start, by = step)
  
  sharps <- vapply(starts, function(s0) {
    train_end <- as.POSIXct(s0 + train, origin = "1970-01-01")
    test_end  <- as.POSIXct(s0 + train + test, origin = "1970-01-01")
    # slice test returns (strictly after train_end)
    test_xt <- rets_xts[paste0(train_end + 1, "/", test_end)]
    if (length(test_xt) < 2) return(NA_real_)
    as.numeric(
      SharpeRatio.annualized(test_xt, scale = scale_secs, geometric = FALSE)
    )
  }, numeric(1))
  
  mean(sharps, na.rm = TRUE)
}

# 3) Sweep the grid
scale_secs <- 6.5 * 3600 * 252
grid[, Sharpe := mapply(
  avg_sharpe,
  train, test, step,
  MoreArgs = list(rets_xts = rets_xts, scale_secs = scale_secs)
)]

# 4) Plot heatmap
ggplot(grid, aes(x = factor(train), y = factor(test), fill = Sharpe)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "A", na.value = "grey90") +
  labs(
    title = "Avg. Walk-Forward Sharpe for Baseline Strategy",
    x     = "Training Window (sec)",
    y     = "Test Window (sec)",
    fill  = "Avg Sharpe"
  ) +
  theme_minimal(base_size = 14)