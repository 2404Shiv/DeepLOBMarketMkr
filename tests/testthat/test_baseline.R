# tests/testthat/test_baseline.R

# 0. Make sure our function is loaded
source("../../R/backtest_baseline.R")

library(data.table)
library(testthat)
library(xts)

test_that("backtest_baseline on toy data produces expected returns", {
  # build toy feature data: 5 timestamps, mid prices 100,101,100,102,101
  ts <- as.POSIXct("2025-01-01 09:30:00") + 0:4
  mid <- c(100, 101, 100, 102, 101)
  feat <- data.table(ts = ts, mid = mid)
  feat[, spread := 0.2]
  feat[, ret1s := c(NA, diff(mid)/mid[-5])]
  
  rets <- backtest_baseline(feat, initEq=1000, trade_size=10, max_pos=5, stop_pct=1)
  expect_s3_class(rets, "xts")
  expect_equal(nrow(rets), 4)   # one less than feat rows
  expect_true(all(is.finite(coredata(rets))))  
})


library(data.table)
library(testthat)
library(xts)

test_that("backtest_baseline on toy data produces expected returns", {
  # build toy feature data: 5 timestamps, mid prices 100,101,100,102,101
  ts <- as.POSIXct("2025-01-01 09:30:00") + 0:4
  mid <- c(100, 101, 100, 102, 101)
  feat <- data.table(ts = ts, mid = mid)
  feat[, spread := 0.2]
  feat[, ret1s := c(NA, diff(mid)/mid[-5])]
  
  rets <- backtest_baseline(feat, initEq=1000, trade_size=10, max_pos=5, stop_pct=1)
  expect_s3_class(rets, "xts")
  expect_equal(nrow(rets), 4)   # one less than feat rows
  # at least numeric and finite
  expect_true(all(is.finite(coredata(rets))))
})
