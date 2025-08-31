# tests/testthat/test_baseline.R

test_that("baseline outputs look sane if present", {
  p <- find_one("baseline_trades.csv")
  if (is.na(p)) testthat::skip("baseline_trades.csv not found in repo/CI")

  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})

test_that("backtest_baseline on toy data works (dev only)", {
  testthat::skip_on_ci()
  if (!file.exists("R/backtest_baseline.R"))
    testthat::skip("R/backtest_baseline.R not available")

  source("R/backtest_baseline.R", local = TRUE)

  ts  <- as.POSIXct("2025-01-01 09:30:00") + 0:4
  mid <- c(100, 101, 100, 102, 101)
  feat <- data.table::data.table(ts = ts, mid = mid)
  feat[, spread := 0.2]
  feat[, ret1s := c(NA, diff(mid) / mid[-5])]

  rets <- backtest_baseline(feat, initEq = 1000, trade_size = 10, max_pos = 5, stop_pct = 1)
  testthat::expect_s3_class(rets, "xts")
  testthat::expect_equal(nrow(rets), 4)
  testthat::expect_true(all(is.finite(xts::coredata(rets))))
})
