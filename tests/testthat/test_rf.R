test_that("RF scripts parse and rf_trades.csv is sane if present", {
  old <- setwd(.root)
  on.exit(setwd(old), add = TRUE)

  expect_error(source("R/03_rf_backtest.R", local = TRUE), NA)

  p <- find_one("rf_trades.csv")
  if (is.na(p)) testthat::skip("rf_trades.csv not found in repo/CI run")

  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})
