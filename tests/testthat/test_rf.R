test_that("RF scripts are syntactically valid; rf_trades.csv sane if present", {
  safe_parse(file.path("R", "03_rf_backtest.R"))

  p <- find_one("rf_trades.csv")
  if (is.na(p)) testthat::skip("rf_trades.csv not found in repo/CI run")

  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})
