test_that("baseline scripts are syntactically valid; baseline_trades.csv sane if present", {
  safe_parse(file.path(.root, "R", "backtest_baseline.R"))
  safe_parse(file.path(.root, "R", "03_baseline_backtest.R"))

  p <- find_one("baseline_trades.csv")
  if (is.na(p)) testthat::skip("baseline_trades.csv not found in repo/CI run")

  dat <- read_if(p)
  testthat::expect_true(is.data.frame(dat))
  testthat::expect_true(nrow(dat) > 0)
  testthat::expect_true(has_pnl(dat))
  testthat::expect_true(has_ts(dat))
})
