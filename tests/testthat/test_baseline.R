test_that("baseline scripts parse and baseline_trades.csv is sane if present", {
  # Source scripts (parse only; don’t run heavy backtests in CI)
  expect_error(source(proj_path("R", "backtest_baseline.R"),  local = TRUE), NA)
  expect_error(source(proj_path("R", "03_baseline_backtest.R"), local = TRUE), NA)

  # If a baseline trades file exists (committed or produced by workflow), validate it
  p <- find_one("baseline_trades.csv")
  if (is.na(p)) testthat::skip("baseline_trades.csv not found in repo/CI run")

  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})
