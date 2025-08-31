test_that("baseline scripts parse and baseline_trades.csv is sane if present", {
  # Use project root so internal source('R/...') resolves in CI
  old <- setwd(.root)
  on.exit(setwd(old), add = TRUE)

  # Parse checks (do NOT run the backtest loops in CI)
  expect_error(source("R/backtest_baseline.R",    local = TRUE), NA)
  expect_error(source("R/03_baseline_backtest.R", local = TRUE), NA)

  # Validate trades file if present (skip if not produced in this run)
  p <- find_one("baseline_trades.csv")
  if (is.na(p)) testthat::skip("baseline_trades.csv not found in repo/CI run")

  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})
