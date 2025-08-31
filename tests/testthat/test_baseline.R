test_that("baseline scripts parse and baseline_trades.csv is sane if present", {
  # Absolute paths into R/ (no setwd, no ../../ anywhere)
  p_parse1 <- file.path(.root, "R", "backtest_baseline.R")
  p_parse2 <- file.path(.root, "R", "03_baseline_backtest.R")

  expect_true(file.exists(p_parse1), info = p_parse1)
  expect_true(file.exists(p_parse2), info = p_parse2)

  expect_error(source(p_parse1, local = TRUE), NA)
  expect_error(source(p_parse2, local = TRUE), NA)

  # Validate output CSV if present
  p_out <- find_one("baseline_trades.csv")
  if (is.na(p_out)) testthat::skip("baseline_trades.csv not found in repo/CI run")

  dat <- read_if(p_out)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})
