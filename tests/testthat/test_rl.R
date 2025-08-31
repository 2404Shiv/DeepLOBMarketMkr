test_that("RL scripts parse and rl_trades.csv is sane if present", {
  # Parse only; do NOT train in CI
  p_train <- file.path(.root, "R", "04_train_RL_agent.R")
  p_btest <- file.path(.root, "R", "05_RL_backtest.R")

  expect_true(file.exists(p_train), info = p_train)
  expect_true(file.exists(p_btest), info = p_btest)

  expect_error(source(p_train, local = TRUE), NA)
  expect_error(source(p_btest, local = TRUE), NA)

  p_out <- find_one("rl_trades.csv")
  if (is.na(p_out)) testthat::skip("rl_trades.csv not found in repo/CI run")

  dat <- read_if(p_out)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})
