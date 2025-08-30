test_that("RL scripts parse and rl_trades.csv is sane if present", {
  # Training script should parse; we do NOT run training in CI
  expect_error(source(proj_path("R", "04_train_RL_agent.R"), local = TRUE), NA)
  # RL backtest script should parse
  expect_error(source(proj_path("R", "05_RL_backtest.R"), local = TRUE), NA)

  p <- find_one("rl_trades.csv")
  if (is.na(p)) testthat::skip("rl_trades.csv not found in repo/CI run")

  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})
