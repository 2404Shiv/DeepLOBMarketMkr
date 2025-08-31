test_that("RL scripts are syntactically valid; rl_trades.csv sane if present", {
  safe_parse(file.path(.root, "R", "04_train_RL_agent.R"))
  safe_parse(file.path(.root, "R", "05_RL_backtest.R"))

  p <- find_one("rl_trades.csv")
  if (is.na(p)) testthat::skip("rl_trades.csv not found in repo/CI run")

  dat <- read_if(p)
  testthat::expect_true(is.data.frame(dat))
  testthat::expect_true(nrow(dat) > 0)
  testthat::expect_true(has_pnl(dat))
  testthat::expect_true(has_ts(dat))
})
