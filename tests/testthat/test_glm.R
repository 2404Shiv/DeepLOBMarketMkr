test_that("GLM scripts are syntactically valid; glm_trades.csv sane if present", {
  safe_parse(file.path(.root, "R", "03_glm_backtest.R"))

  p <- find_one("glm_trades.csv")
  if (is.na(p)) testthat::skip("glm_trades.csv not found in repo/CI run")

  dat <- read_if(p)
  testthat::expect_true(is.data.frame(dat))
  testthat::expect_true(nrow(dat) > 0)
  testthat::expect_true(has_pnl(dat))
  testthat::expect_true(has_ts(dat))
})
