test_that("GLM scripts parse and glm_trades.csv is sane if present", {
  p_parse <- file.path(.root, "R", "03_glm_backtest.R")

  expect_true(file.exists(p_parse), info = p_parse)
  expect_error(source(p_parse, local = TRUE), NA)

  p_out <- find_one("glm_trades.csv")
  if (is.na(p_out)) testthat::skip("glm_trades.csv not found in repo/CI run")

  dat <- read_if(p_out)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})
