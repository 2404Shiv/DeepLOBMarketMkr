test_that("GLM scripts parse and glm_trades.csv is sane if present", {
  # Use project root so internal source('R/...') resolves in CI
  old <- setwd(.root)
  on.exit(setwd(old), add = TRUE)

  # Parse only
  expect_error(source("R/03_glm_backtest.R", local = TRUE), NA)

  # Validate CSV if present
  p <- find_one("glm_trades.csv")
  if (is.na(p)) testthat::skip("glm_trades.csv not found in repo/CI run")

  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})
