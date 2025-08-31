test_that("GLM outputs look sane if present", {
  p <- find_one("glm_trades.csv")
  if (is.na(p)) testthat::skip("glm_trades.csv not found in repo/CI")
  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})
