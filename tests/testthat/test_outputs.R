# ---- tiny helpers (self-contained; no source(), no dependencies on R/ ) ----
find_one <- function(name) {
  hits <- list.files(
    ".", pattern = paste0("^", gsub("\\.", "\\\\.", name), "$"),
    recursive = TRUE, full.names = TRUE, ignore.case = FALSE
  )
  if (length(hits)) hits[[1]] else NA_character_
}
read_if <- function(p) if (!is.na(p) && file.exists(p)) read.csv(p, check.names = FALSE) else NULL
has_pnl <- function(x) any(c("PnL","pnl","PnL_net","PnL_RL","PnL_glm","PnL_rf","ret") %in% names(x))
has_ts  <- function(x) "ts" %in% names(x)

# ---- Baseline ----
test_that("baseline outputs look sane if present", {
  p <- find_one("baseline_trades.csv")
  if (is.na(p)) testthat::skip("baseline_trades.csv not found in repo/CI")
  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})

# ---- GLM ----
test_that("GLM outputs look sane if present", {
  p <- find_one("glm_trades.csv")
  if (is.na(p)) testthat::skip("glm_trades.csv not found in repo/CI")
  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})

# ---- RF ----
test_that("RF outputs look sane if present", {
  p <- find_one("rf_trades.csv")
  if (is.na(p)) testthat::skip("rf_trades.csv not found in repo/CI")
  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})

# ---- RL ----
test_that("RL outputs look sane if present", {
  p <- find_one("rl_trades.csv")
  if (is.na(p)) testthat::skip("rl_trades.csv not found in repo/CI")
  dat <- read_if(p)
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(has_pnl(dat))
  expect_true(has_ts(dat))
})
