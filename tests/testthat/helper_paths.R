## tests/testthat/helper_paths.R
## Robust helpers used by all tests

# Project root (CI runs tests from tests/testthat, so ".." is the repo root)
.root <- normalizePath("..", winslash = "/", mustWork = TRUE)

# Read a CSV if it exists, else return NULL
read_if <- function(path) {
  if (!is.null(path) && !is.na(path) && file.exists(path)) {
    suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  } else {
    NULL
  }
}

# Find a single file anywhere under the repo (first match) or NA if not found
find_one <- function(filename) {
  hits <- list.files(.root, pattern = paste0("^", gsub("\\.", "\\\\.", filename), "$"),
                     recursive = TRUE, full.names = TRUE)
  if (length(hits)) normalizePath(hits[[1]], winslash = "/", mustWork = FALSE) else NA_character_
}

# Minimal structure checks used in tests
has_pnl <- function(df) {
  any(tolower(names(df)) %in% c("pnl", "pnl_net", "pnl_rl", "pnl_glm", "pnl_rf", "ret", "ret1s"))
}
has_ts <- function(df) {
  "ts" %in% names(df)
}

.root <- normalizePath(".", winslash = "/", mustWork = TRUE)

find_one <- function(fname) {
  hits <- list.files(.root, pattern = paste0("^", gsub("\\.", "\\\\.", fname), "$"),
                     recursive = TRUE, full.names = TRUE)
  if (length(hits)) hits[[1]] else NA_character_
}

read_if <- function(p) if (!is.na(p) && file.exists(p)) read.csv(p) else NULL
has_pnl <- function(x) any(tolower(names(x)) %in% c("pnl","pnl_net","pnl_rl","pnl_glm","pnl_rf"))
has_ts  <- function(x) any(tolower(names(x)) == "ts")

safe_parse <- function(path) {
  testthat::expect_true(file.exists(path), info = paste("Missing:", path))
  testthat::expect_error(parse(file = path, keep.source = FALSE), NA)
}
