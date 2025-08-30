# helper_paths.R — loaded before tests

# Project root from tests/testthat/
.root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")

# Build a path under project root
proj_path <- function(...) file.path(.root, ...)

# Find a file anywhere under repo (first match)
find_one <- function(fname) {
  hits <- list.files(.root, pattern = paste0("^", gsub("\\.", "\\\\.", fname), "$"),
                     recursive = TRUE, full.names = TRUE)
  if (length(hits)) hits[[1]] else NA_character_
}

# Read CSV if exists
read_if <- function(path) {
  if (!is.na(path) && file.exists(path)) {
    suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  } else NULL
}

# Does a data.frame have any plausible PnL/ret column?
has_pnl <- function(x) {
  any(c("PnL_net","PnL","pnl","PnL_RL","PnL_glm","PnL_rf","PnL_baseline",
        "ret","ret_glm","ret_rf","ret_baseline","ret_rl") %in% names(x))
}

# Does it have a time index?
has_ts <- function(x) any(c("ts","time","datetime","timestamp") %in% names(x))
