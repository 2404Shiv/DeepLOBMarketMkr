# Minimal helpers used by tests
find_one <- function(fname) {
  hits <- list.files(".", pattern = paste0("^", gsub("\\.", "\\\\.", fname), "$"),
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
