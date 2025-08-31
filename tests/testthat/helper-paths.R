# helper_paths.R — tiny helpers used by tests
find_one <- function(name) {
  hits <- list.files(".", pattern = paste0("^", gsub("\\.", "\\\\.", name), "$"),
                     recursive = TRUE, full.names = TRUE, ignore.case = FALSE)
  if (length(hits)) hits[[1]] else NA_character_
}

read_if <- function(p) if (!is.na(p) && file.exists(p)) read.csv(p, check.names = FALSE) else NULL

has_pnl <- function(x) any(c("PnL","pnl","PnL_net","PnL_RL","PnL_glm","PnL_rf","ret") %in% names(x))
has_ts  <- function(x) "ts" %in% names(x)
