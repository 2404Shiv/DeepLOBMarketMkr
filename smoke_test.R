# smoke_test.R — tolerant check for DeepLOBMarketMkr

req <- c("readr","dplyr","stringr")
to_i <- setdiff(req, rownames(installed.packages()))
if (length(to_i)) install.packages(to_i, quiet = TRUE)

suppressPackageStartupMessages({ library(readr); library(dplyr); library(stringr) })

find_file <- function(filename) {
  hits <- list.files(".", pattern = paste0("^", gsub("\\.", "\\\\.", filename), "$"),
                     recursive = TRUE, full.names = TRUE)
  if (length(hits)) hits[[1]] else NA_character_
}

must_exist <- function(fn) {
  p <- find_file(fn); if (is.na(p)) stop("Missing: ", fn); p
}

pick_pnl_col <- function(cols) {
  # preferred names first, then fall back to any PnL*/pnl*/ret* column
  preferred <- c("PnL_net","PnL","pnl","PnL_RL",
                 "PnL_glm","PnL_rf","PnL_baseline","PnL_rl",
                 "ret","ret_glm","ret_rf","ret_baseline","ret_rl")
  cand <- intersect(preferred, cols)
  if (length(cand)) return(cand[1])
  # regex fallback
  rx <- which(str_detect(cols, "^(PnL|pnl|ret)"))
  if (length(rx)) return(cols[rx[1]])
  NA_character_
}

trade_ok <- function(p) {
  df <- readr::read_csv(p, show_col_types = FALSE, n_max = 5)
  pnl_col <- pick_pnl_col(names(df))
  if (is.na(pnl_col)) {
    stop("No PnL/ret column in: ", p, " (cols: ", paste(names(df), collapse=", "), ")")
  }
  invisible(TRUE)
}

files <- c("baseline_trades.csv","glm_trades.csv","rf_trades.csv","rl_trades.csv")
paths <- sapply(files, must_exist)
invisible(lapply(paths, trade_ok))

cat("Smoke test passed ✅ — all trade files have a usable PnL/ret column\n")