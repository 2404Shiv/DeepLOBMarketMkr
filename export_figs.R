# export_figs.R  — write README figures from trade CSVs
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2); library(tibble)
})

# Locate trade files (root or output/results)
find_one <- function(fname) {
  hits <- list.files(".", pattern = paste0("^", gsub("\\.", "\\\\.", fname), "$"),
                     recursive = TRUE, full.names = TRUE)
  if (length(hits)) hits[[1]] else NA_character_
}

p_baseline <- find_one("baseline_trades.csv")
p_glm      <- find_one("glm_trades.csv")
p_rf       <- find_one("rf_trades.csv")
p_rl       <- find_one("rl_trades.csv")

read_if <- function(p) if (!is.na(p) && file.exists(p)) readr::read_csv(p, show_col_types = FALSE) else NULL

baseline <- read_if(p_baseline)
glm      <- read_if(p_glm)
rf       <- read_if(p_rf)
rl       <- read_if(p_rl)

norm_trades <- function(df, name){
  if (is.null(df)) return(NULL)
  dt <- as_tibble(df)
  if ("ts" %in% names(dt)) dt$ts <- as.POSIXct(dt$ts, tz = "UTC")
  preferred <- c("PnL_net","PnL","pnl","PnL_RL","PnL_glm","PnL_rf","PnL_baseline","PnL_rl",
                 "ret","ret_glm","ret_rf","ret_baseline","ret_rl")
  pnl_col <- intersect(preferred, names(dt))
  if (length(pnl_col) > 1) pnl_col <- pnl_col[1]
  if (!length(pnl_col)) {
    hits <- grep("^(PnL|pnl|ret)", names(dt), value = TRUE)
    if (length(hits)) pnl_col <- hits[1] else return(NULL)
  }
  dt |> transmute(ts = if ("ts" %in% names(dt)) ts else row_number(),
                  pnl = .data[[pnl_col]], strat = name)
}

trades <- bind_rows(
  norm_trades(baseline, "Baseline"),
  norm_trades(glm,      "GLM"),
  norm_trades(rf,       "RF"),
  norm_trades(rl,       "RL")
)

stopifnot(nrow(trades) > 0)

perf <- trades |>
  arrange(ts) |>
  group_by(strat) |>
  mutate(pnl_cum = cumsum(replace_na(pnl, 0))) |>
  ungroup()

# Ensure output dirs
fig_dir <- "figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("output/results", recursive = TRUE, showWarnings = FALSE)

# PnL plot
p_pnl <- ggplot(perf, aes(ts, pnl_cum, color = strat)) +
  geom_line() + labs(title = "Cumulative PnL by Strategy", x = NULL, y = "Cumulative PnL")
ggsave(file.path(fig_dir, "pnl_by_strategy.png"), p_pnl, width = 9, height = 4.5, dpi = 150)

# Drawdown plot
dd <- perf |>
  group_by(strat) |>
  arrange(ts) |>
  mutate(peak = cummax(pnl_cum), drawdown = pnl_cum - peak)

p_dd <- ggplot(dd, aes(ts, drawdown, color = strat)) +
  geom_line() + labs(title = "Drawdown by Strategy", x = NULL, y = "Drawdown")
ggsave(file.path(fig_dir, "drawdown_by_strategy.png"), p_dd, width = 9, height = 4.5, dpi = 150)

# Performance summary CSV (for table in README if wanted)
perf_tbl <- perf |>
  group_by(strat) |>
  summarise(
    n_trades = dplyr::n(),
    pnl_total = sum(pnl, na.rm = TRUE),
    pnl_mean  = mean(pnl, na.rm = TRUE),
    pnl_sd    = sd(pnl, na.rm = TRUE),
    sharpe    = ifelse(pnl_sd > 0, pnl_mean / pnl_sd * sqrt(252), NA_real_),
    hit_rate  = mean(pnl > 0, na.rm = TRUE)
  ) |>
  arrange(desc(pnl_total))

readr::write_csv(perf_tbl, "output/results/perf_summary.csv")

cat("Wrote:\n - figures/pnl_by_strategy.png\n - figures/drawdown_by_strategy.png\n - output/results/perf_summary.csv\n")
