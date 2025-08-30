# R/06_evaluation.R
# Purpose: aggregate all backtests and compare performance

# 0) Dependencies
library(data.table)
library(xts)
library(PerformanceAnalytics)

# 1) Read in each backtest’s net-return series (must all have 'ts' and 'ret_net' or equivalent)
base_dt <- fread("output/results/baseline_trades.csv")
glm_dt  <- fread("output/results/glm_trades.csv")
rf_dt   <- fread("output/results/rf_trades.csv")
rl_dt   <- fread("output/results/RL_trades.csv")  # adjust path/name if different

# 2) Turn into xts (using the same POSIXct timestamps)
base_xt <- xts(base_dt$ret_net, order.by = as.POSIXct(base_dt$ts))
glm_xt  <- xts(glm_dt$ret_glm,  order.by = as.POSIXct(glm_dt$ts))
rf_xt   <- xts(rf_dt$ret_rf,    order.by = as.POSIXct(rf_dt$ts))
rl_xt   <- xts(rl_dt$ret_rl,    order.by = as.POSIXct(rl_dt$ts))

# 3) Annualize and tabulate
secs_per_year <- 6.5 * 3600 * 252  # trading seconds per year
ann <- rbind(
  Baseline = table.AnnualizedReturns(base_xt, scale = secs_per_year),
  GLM      = table.AnnualizedReturns(glm_xt,  scale = secs_per_year),
  RF       = table.AnnualizedReturns(rf_xt,   scale = secs_per_year),
  RL       = table.AnnualizedReturns(rl_xt,   scale = secs_per_year)
)
print(ann)

# 4) Plot cumulative returns and drawdowns
#    Combine into one multi‐series object:
all_rets <- cbind(Baseline = base_xt,
                  GLM      = glm_xt,
                  RF       = rf_xt,
                  RL       = rl_xt)

# 4a) Cumulative returns
chart.CumReturns(
  all_rets,
  main       = "Cumulative Returns: All Strategies",
  legend.loc = "topleft"
)

# 4b) Drawdowns
chart.Drawdown(
  all_rets,
  main       = "Drawdowns: All Strategies",
  legend.loc = "bottomleft"
)

# 5) Barplot of just the Sharpe ratios
sharpe_vals <- as.numeric(ann[,"Annualized Sharpe (Rf=0%)"])
barplot(
  sharpe_vals,
  names.arg = rownames(ann),
  main      = "Annualized Sharpe Comparison",
  ylab      = "Sharpe"
)

# no top-level return; this script is meant to be sourced, not called as a function