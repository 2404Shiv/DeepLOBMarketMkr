# 05_RL_backtest.R
# Purpose: apply the learned tabular policy to features and record PnL

library(data.table)
library(ReinforcementLearning)
library(xts)
library(PerformanceAnalytics)

# 1. Load baseline trades (for base returns)
base_trades <- fread("output/results/baseline_trades.csv")

# 2. Load features & merge on ts
feat   <- fread("data/processed/features.csv")[!is.na(ret1s)]
dt     <- merge(
  feat[, .(ts, mid, spread, ret1s)],
  base_trades[, .(ts, mid, spread, ret1s, PnL, ret)],
  by = c("ts","mid","spread","ret1s"),
  all.x = TRUE
)

# 3. Build discrete State exactly as in training
dt[, State := cut(
  ret1s,
  breaks = c(-Inf, -0.0005, 0.0005, Inf),
  labels = c("Down","Flat","Up")
)]
dt[, State := as.character(State)]

# 4. Lookup Action from your trained model

# First, extract the policy data.frame and coerce to character
policy_df <- model$Policy
# Assume policy_df has columns State and Action
policy_vec <- setNames(
  as.character(policy_df$Action),   # values
  as.character(policy_df$State)     # names
)

# Now do a vectorized lookup that can never return a data.frame
dt[, Action := policy_vec[State]]

# If any States were unseen and gave NA, you can fill them flat:
dt[is.na(Action), Action := "Flat"]

# 5. Compute RL PnL
trade_size <- 100
dt[, px_diff := mid - shift(mid, fill = first(mid))]
dt[, PnL_RL := fifelse(
  Action == "Long",  px_diff * trade_size,
  fifelse(Action == "Short", -px_diff * trade_size, 0)
)]

# 6. Equity returns
initial_capital <- 1e6
dt[, ret_RL := PnL_RL / initial_capital]

# 7. Save RL trade log
dir.create("output/results", recursive = TRUE, showWarnings = FALSE)
fwrite(
  dt[, .(ts, mid, spread, ret1s, State, Action, px_diff, PnL_RL, ret_RL)],
  "output/results/rl_trades.csv"
)

# 8. Build xts series & compare performance
rets_base <- xts(base_trades$ret, order.by = as.POSIXct(base_trades$ts))
rets_rl   <- xts(dt$ret_RL,    order.by = as.POSIXct(dt$ts))

# 9. Annualized tables (per-second → scale by 6.5*3600*252)
scale_secs <- 6.5 * 3600 * 252
perf_table <- rbind(
  Baseline = table.AnnualizedReturns(rets_base, scale = scale_secs),
  RL       = table.AnnualizedReturns(rets_rl,   scale = scale_secs)
)
print(perf_table)

# 10. Combined performance chart
charts.PerformanceSummary(
  cbind(Baseline = rets_base, RL = rets_rl),
  main = "Baseline vs. RL Performance"
)