# R/02_tune_rl.R
# Tune tabular Q-learning hyperparameters by grid search

library(data.table)
library(ReinforcementLearning)
library(xts)
library(PerformanceAnalytics)

# ─── 1. Hyperparameter grid ──────────────────────────────
alphas   <- c(0.05, 0.1, 0.2)
gammas   <- c(0.9,  0.95, 0.99)
epsilons <- c(0.1,  0.2,  0.3)
grid     <- expand.grid(alpha = alphas,
                        gamma = gammas,
                        epsilon = epsilons)
grid$Sharpe <- NA_real_

# ─── 2. Load data & build rl_base ─────────────────────────
# 2.1 features + baseline trades
feat   <- fread("data/processed/features.csv")[!is.na(ret1s)]
trades <- fread("output/results/baseline_trades.csv")

# 2.2 merge on timestamp (bring in signal & ret)
dt_base <- merge(
  feat[,   .(ts, ret1s, ret_bin)],
  trades[, .(ts, signal, ret)],
  by = "ts"
)

# 2.3 create Action column (Short/Flat/Long)
dt_base[, Action := factor(
  signal,
  levels = c(-1, 0, 1),
  labels = c("Short", "Flat", "Long")
)]
dt_base[, Action := as.character(Action)]

# 2.4 build NextState
dt_base[, NextBin := shift(ret_bin, type = "lead")]

# 2.5 assemble rl_base with ts + four columns
rl_base <- dt_base[!is.na(NextBin), .(
  ts        = ts,
  State     = as.character(ret_bin),
  Action    = Action,
  Reward    = ret,                   # use equity return
  NextState = as.character(NextBin)
)]

# ─── 3. Grid search ────────────────────────────────────────
for (i in seq_len(nrow(grid))) {
  alpha   <- grid$alpha[i]
  gamma   <- grid$gamma[i]
  epsilon <- grid$epsilon[i]
  
  # 3.1 Train Q-learner
  model <- ReinforcementLearning(
    data    = rl_base,
    s       = "State",
    a       = "Action",
    r       = "Reward",
    s_new   = "NextState",
    control = list(alpha = alpha,
                   gamma = gamma,
                   epsilon = epsilon)
  )
  
  # 3.2 Apply policy: atomic named vector State→Action
  policy_vec   <- model$Policy
  actions_pred <- policy_vec[rl_base$State]
  
  # 3.3 Compute RL returns (+/- Reward)
  rets_rl <- ifelse(actions_pred == "Long", 
                    rl_base$Reward,
                    ifelse(actions_pred == "Short",
                           -rl_base$Reward, 
                           0))
  for (i in seq_len(nrow(grid))) {
    # … after building rets_xts …
    
    # 3.4 Compute annualized Sharpe
    sharpe_i <- as.numeric(
      PerformanceAnalytics::SharpeRatio.annualized(
        rets_xts,
        scale     = scale_secs,
        geometric = FALSE
      )
    )
    
    # 3.5 Store it
    grid$Sharpe[i] <- sharpe_i
    
    cat(sprintf("[%2d/%2d] α=%.2f γ=%.2f ε=%.2f → Sharpe=%.2f\n",
                i, nrow(grid), alpha, gamma, epsilon, sharpe_i))
  }
# ─── 4. Show best results ─────────────────────────────────
best <- grid[order(-Sharpe), ]
print(best[1:5, ])