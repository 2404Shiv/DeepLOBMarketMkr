# R/04_train_RL_agent.R

library(data.table)
library(yaml)
library(ReinforcementLearning)

train_rl_agent <- function(config    = "config/params.yml",
                           feat_file = "data/processed/features.csv") {
  rl_cfg <- yaml::read_yaml(config)$strategy$rl
  eps    <- min(max(rl_cfg$epsilon_start, 0), 0.99)
  
  df <- fread(feat_file)[!is.na(ret1s)]
  dt <- df[, .(
    State     = as.character(ret_bin),
    Action    = as.character(ret_bin),
    Reward    = ret1s,
    NextState = shift(ret_bin, type="lead")
  )][!is.na(NextState)]
  
  if (nrow(dt) < 10) {
    stop("Need ≥10 transitions; got ", nrow(dt))
  }
  
  # *** Single Q-learning call using the clipped eps ***
  model <- ReinforcementLearning(
    data    = dt,
    s       = "State",
    a       = "Action",
    r       = "Reward",
    s_new   = "NextState",
    control = list(
        alpha   = rl_cfg$learning_rate,
        gamma   = rl_cfg$gamma,
        epsilon = 0.9,      # hard-coded safe value
        iter    = 100
      )
  )
  
  return(model)
}