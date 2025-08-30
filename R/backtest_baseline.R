# R/backtest_baseline.R
# Purpose: wrapper to run both baseline GLM and RF backtests

# load dependencies
library(data.table)

# source the two backtest functions
source("R/03_glm_backtest.R")
source("R/03_rf_backtest.R")

#' Run both GLM and RF backtests and return a combined xts of returns
#'
#' @param feature_file path to features.csv
#' @return xts with two columns: GLM and RF returns
#' @export
run_baseline_backtests <- function(feature_file = "data/processed/features.csv") {
  feats <- fread(feature_file)
  
  # run GLM and RF
  glm_out <- run_glm_backtest(feats)
  rf_out  <- run_rf_backtest(feats)
  
  # merge the two xts return series by timestamp
  rets   <- merge(glm_out$rets, rf_out$rets, join = "inner")
  return(rets)
}

# if interactive, demo
if (interactive()) {
  rets <- run_baseline_backtests()
  print(table.AnnualizedReturns(rets, scale = 6.5*3600*252))
  chart.CumReturns(rets, main="GLM vs RF Cumulative Returns")
  chart.Drawdown(rets, main="GLM vs RF Drawdowns")
}