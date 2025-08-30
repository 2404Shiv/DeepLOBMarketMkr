# R/03_rf_backtest.R
# Purpose: train a random forest on ret_bin and backtest a mean-reversion policy

library(data.table)
library(randomForest)
library(xts)
library(PerformanceAnalytics)

#' Fit RF on ret_bin state and backtest a mean-reversion strategy
#'
#' @param feat_dt data.table with columns ts, mid, spread, vol5, imbalance_l1, ret_bin
#' @param trade_size shares per action (default 100L)
#' @return list(model, rets)
#' @export
run_rf_backtest <- function(feat_dt, trade_size = 100L) {
  dt <- copy(feat_dt)
  
  # 1) drop rows missing any required column
  req_cols <- c("mid","spread","vol5","imbalance_l1","ret_bin")
  dt <- dt[complete.cases(dt[, ..req_cols])]
  cat("RF rows after dropping NAs:", nrow(dt), "\n")
  if (nrow(dt) < 10) stop("Need at least 10 rows to train RF.")
  
  # 2) prepare state/action/reward
  dt[, State     := factor(ret_bin)]
  dt[, NextState := shift(State, type="lead")]
  dt[, Reward    := ret1s]
  dt[, Action    := as.character(
    factor(sign(ret1s),
           levels = c(-1,0,1),
           labels = c("Short","Flat","Long"))
  )]
  dt <- dt[!is.na(NextState)]
  cat("RF rows after aligning NextState:", nrow(dt), "\n")
  
  # 3) train Random Forest on transitions
  rf_fit <- randomForest(
    x      = dt[, .(mid, spread, vol5, imbalance_l1)],
    y      = dt$NextState,
    ntree  = 100
  )
  
  # 4) get policy: map each State → predicted NextState → trade signal
  dt[, pred := predict(rf_fit, newdata = dt)]
  dt[, signal_rf := ifelse(pred %in% c("B1","B2"),  1L,
                           ifelse(pred %in% c("B4","B5"), -1L,  0L))]
  
  # 5) simulate PnL
  dt[, px_diff := mid - shift(mid, fill=mid[1])]
  dt[, PnL_rf  := signal_rf * px_diff * trade_size]
  dt[, ret_rf  := PnL_rf / 1e6]
  
  # 6) build xts and report
  rets_rf <- xts(dt$ret_rf, order.by = as.POSIXct(dt$ts))
  colnames(rets_rf) <- "RF"
  
  print(table.AnnualizedReturns(rets_rf, scale = 6.5*3600*252))
  chart.CumReturns(rets_rf, main="RF Cumulative Returns", legend.loc="topright")
  chart.Drawdown(rets_rf, main="RF Drawdowns", legend.loc="bottomright")
  
  return(list(model = rf_fit, rets = rets_rf))
}