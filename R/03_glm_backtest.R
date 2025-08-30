# R/03_glm_backtest.R
# Purpose: train a logistic regression on ret1s direction and backtest

library(data.table)
library(xts)
library(PerformanceAnalytics)

#' Fit GLM and backtest a mean-reversion classifier
#'
#' @param feat_dt data.table with columns ts, mid, spread, vol5, imbalance_l1, ret1s
#' @param trade_size integer shares per signal (default 100L)
#' @return list(model, rets_xts)
#' @export
run_glm_backtest <- function(feat_dt, trade_size = 100L) {
  # Copy input
  dt <- copy(feat_dt)
  
  # Diagnostics
  cat("Starting rows: ", nrow(dt), "\n")
  cat(" NAs mid:", sum(is.na(dt$mid)),
      " spread:", sum(is.na(dt$spread)),
      " vol5:", sum(is.na(dt$vol5)),
      " imbal:", sum(is.na(dt$imbalance_l1)),
      " ret1s:", sum(is.na(dt$ret1s)), "\n")
  
  # Drop rows with any NA in core columns
  keep <- complete.cases(dt[, .(mid, spread, vol5, imbalance_l1, ret1s)])
  dt <- dt[keep]
  cat("Rows after dropping NAs:", nrow(dt), "\n")
  if (nrow(dt) < 2) {
    stop("Need at least 2 complete rows to run GLM backtest.")
  }
  
  # Build response for next-second up/down
  dt[, up := as.integer(shift(ret1s, type = "lead") > 0)]
  dt <- dt[!is.na(up)]
  if (nrow(dt) < 2) {
    stop("Need at least 2 non-NA response rows to run GLM backtest.")
  }
  
  # Fit logistic regression
  glm_fit <- glm(up ~ mid + spread + vol5 + imbalance_l1,
                 data = dt, family = binomial())
  
  # Predict probabilities and signals
  dt[, p_up      := predict(glm_fit, newdata = dt, type = "response")]
  dt[, signal_glm := ifelse(p_up > 0.5, 1L, -1L)]
  
  # Compute PnL and returns
  dt[, px_diff  := mid - shift(mid, fill = mid[1])]
  dt[, PnL_glm  := signal_glm * px_diff * trade_size]
  dt[, ret_glm  := PnL_glm / 1e6]
  
  # Build xts series
  rets_xts <- xts(dt$ret_glm, order.by = as.POSIXct(dt$ts))
  colnames(rets_xts) <- "GLM"
  
  # Print performance
  print(table.AnnualizedReturns(rets_xts, scale = 6.5*3600*252))
  chart.CumReturns(rets_xts,
                   main       = "GLM Cumulative Returns",
                   legend.loc = "topright")
  chart.Drawdown(rets_xts,
                 main       = "GLM Drawdowns",
                 legend.loc = "bottomright")
  
  return(list(model = glm_fit, rets = rets_xts))
}

# No top-level calls: sourcing only defines the function.