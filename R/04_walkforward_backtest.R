# R/04_walkforward_backtest.R
# Purpose: walk‐forward backtest of baseline strategy, computing Sharpe in rolling windows

library(data.table)
library(xts)
library(PerformanceAnalytics)

#’ Walk-forward backtest of baseline returns
#’
#’ @param trades_file Path to CSV of baseline trades (‘output/results/baseline_trades.csv’)
#’ @param train_secs  Training window length, in seconds (default 30s)
#’ @param test_secs   Testing window length, in seconds (default 15s)
#’ @param step_secs   Step size to roll windows, in seconds (default 15s)
#’ @export
walkforward_baseline <- function(trades_file = "output/results/baseline_trades.csv",
                                 train_secs  = 30,
                                 test_secs   = 15,
                                 step_secs   = 15) {
  # 1) Read in baseline trade log with net returns
  trades   <- fread(trades_file)
  rets_xts <- xts(trades$ret_net, order.by = as.POSIXct(trades$ts))
  
  # 2) Ensure we have enough data
  total_secs <- length(rets_xts)
  if (total_secs < train_secs + test_secs) {
    stop("Not enough data for a ",
         train_secs, "s train + ", test_secs, "s test window.")
  }
  
  # 3) Build the sequence of window start indices
  starts <- seq(
    from = 1,
    to   = total_secs - train_secs - test_secs,
    by   = step_secs
  )
  
  # 4) Prepare results table
  wf_results <- data.table(
    window_start = as.POSIXct(character()),
    window_end   = as.POSIXct(character()),
    Sharpe       = numeric()
  )
  
  # 5) Loop over each window
  for (i in seq_along(starts)) {
    s         <- starts[i]
    train_idx <- s:(s + train_secs - 1)
    test_idx  <- (s + train_secs):(s + train_secs + test_secs - 1)
    
    test_xt <- rets_xts[test_idx]
    
    if (length(test_xt) < 2) next  # skip if too few points
    
    sr <- SharpeRatio.annualized(
      test_xt,
      scale     = 6.5 * 3600 * 252,  # per‐second scale
      geometric = FALSE
    )
    
    ws <- index(test_xt)[1]
    we <- index(test_xt)[length(test_xt)]
    
    wf_results <- rbind(
      wf_results,
      list(window_start = ws,
           window_end   = we,
           Sharpe       = as.numeric(sr))
    )
  }
  
  # 6) Print and plot
  print(wf_results)
  plot(
    wf_results$window_end,
    wf_results$Sharpe,
    type   = "b",
    xlab   = "Test Window End",
    ylab   = "Annualized Sharpe",
    main   = "Walk-Forward Sharpe Over Time"
  )
  abline(
    h   = mean(wf_results$Sharpe, na.rm = TRUE),
    col = "blue",
    lty = 2
  )
  
  return(wf_results)
}

# End of file — sourcing this defines walkforward_baseline()
