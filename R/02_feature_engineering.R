# R/02_feature_engineering.R
# Purpose: compute mid, spread, returns, vol, imbalance, bins; save features

library(data.table)
library(yaml)
library(TTR)

#' Compute features from clean LOB
#' @export
run_feature_engineering <- function(config = "config/params.yml") {
  params  <- yaml::read_yaml(config)
  in_dir  <- params$data$processed_path
  out_dir <- in_dir
  dt      <- fread(file.path(in_dir, "lob_clean.csv"))
  
  dt[, mid    := (best_bid + best_ask)/2]
  dt[, spread := best_ask - best_bid]
  dt[, ret1s  := ROC(mid, n = 1, type = "discrete")]
  dt[, vol5   := runSD(ret1s, n = 5, cumulative = FALSE)]
  # Level-1 order-book imbalance
  dt[, imbalance_l1 := (ask_size1 - bid_size1)/(ask_size1 + bid_size1)]
  
  # Impute 0 where (ask_size1 + bid_size1)==0 → NaN
  dt[is.na(imbalance_l1), imbalance_l1 := 0]
  cuts <- quantile(dt$ret1s, probs = seq(0,1, length.out=6), na.rm=TRUE)
  dt[, ret_bin := cut(ret1s, breaks = cuts, labels = paste0("B",1:5),
                      include.lowest = TRUE)]
  
  feats <- dt[!is.na(ret1s), .(ts, mid, spread, ret1s, vol5, imbalance_l1, ret_bin)]
  fwrite(feats, file.path(out_dir, "features.csv"))
  return(feats)
}

if (interactive()) run_feature_engineering()