# R/01_data_prep.R
# Purpose: read raw LOB CSVs, clean, filter trading hours, save clean LOB

library(data.table)
library(yaml)

#' Clean raw LOB files into one table
#' @param config path to params.yml
#' @export
run_data_prep <- function(config = "config/params.yml") {
  params   <- yaml::read_yaml(config)
  raw_dir  <- params$data$raw_path
  out_dir  <- params$data$processed_path
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  files <- list.files(raw_dir, "\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("No CSVs in ", raw_dir)
  
  dt_list <- lapply(files, function(f) {
    dt <- fread(f)
    setnames(dt, tolower(names(dt)))
    
    # ensure essential cols exist; fill missing sizes with 0
    required <- c("timestamp","best_bid","best_ask","bid_size1","ask_size1")
    miss     <- setdiff(required, names(dt))
    if (length(miss) > 0) {
      warning("Missing columns: ", paste(miss, collapse = ", "),
              "; filling with zero.")
      for (col in miss) dt[[col]] <- 0L
    }
    
    # now parse timestamp
    dt[, ts := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")]
    dt[, timestamp := NULL]
    return(dt)
  })
  lob <- rbindlist(dt_list)
  lob <- lob[format(ts, "%H:%M:%S") >= "09:30:00" & format(ts, "%H:%M:%S") <= "16:00:00"]
  setkey(lob, ts)
  fwrite(lob, file.path(out_dir, "lob_clean.csv"))
  return(lob)
}

# If run interactively:
if (interactive()) run_data_prep()