# 03_baseline_backtest.R
# Purpose: simple mean-reversion backtest with transaction costs, net returns, 
#          explicit annualization, and clean performance plots

library(data.table)
library(yaml)
library(TTR)
library(xts)
library(PerformanceAnalytics)

# ─── 0) Precompute scale for per-second data ────────────────────
scale_secs <- 6.5 * 3600 * 252  # 6.5 hours/day, 3600 sec/hr, 252 days

# ─── 1) Read config & features ──────────────────────────────────
params <- yaml::read_yaml("config/params.yml")
proc   <- params$data$processed_path
feat   <- fread(file.path(proc, "features.csv"))

# Drop any NA in ret1s, reset order
feat <- feat[!is.na(ret1s)]

# ─── 2) Setup capital & sizing ─────────────────────────────────
initEq     <- 1e6     # $1M starting capital
trade_size <- 100     # shares per signal

# ─── 3) Generate mean-reversion signal ────────────────────────
#    If mid moved up last second → go SHORT (-1)
#    If mid moved down → go LONG (+1), else FLAT (0)
feat[, signal := fifelse(ret1s >  0, -1L,
                         fifelse(ret1s <  0,  1L, 0L))]

# Shift so we trade on next timestamp
feat[, signal := shift(signal, fill = 0L)]

feat[, position := 0L]   # new column

# define cap
max_pos <- 200          # e.g. max 200 shares net
# Update and cap running position
feat[, position := {
  prev <- shift(position, fill = 0L)
  desire <- prev + signal
  pmin(pmax(desire, -max_pos), max_pos)
}]
# Raw PnL based on held position
feat[, PnL := position * px_diff]

# ─── 4) Compute raw PnL & apply costs ──────────────────────────
feat[, px_diff   := mid - shift(mid, fill = first(mid)) ]
feat[, PnL       := signal * px_diff * trade_size         ]

# A) Slippage: 0.05% of trade value
feat[, slip_cost := abs(px_diff) * 0.0005 * trade_size    ]

# B) Commission: $0.005 per share whenever trading
feat[, comm      := fifelse(signal != 0L, 0.005 * trade_size, 0) ]

# C) Net PnL after costs
feat[, PnL_net   := PnL - slip_cost - comm                ]
# Stop‐loss: if position loses >1% of capital in one bar, flatten to 0 next
stop_pct <- 0.01   # 1%

feat[, stop := fifelse(
  PnL_net / (abs(position) * mid) < -stop_pct,
  TRUE, FALSE
)]
feat[, position := ifelse(shift(stop, fill = FALSE), 0L, position)]


# D) Net return on initial capital
feat[, ret_net   := PnL_net / initEq                      ]

# ─── 5) Save trade log ───────────────────────────────────────
dir.create("output/results", recursive = TRUE, showWarnings = FALSE)
fwrite(
  feat[, .(ts, mid, spread, ret1s, signal, px_diff, PnL, slip_cost, comm, PnL_net, ret_net)],
  file.path("output/results", "baseline_trades.csv")
)

# ─── 6) Performance metrics & plots ──────────────────────────

# Build & name xts series from net returns
rets <- xts(feat$ret_net, order.by = as.POSIXct(feat$ts))
colnames(rets) <- "Baseline"

# Annualized table
print(
  PerformanceAnalytics::table.AnnualizedReturns(
    rets,
    scale = scale_secs
  )
)

# Cumulative returns plot
chart.CumReturns(
  rets,
  main       = "Baseline Net Strategy – Cumulative Returns",
  legend.loc = "topleft"
)

# Drawdowns plot
chart.Drawdown(
  rets,
  main       = "Baseline Net Strategy – Drawdowns",
  legend.loc = "bottomleft"
)