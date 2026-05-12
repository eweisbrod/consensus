# ==============================================================================
# 014-portfolio-returns.R
#
# Purpose:
#   Build Table 7 (Abnormal Returns to FDP Miss-and-Beat Portfolios) by
#   pulling forward CRSP returns for each Miss-and-Beat firm-quarter and
#   computing equal-weighted buy-and-hold market-adjusted returns for
#   long-beat / short-miss portfolios across three windows ([0,+1],
#   [+1,+30], [+2,+30]).
#
# Inputs (from DATA_DIR):
#   2025_12_16_fdp_firm_qtr_regdata.parquet  (output of 012; the FDP-firm-
#                                            quarter Miss-and-Beat sample)
#
# Inputs (from WRDS):
#   crsp.dsi                                 (trading-day calendar)
#   crsp.dsf                                 (daily stock returns)
#
# Outputs (to DATA_DIR):
#   wrds_crsp_returns.parquet                (cached daily returns for
#                                            -2 to +30 trading days around
#                                            each announcement; cache-aware,
#                                            skip if exists)
#
# Outputs (to OUTPUT_DIR):
#   table-07-panel-a.csv                     (5 FDPs)
#   table-07-panel-b.csv                     (5 FDP Avg Quality ranks)
#   table-07-panel-c.csv                     (5 FDPs x High/Low quality)
#
# Notes:
#   - Reads DATA_DIR and OUTPUT_DIR from .env via the dotenv package.
#   - WRDS credentials read via the keyring package.
#   - Cumulative returns: BHAR = cumprod(1+ret) - cumprod(1+vwretd) over
#     the relevant window. Profit = +BHAR for beats, -BHAR for misses.
#   - Panel C "High" portfolios = FDP Avg Quality in {0.75, 1.00};
#     "Low" portfolios = FDP Avg Quality in {0.00, 0.25}.
#   - T-statistics from feols with two-way clustering (permno, anndate).
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(fixest)
library(glue)
library(arrow)
library(DBI)
library(RPostgres)
library(dbplyr)
library(dotenv)

load_dot_env()
data_path   <- Sys.getenv("DATA_DIR")
output_path <- Sys.getenv("OUTPUT_DIR")

source("src/utils.R")  # provides trading_day_window()

setFixest_notes(FALSE)

# Load the Miss-and-Beat FDP-firm-quarter sample, restricted to FDPs whose
# surprise unequivocally signals a beat or miss (mbe_2 != 0).
fdp_data <- read_parquet(glue("{data_path}/2025_12_16_fdp_firm_qtr_regdata.parquet")) |>
  filter(mbe_2 != 0) |>
  select(permno, best_anndats_adj, fdp_num, rank_avg_quality, mbe_2) |>
  mutate(
    fdp_num = unclass(fdp_num),
    mbe_2   = unclass(mbe_2)
  )

message(glue("Miss-and-Beat sample (mbe_2 != 0): ",
             "{nrow(fdp_data)} FDP-firm-quarter rows, ",
             "{n_distinct(paste(fdp_data$permno, fdp_data$best_anndats_adj))} ",
             "unique announcements"))


# Pull forward CRSP returns from WRDS (cache-aware) ----------------------------

returns_cache <- glue("{data_path}/wrds_crsp_returns.parquet")

if (file.exists(returns_cache)) {
  message(glue("Loading cached CRSP returns from {returns_cache}"))
  crsp_rets <- read_parquet(returns_cache)
} else {
  message("Connecting to WRDS to pull daily returns...")
  wrds <- dbConnect(
    Postgres(),
    host     = "wrds-pgdata.wharton.upenn.edu",
    port     = 9737,
    user     = keyring::key_get("WRDS_user"),
    password = keyring::key_get("WRDS_pw"),
    sslmode  = "require",
    dbname   = "wrds"
  )

  trading_calendar <- tbl(wrds, in_schema("crsp", "dsi")) |>
    distinct(date) |>
    arrange(date) |>
    collect() |>
    mutate(td = row_number())

  firm_qtrs <- fdp_data |>
    select(permno, best_anndats_adj) |>
    distinct()

  t_dates <- trading_day_window(
    firm_qtrs, trading_calendar,
    "best_anndats_adj", -2, 30
  )

  crsp_rets <- copy_inline(wrds, t_dates) |>
    inner_join(
      tbl(wrds, in_schema("crsp", "dsf")) |> select(permno, date, ret),
      by = c("permno", "date")
    ) |>
    inner_join(
      tbl(wrds, in_schema("crsp", "dsi")) |> select(date, vwretd),
      by = "date"
    ) |>
    collect()

  dbDisconnect(wrds)
  message(glue("Retrieved {nrow(crsp_rets)} daily return rows; caching..."))
  write_parquet(crsp_rets, returns_cache)
}


# Compute cumulative returns over the 3 paper windows --------------------------

cumulative_bhar <- function(rets, begin, end) {
  rets |>
    filter(offset >= begin, offset <= end) |>
    group_by(permno, best_anndats_adj) |>
    arrange(permno, best_anndats_adj, date) |>
    mutate(
      cum_ret   = cumprod(1 + ret) - 1,
      cum_vwret = cumprod(1 + vwretd) - 1,
      bhar      = cum_ret - cum_vwret
    ) |>
    ungroup() |>
    filter(offset == max(offset))  # keep only the endpoint row per event
}

cum_01  <- cumulative_bhar(crsp_rets, 0, 1)
cum_130 <- cumulative_bhar(crsp_rets, 1, 30)
cum_230 <- cumulative_bhar(crsp_rets, 2, 30)


# Build profit datasets: long beats, short misses, scaled to percent ----------

build_profit <- function(cum, fdp) {
  cum |>
    inner_join(fdp, by = c("permno", "best_anndats_adj")) |>
    mutate(profit = if_else(mbe_2 == 1, bhar, -bhar) * 100) |>
    filter(!is.na(profit))
}

d_01  <- build_profit(cum_01,  fdp_data)
d_130 <- build_profit(cum_130, fdp_data)
d_230 <- build_profit(cum_230, fdp_data)


# Helper: portfolio mean profit + clustered t-stat across 3 windows -----------

portfolio_stats <- function(s01, s130, s230) {
  cl <- ~ permno + best_anndats_adj
  r01  <- feols(profit ~ 1, data = s01,  cluster = cl)
  r130 <- feols(profit ~ 1, data = s130, cluster = cl)
  r230 <- feols(profit ~ 1, data = s230, cluster = cl)
  c(
    n        = nrow(s01),
    ret_0_1  = coef(r01)[[1]],  t_0_1  = coeftable(r01)[1, "t value"],
    ret_1_30 = coef(r130)[[1]], t_1_30 = coeftable(r130)[1, "t value"],
    ret_2_30 = coef(r230)[[1]], t_2_30 = coeftable(r230)[1, "t value"]
  )
}


# Panel A: portfolios by FDP identity -----------------------------------------

fdp_names <- c("IBES", "Zacks", "CIQ", "Bloomberg", "FactSet")
# Paper presents Panel A in the order IBES, CIQ, Zacks, BB, FSET (= 1,3,2,4,5).
panel_a_order <- c(1, 3, 2, 4, 5)

panel_a <- map_dfr(panel_a_order, function(fdp) {
  s <- portfolio_stats(
    d_01  |> filter(fdp_num == fdp),
    d_130 |> filter(fdp_num == fdp),
    d_230 |> filter(fdp_num == fdp)
  )
  tibble(
    fdp      = fdp_names[fdp],
    n        = s[["n"]],
    ret_0_1  = s[["ret_0_1"]],  t_0_1  = s[["t_0_1"]],
    ret_1_30 = s[["ret_1_30"]], t_1_30 = s[["t_1_30"]],
    ret_2_30 = s[["ret_2_30"]], t_2_30 = s[["t_2_30"]]
  )
})

write.csv(panel_a, glue("{output_path}/table-07-panel-a.csv"),
          row.names = FALSE)
message(glue("    ✓ Wrote Table 7 Panel A: {output_path}/table-07-panel-a.csv"))


# Panel B: portfolios by FDP Avg Quality rank ---------------------------------

panel_b <- map_dfr(c(1.00, 0.75, 0.50, 0.25, 0.00), function(q) {
  s <- portfolio_stats(
    d_01  |> filter(rank_avg_quality == q),
    d_130 |> filter(rank_avg_quality == q),
    d_230 |> filter(rank_avg_quality == q)
  )
  tibble(
    rank_avg_quality = q,
    n                = s[["n"]],
    ret_0_1          = s[["ret_0_1"]],  t_0_1  = s[["t_0_1"]],
    ret_1_30         = s[["ret_1_30"]], t_1_30 = s[["t_1_30"]],
    ret_2_30         = s[["ret_2_30"]], t_2_30 = s[["t_2_30"]]
  )
})

write.csv(panel_b, glue("{output_path}/table-07-panel-b.csv"),
          row.names = FALSE)
message(glue("    ✓ Wrote Table 7 Panel B: {output_path}/table-07-panel-b.csv"))


# Panel C: 5 FDPs x {High = top-2 ranks, Low = bottom-2 ranks} ----------------

panel_c <- map_dfr(panel_a_order, function(fdp) {
  high_filter <- function(d) d |>
    filter(fdp_num == fdp, rank_avg_quality %in% c(0.75, 1.00))
  low_filter  <- function(d) d |>
    filter(fdp_num == fdp, rank_avg_quality %in% c(0.00, 0.25))

  s_high <- portfolio_stats(high_filter(d_01),
                            high_filter(d_130),
                            high_filter(d_230))
  s_low  <- portfolio_stats(low_filter(d_01),
                            low_filter(d_130),
                            low_filter(d_230))

  bind_rows(
    tibble(fdp = fdp_names[fdp], quality = "High",
           n = s_high[["n"]],
           ret_0_1  = s_high[["ret_0_1"]],  t_0_1  = s_high[["t_0_1"]],
           ret_1_30 = s_high[["ret_1_30"]], t_1_30 = s_high[["t_1_30"]],
           ret_2_30 = s_high[["ret_2_30"]], t_2_30 = s_high[["t_2_30"]]),
    tibble(fdp = fdp_names[fdp], quality = "Low",
           n = s_low[["n"]],
           ret_0_1  = s_low[["ret_0_1"]],  t_0_1  = s_low[["t_0_1"]],
           ret_1_30 = s_low[["ret_1_30"]], t_1_30 = s_low[["t_1_30"]],
           ret_2_30 = s_low[["ret_2_30"]], t_2_30 = s_low[["t_2_30"]])
  )
})

write.csv(panel_c, glue("{output_path}/table-07-panel-c.csv"),
          row.names = FALSE)
message(glue("    ✓ Wrote Table 7 Panel C: {output_path}/table-07-panel-c.csv"))

message("\n=== Script completed successfully ===")
