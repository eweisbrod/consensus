# ==============================================================================
# 012-create-firm-qtr-fdp-sample.R
#
# Purpose:
#   Reshape the firm-quarter regression sample (output of 011) into a
#   long-format FDP-firm-quarter dataset, where each row is a single
#   forecast-data-provider observation. This is the "all-5-providers"
#   stack used for FDP-vs-FDP comparisons in the regression analysis.
#
# Inputs (from DATA_DIR):
#   2025_12_16_firm_qtr_regdata.parquet      (output of 011)
#
# Outputs (to DATA_DIR):
#   2025_12_16_fdp_firm_qtr_regdata_all5.parquet/.dta
#                                            (long-format stack with all 5
#                                            FDPs)
#   2025_12_16_fdp_firm_qtr_regdata.parquet/.dta
#                                            (analysis-restricted version)
#
# Outputs (to OUTPUT_DIR):
#   table-01-panel-a.csv                     (appends Miss and Beat row to
#                                            the panel A cascade written by
#                                            011)
#
# Notes:
#   - Reads DATA_DIR and OUTPUT_DIR from .env via the dotenv package.
#   - Pure reshape -- no WRDS, no networking, no utility helpers.
#   - Output filenames preserve the 2025_12_16 date suffix from Jessie's
#     prior run for replication parity.
# ==============================================================================

# Setup - load packages and functions ------------------------------------------

library(dplyr)
library(glue)
library(arrow)
library(haven)
library(tidyr)
library(dotenv)

# Load .env (project root, one level above src/) and read pipeline paths.
load_dot_env()
data_path   <- Sys.getenv("DATA_DIR")
output_path <- Sys.getenv("OUTPUT_DIR")


# Load firm-quarter sample -----------------------------------------------------

main_data <- arrow::read_parquet(glue("{data_path}/2025_12_16_firm_qtr_regdata.parquet"))

message(glue("Loaded firm-quarter sample: {nrow(main_data)} observations"))


# Define common variables that don't vary by FDP ------------------------------

common_vars <- c(
  "permno", "gvkey",  "datadate", "yearqtr", "cusip", "fyearq",
  "fqtr", "conm", "rdq", "crsp_ticker", "crsp_comnam", "best_anndats",
  "best_dttm", "best_anndats_adj", "prcn2", "ea_count", "log_ea_count",
  "fic", "sic", "unique_2cent_actual", "unique_2cent_mean", "unique_2cent_surp",
  "ibq", "atq", "stkcoq", "cshoq", "lnmve", "btm", "percent_change_cshfdq",
  "percent_change_ibq", "q4", "abs_spiq_ibq", "unexpected_item", "stock_split",
  "dispersion", "lagmins", "log_lagmins", "ea_articles", "ln_n_articles",
  "guidance", "io", "ret_vol",  "unique_following",
  "max_surp", "min_surp", "max_min_surp", "std_dev_surp", "max_min_surp_price",
  "std_dev_surp_price", "at_least_one_miss", "at_least_one_beat",
  "miss_and_beat_mbe_2", "car_0top1", "car_0top1_100",
  "future_op_earn", "future_op_cf", "tansitory_pos_value", "tansitory_neg_value",
  "stock_compensation", "amortization", 
  "abnormal_spread", "abnormal_depth", "abnormal_price_impact",
  "abnormal_volatility_rh", "MRT", "high_stkcomp","slnmve_w1",
  "sbtm_w1","sio_w1", "sunique_following","guidance",
  "sdispersion_w1","spercent_change_ibq_w1",
  "spercent_change_cshfdq_w1", "sabs_spiq_ibq_w1","sret_vol_w1",
  "slog_lagmins_w1", "slog_ea_count_w1","sln_n_articles_w1"
  
)


# Create individual FDP datasets -----------------------------------------------
message("Creating FDP-specific datasets...")

# IBES
sample1_ibes <- main_data |>
  select(
    all_of(common_vars),
    following = ibes_following,
    mean_u = ibes_mean_u,
    actual_u = ibes_actual_u,
    surp_u = ibes_surp_u,
    surp_u_price = ibes_surp_u_price,
    ssurp_u_price = sibes_surp_u_price_w1,
    exclusions = ibes_exclusions,
    earnings = ibes_earnings,
    future_earnings = ibes_future_earnings,
    act_u_fdpagree = ibes_act_u_fdpagree,
    quarters_followed = ibes_quarters_followed,
    rank_lag_accuracy = ibes_rank_lag_accuracy,
    coef_earn = ibes_coef_earn,
    coef_cf = ibes_coef_cf,
    articles = ibes_articles
  ) |>
  mutate(
    fdpibes = 1L,
    fdpfset = 0L,
    fdpzacks = 0L,
    fdpciq = 0L,
    fdpbb = 0L,
    fdp_num = 1L
  )

# FSET
sample1_fset <- main_data |>
  select(
    all_of(common_vars),
    following = fset_following,
    mean_u = fset_mean_u,
    actual_u = fset_actual_u,
    surp_u = fset_surp_u,
    surp_u_price = fset_surp_u_price,
    ssurp_u_price = sfset_surp_u_price_w1,
    exclusions = fset_exclusions,
    earnings = fset_earnings,
    future_earnings = fset_future_earnings,
    act_u_fdpagree = fset_act_u_fdpagree,
    quarters_followed = fset_quarters_followed,
    rank_lag_accuracy = fset_rank_lag_accuracy,
    coef_earn = fset_coef_earn,
    coef_cf = fset_coef_cf,
    articles = fset_articles
  ) |>
  mutate(
    fdpibes = 0L,
    fdpfset = 1L,
    fdpzacks = 0L,
    fdpciq = 0L,
    fdpbb = 0L,
    fdp_num = 5L
  )

# Zacks
sample1_zacks <- main_data |>
  select(
    all_of(common_vars),
    following = zacks_following,
    mean_u = zacks_mean_u,
    actual_u = zacks_actual_u,
    surp_u = zacks_surp_u,
    surp_u_price = zacks_surp_u_price,
    ssurp_u_price = szacks_surp_u_price_w1,
    exclusions = zacks_exclusions,
    earnings = zacks_earnings,
    future_earnings = zacks_future_earnings,
    act_u_fdpagree = zacks_act_u_fdpagree,
    quarters_followed = zacks_quarters_followed,
    rank_lag_accuracy = zacks_rank_lag_accuracy,
    coef_earn = zacks_coef_earn,
    coef_cf = zacks_coef_cf,
    articles = zacks_articles
  ) |>
  mutate(
    fdpibes = 0L,
    fdpfset = 0L,
    fdpzacks = 1L,
    fdpciq = 0L,
    fdpbb = 0L,
    fdp_num = 2L
  )

# CIQ
sample1_ciq <- main_data |>
  select(
    all_of(common_vars),
    following = ciq_following,
    mean_u = ciq_mean_u,
    actual_u = ciq_actual_u,
    surp_u = ciq_surp_u,
    surp_u_price = ciq_surp_u_price,
    ssurp_u_price = sciq_surp_u_price_w1,
    exclusions = ciq_exclusions,
    earnings = ciq_earnings,
    future_earnings = ciq_future_earnings,
    act_u_fdpagree = ciq_act_u_fdpagree,
    quarters_followed = ciq_quarters_followed,
    rank_lag_accuracy = ciq_rank_lag_accuracy,
    coef_earn = ciq_coef_earn,
    coef_cf = ciq_coef_cf,
    articles = ciq_articles
  ) |>
  mutate(
    fdpibes = 0L,
    fdpfset = 0L,
    fdpzacks = 0L,
    fdpciq = 1L,
    fdpbb = 0L,
    fdp_num = 3L
  )

# Bloomberg
sample1_bb <- main_data |>
  select(
    all_of(common_vars),
    following = bb_following,
    mean_u = bb_mean_u,
    actual_u = bb_actual_u,
    surp_u = bb_surp_u,
    surp_u_price = bb_surp_u_price,
    ssurp_u_price = sbb_surp_u_price_w1,
    exclusions = bb_exclusions,
    earnings = bb_earnings,
    future_earnings = bb_future_earnings,
    act_u_fdpagree = bb_act_u_fdpagree,
    quarters_followed = bb_quarters_followed,
    rank_lag_accuracy = bb_rank_lag_accuracy,
    coef_earn = bb_coef_earn,
    coef_cf = bb_coef_cf,
    articles = bb_articles
  ) |>
  mutate(
    fdpibes = 0L,
    fdpfset = 0L,
    fdpzacks = 0L,
    fdpciq = 0L,
    fdpbb = 1L,
    fdp_num = 4L
  )


# Combine all FDPs -------------------------------------------------------------
message("Combining FDP datasets...")

sample2 <- bind_rows(
  sample1_ibes,
  sample1_fset,
  sample1_zacks,
  sample1_ciq,
  sample1_bb
) |>
  arrange(gvkey, datadate, fdp_num)

message(glue("Combined FDP sample: {nrow(sample2)} observations"))


# Create within firm-quarter ranks --------------------------------------------
message("Creating within firm-quarter ranks...")

# Helper function to replicate SAS proc rank with ties=high
# Returns raw ranks (not scaled), matching SAS behavior
sas_rank_high <- function(x) {
  # SAS proc rank with ties=high: tied values get the maximum rank in their tie group
  # Missing values remain missing
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  
  # Use frank from data.table style ranking: ties.method = "max"
  # This gives highest rank to ties (what SAS ties=high does)
  rank(x, ties.method = "max", na.last = "keep")
}

sample3 <- sample2 |>
  group_by(gvkey, datadate) |>
  mutate(
    # Create raw ranks (SAS style with ties=high)
    rank_following = sas_rank_high(following),
    rank_exp = sas_rank_high(quarters_followed),
    rank_earn_persist = sas_rank_high(coef_earn),
    rank_cf_persist = sas_rank_high(coef_cf),
    rank_media = sas_rank_high(articles),
    # Actual agreement (already 0-1 scaled)
    actual_agree = act_u_fdpagree / 4
  ) |>
  ungroup() |>
  # Transform ranks to 0-1 scale (matching SAS: (rank-1)/4)
  mutate(
    rank_following = (rank_following - 1) / 4,
    rank_exp = (rank_exp - 1) / 4,
    rank_earn_persist = (rank_earn_persist - 1) / 4,
    rank_cf_persist = (rank_cf_persist - 1) / 4,
    rank_media = (rank_media - 1) / 4
  )


# Create average quality measure -----------------------------------------------

sample4 <- sample3 |>
  mutate(
    # Create missing indicators
    missing_persist_earn = as.integer(is.na(rank_earn_persist)),
    missing_persist_cf = as.integer(is.na(rank_cf_persist)),
    missing_accuracy = as.integer(is.na(rank_lag_accuracy)),
    missing_cites2 = as.integer(is.na(rank_media)),
    # Replace missing with 0 for average calculation
    rank_earn_persist = if_else(is.na(rank_earn_persist), 0, rank_earn_persist),
    rank_cf_persist = if_else(is.na(rank_cf_persist), 0, rank_cf_persist),
    rank_lag_accuracy = if_else(is.na(rank_lag_accuracy), 0, rank_lag_accuracy),
    rank_media = if_else(is.na(rank_media), 0, rank_media),
    # Calculate average quality
    avg_quality = (rank_lag_accuracy + rank_earn_persist + rank_cf_persist +
                     rank_exp + actual_agree + rank_media + rank_following) / 7
  )


# Rank average quality within firm-quarter ------------------------------------

sample5 <- sample4 |>
  group_by(gvkey, datadate) |>
  mutate(
    rank_avg_quality = sas_rank_high(avg_quality),
    rank_avg_quality = (rank_avg_quality - 1) / 4
  ) |>
  ungroup() |> 
  mutate(
    fdp_num = labelled(
      fdp_num,
      labels = c(
        IBES  = 1,
        ZACKS = 2,
        CIQ   = 3,
        BB    = 4,
        FSET  = 5
      )
    )
  )


# Create additional variables for regressions ---------------------------------

sample6 <- sample5 |>
  mutate(
    # Signed return indicator (1 if positive, 0 if negative)
    returns_pos = as.integer(car_0top1_100 >= 0),
    # MBE categorical variable based on surprise threshold
    mbe_2 = case_when(
      surp_u < -0.015 ~ -1L,
      surp_u > 0.015 ~ 1L,
      TRUE ~ 0L
    )
  ) 

# Save to parquet
arrow::write_parquet(sample6, glue("{data_path}/2025_12_16_fdp_firm_qtr_regdata_all5.parquet"))
message(glue("    ✓ Wrote Parquet file: {data_path}/2025_12_16_fdp_firm_qtr_regdata_all5.parquet"))

# Save to Stata
write_dta(sample6, glue("{data_path}/2025_12_16_fdp_firm_qtr_regdata_all5.dta"))
message(glue("    ✓ Wrote Stata file: {data_path}/2025_12_16_fdp_firm_qtr_regdata_all5.dta"))

# Keep only observations that both miss and beat in at least two FDPs ----------

sample7 <- sample6 |> 
  filter(miss_and_beat_mbe_2==1)


# Export final dataset ---------------------------------------------------------
message("\nExporting final datasets...")

# Save to parquet
arrow::write_parquet(sample7, glue("{data_path}/2025_12_16_fdp_firm_qtr_regdata.parquet"))
message(glue("    ✓ Wrote Parquet file: {data_path}/2025_12_16_fdp_firm_qtr_regdata.parquet"))

# Save to Stata
write_dta(sample7, glue("{data_path}/2025_12_16_fdp_firm_qtr_regdata.dta"))
message(glue("    ✓ Wrote Stata file: {data_path}/2025_12_16_fdp_firm_qtr_regdata.dta"))


# Table 1 Panel A row 8 --------------------------------------------------------
# Append the Miss and Beat row to the cascade written by 011.

panel_a <- read.csv(glue("{output_path}/table-01-panel-a.csv"),
                    stringsAsFactors = FALSE, check.names = FALSE)

n_firm_qtr_mb     <- sample7 |> distinct(gvkey, datadate) |> nrow()
n_fdp_firm_qtr_mb <- nrow(sample7)

panel_a <- rbind(
  panel_a,
  data.frame(
    section              = "Miss and Beat Sample",
    description          = paste0(
      "with at least one FDP earnings surprise <= -0.015 and at least ",
      "one FDP earnings surprise >= 0.015"
    ),
    firm_quarter_obs     = n_firm_qtr_mb,
    fdp_firm_quarter_obs = n_fdp_firm_qtr_mb,
    stringsAsFactors     = FALSE
  )
)

write.csv(panel_a, glue("{output_path}/table-01-panel-a.csv"),
          row.names = FALSE, na = "")
message(glue("    ✓ Appended Miss and Beat row to Table 1 Panel A: ",
             "{output_path}/table-01-panel-a.csv"))


