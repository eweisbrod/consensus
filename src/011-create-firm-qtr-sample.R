# ==============================================================================
# 011-create-firm-qtr-sample.R
#
# Purpose:
#   Assemble the firm-quarter regression sample by joining all_five4 with
#   the four downstream-derived datasets (unique-value counts, abnormal
#   OIB measures, market-response-time features, and RavenPack article
#   counts), apply sample-restriction filters (winsorization, year cutoff,
#   etc.), and write the regression-input parquets / dta files consumed
#   by 013.
#
# Inputs (from DATA_DIR):
#   all_five4.sas7bdat                       (output of 005)
#   unique_counts.parquet                    (output of 010)
#   abnormal-oib-data.parquet                (output of 007)
#   mrt_all.parquet                          (output of 008)
#   rpack_EA_article_counts.parquet          (output of 009)
#
# Outputs (to DATA_DIR):
#   all_vars_all_obs.parquet / .dta          (intermediate: all-vars,
#                                            all-firm-quarters, pre-filter)
#   firm_qtr_2020andearlier_CCM.parquet/.dta (CCM-linkable subset, <=2020)
#   firm_qtr_2025_12_16.dta                  (dated full-sample dta)
#   2025_12_16_firm_qtr_regdata.parquet/.dta (final regression input
#                                            consumed by 013)
#
# Outputs (to OUTPUT_DIR):
#   table-01-panel-a.csv                     (sample-selection cascade,
#                                            rows 1-7; row 8 appended by 012)
#   table-01-panel-b.csv                     (FDP coverage of CCM sample)
#
# Notes:
#   - Reads DATA_DIR and OUTPUT_DIR from .env via the dotenv package.
#   - Pure assembly + filtering -- no WRDS, no networking.
#   - Output filenames preserve the 2025_12_16 date suffix from Jessie's
#     prior run for replication parity.
# ==============================================================================

# Setup - load packages and functions ------------------------------------------

library(dplyr)
library(RPostgres)
library(DBI)
library(dbplyr)
library(glue)
library(arrow)
library(haven)
library(stringr)
library(httr)
library(tidyverse)
library(dotenv)

# Load .env (project root, one level above src/) and read pipeline paths.
load_dot_env()
data_path   <- Sys.getenv("DATA_DIR")
output_path <- Sys.getenv("OUTPUT_DIR")

# Shared helpers (winsorize_x is used below in the regression-sample build).
source("src/utils.R")

# Load the all_five4 SAS dataset -----------------------------------------------

main_data <- read_sas(glue("{data_path}/all_five4.sas7bdat"))

# Load Data from Script 012 - Unique Counts ------------------------------------

check <- arrow::read_parquet(glue("{data_path}/unique_counts.parquet"))

unique_data <- arrow::read_parquet(glue("{data_path}/unique_counts.parquet")) |> 
  select(gvkey,datadate,starts_with("unique_"),-unique_following)

data1 <- main_data |>  
  rename_with(tolower) |>
  left_join(unique_data, by = c("gvkey", "datadate"))


# Merge in TAQ data ------------------------------------------------------------

abnormal_data <- arrow::read_parquet(glue("{data_path}/abnormal-oib-data.parquet")) |>
  select(
    sym_root, sym_suffix, best_anndats_adj,
    ab_retail_imbalance, ab_inst_imbalance, ab_large_imbalance, ab_ret_share,
    abnormal_volatility_all, abnormal_volatility_rh
  )

data2 <- data1 |>
  rename_with(tolower) |>
  left_join(abnormal_data, by = c("sym_root", "sym_suffix", "best_anndats_adj"))


# Merge in MRT data ------------------------------------------------------------

mrt_data <- arrow::read_parquet(glue("{data_path}/mrt_all.parquet")) |>
  select(sym_root, sym_suffix, best_dttm, MRT)

data3 <- data2 |>
  rename(best_dttm = best_anntims) |>
  left_join(mrt_data, by = c("sym_root", "sym_suffix", "best_dttm"))


# Merge in RavenPack news data -------------------------------------------------

rpack_data <- arrow::read_parquet(glue("{data_path}/rpack_EA_article_counts.parquet")) |>
  select(
    permno, best_anndats_adj, ea_articles,
    ibes_articles = ibes_total2_articles,
    zacks_articles = zacks_total2_articles,
    ciq_articles = ciq_total2_articles,
    bb_articles = bb_total2_articles,
    fset_articles = fset_total2_articles
  )

data4 <- data3 |>
  left_join(rpack_data, by = c("permno", "best_anndats_adj"))


# Data quality checks ----------------------------------------------------------

dup_keys1 <- data4 |>
  count(gvkey, datadate) |>
  filter(n > 1) |>
  select(gvkey, datadate)

if (nrow(dup_keys1) > 0) {
  warning(glue("Dropping {nrow(dup_keys1)} duplicated gvkey-datadate groups entirely"))
  
  data4 <- data4 |>
    anti_join(dup_keys1, by = c("gvkey", "datadate"))
} else {
  message("    ✓ No duplicates on gvkey-datadate")
}

# 2) permno × best_anndats_adj — drop all dup groups
dup_keys2 <- data4 |>
  count(permno, best_anndats_adj) |>
  filter(n > 1) |>
  select(permno, best_anndats_adj)

if (nrow(dup_keys2) > 0) {
  warning(glue("Dropping {nrow(dup_keys2)} duplicated permno-announcement date groups entirely"))
  
  data4 <- data4 |>
    anti_join(dup_keys2, by = c("permno", "best_anndats_adj"))
  } else {
  message("    ✓ No duplicates on permno-announcement date")
  }

# Write merged output ----------------------------------------------------------

write_dta(data4, glue("{data_path}/all_vars_all_obs.dta"))
message(glue("    ✓ Wrote Stata file: {data_path}/all_vars_all_obs.dta"))

arrow::write_parquet(data4, glue("{data_path}/all_vars_all_obs.parquet"))
message(glue("    ✓ Wrote Parquet file: {data_path}/all_vars_all_obs.parquet"))


################################################################################
# SAMPLE SELECTION
################################################################################



#Can pick up sample selection here if nothing changed in data collection
all_vars_all_obs <-  read_parquet(glue("{data_path}/all_vars_all_obs.parquet"))



# Apply Basic CCM Filters ------------------------------------------------------




fdp_ccm0 <- all_vars_all_obs |>    
  # Create year variable
  mutate(year = year(datadate)) |>
  filter(
    # Year filter
    year >= 2002, year <= 2020,
    # USA filter
    fic == "USA")

message(sprintf("Initial CCM Sample: %d observations", nrow(fdp_ccm0)))


fdp_ccm1 <- fdp_ccm0 |>
  filter(
    # Financial statement filters
    !is.na(atq), !is.na(saleq), !is.na(ceqq),
    saleq > 25, atq > 100, prccq > 1)


message(sprintf("After CCM Size filters: %d observations", nrow(fdp_ccm1)))

fdp_ccm <- fdp_ccm1 |>
  filter(
    # CUSIP match
    substr(cusip, 1, 8) == ncusip
  ) |>
  # Create coverage variables
  mutate(
    coverage_number = ibes_covered + fset_covered + zacks_covered +
      ciq_covered + bb_covered,
    coverage_all = as.integer(coverage_number == 5)
  )

message(sprintf("After CCM Cusip filters: %d observations", nrow(fdp_ccm)))

# Save CCM dataset for coverage figures
write_dta(fdp_ccm, glue("{data_path}/firm_qtr_2020andearlier_CCM.dta"))
message(glue("✓ Wrote Stata file: {data_path}/firm_qtr_2020andearlier_CCM.dta"))

arrow::write_parquet(fdp_ccm, glue("{data_path}/firm_qtr_2020andearlier_CCM.parquet"))
message(glue("✓ Wrote Parquet file: {data_path}/firm_qtr_2020andearlier_CCM.parquet"))


# Continue filtering for complete sample ---------------------------------------
message("Applying complete sample filters...")

# Define FDP columns for operations
fdp_cols <- c("ibes", "fset", "zacks", "ciq", "bb")
actual_cols <- paste0(fdp_cols, "_actual_u")
surp_cols <- paste0(fdp_cols, "_surp_u")
mean_cols <- paste0(fdp_cols, "_mean_u")
following_cols <- paste0(fdp_cols, "_following")

# Define control variables that must be non-missing
control_vars <- c(
  "unique_following", "unexpected_item", "abs_spiq_ibq", "percent_change_cshfdq",
  "stock_split", "dispersion", "log_lagmins", "lnmve", "btm", "io", "guidance",
  "percent_change_ibq", "ret_vol", "q4", "log_ea_count", "max_min_surp_price",
  "std_dev_surp_price", "car_0top1", "stock_compensation", "amortization",
  #"tansitory_pos_value", "tansitory_neg_value", "future_op_earn", "future_op_cf",
  "ibes_rank_lag_accuracy","zacks_rank_lag_accuracy","ciq_rank_lag_accuracy",
  "fset_rank_lag_accuracy","bb_rank_lag_accuracy"
)

#With IBES coverage
fdp_filtered1 <- fdp_ccm |>
  filter(
    # All 5 FDPs required
    ibes_covered == 1)

message(sprintf("With IBES Coverage: %d observations", nrow(fdp_filtered1)))

#ALL 5 FDPS
fdp_filtered2 <- fdp_filtered1 |>
  filter(
    fset_covered == 1, zacks_covered == 1,
    ciq_covered == 1, bb_covered == 1
  ) 


message(sprintf("With ALL FDP Coverage: %d observations", nrow(fdp_filtered2)))

fdp_filtered <- fdp_filtered2 |>
  # Check all control variables and FDP variables are non-missing
  filter(
    if_all(all_of(control_vars), ~ !is.na(.x)),
    if_all(all_of(c(actual_cols, surp_cols, mean_cols, following_cols)), ~ !is.na(.x))
  )


message(sprintf("After complete filters: %d observations", nrow(fdp_filtered)))


# Winsorization of FDP variables -----------------------------------------------
message("Winsorizing FDP actual and surprise variables...")

# Winsorize all actual and surprise variables at 1%/99%
fdp_winsorized <- fdp_filtered |>
  mutate(
    across(all_of(c(actual_cols, surp_cols)),
           ~ winsorize_x(.x, cut = 0.01),
           .names = "{.col}_w1")
  ) |>
  # Create truncate flag
  mutate(
    truncate = as.integer(
      ibes_actual_u != ibes_actual_u_w1 | ibes_surp_u != ibes_surp_u_w1 |
        fset_actual_u != fset_actual_u_w1 | fset_surp_u != fset_surp_u_w1 |
        zacks_actual_u != zacks_actual_u_w1 | zacks_surp_u != zacks_surp_u_w1 |
        ciq_actual_u != ciq_actual_u_w1 | ciq_surp_u != ciq_surp_u_w1 |
        bb_actual_u != bb_actual_u_w1 | bb_surp_u != bb_surp_u_w1
    ),
    # Create mean surprise
    mean_surp = (ibes_surp_u + fset_surp_u + zacks_surp_u +
                   ciq_surp_u + bb_surp_u) / 5,
    mean_surp_price = mean_surp / prcn2
  )

message(sprintf("Truncated: %d (%.1f%%)",
                sum(fdp_winsorized$truncate),
                100 * mean(fdp_winsorized$truncate)))

# Remove truncated observations
fdp_truncated <- fdp_winsorized |>
  filter(truncate == 0)

message(sprintf("After Truncation: %d observations", nrow(fdp_truncated)))


# Stock compensation and additional variables ----------------------------------
message("Creating stock compensation and additional variables...")

sample1 <- fdp_truncated |>
  mutate(
    # Stock compensation
    stkcoq = if_else(is.na(stkcoq), 0, stkcoq),
    sbc = stkcoq / cshoq
  ) |>
  mutate(ln_n_articles = coalesce(log(ea_articles + 1),0)) |> 
  group_by(yearqtr) |>
  mutate(
    median_sbc = median(sbc, na.rm = TRUE),
    high_stkcomp = as.integer(sbc > median_sbc)
  ) |>
  ungroup() 

# Disagreement Variables -------------------------------------------------------

sample2 <- sample1 |>
  rowwise() |>
  mutate(
    # Max/min for actual
    max_actual_u = max(c_across(all_of(actual_cols)), na.rm = TRUE),
    min_actual_u = min(c_across(all_of(actual_cols)), na.rm = TRUE),
    max_min_actual_u = max_actual_u - min_actual_u,
    # Max/min for mean
    max_mean_u = max(c_across(all_of(mean_cols)), na.rm = TRUE),
    min_mean_u = min(c_across(all_of(mean_cols)), na.rm = TRUE),
    max_min_mean_u = max_mean_u - min_mean_u
  ) |>
  ungroup() |>
  mutate(
    # Create buckets for Figure 5
    bucket_actual_u = case_when(
      max_min_actual_u >= 0.015 & max_min_actual_u <= 0.02 ~ 1L,
      max_min_actual_u > 0.02 & max_min_actual_u <= 0.05 ~ 2L,
      max_min_actual_u > 0.05 & max_min_actual_u <= 0.1 ~ 3L,
      max_min_actual_u > 0.1 & max_min_actual_u <= 0.2 ~ 4L,
      max_min_actual_u > 0.2 ~ 5L,
      TRUE ~ NA_integer_
    ),
    bucket_mean_u = case_when(
      max_min_mean_u >= 0.015 & max_min_mean_u <= 0.02 ~ 1L,
      max_min_mean_u > 0.02 & max_min_mean_u <= 0.05 ~ 2L,
      max_min_mean_u > 0.05 & max_min_mean_u <= 0.1 ~ 3L,
      max_min_mean_u > 0.1 & max_min_mean_u <= 0.2 ~ 4L,
      max_min_mean_u > 0.2 ~ 5L,
      TRUE ~ NA_integer_
    ),
    bucket_surp = case_when(
      max_min_surp >= 0.015 & max_min_surp <= 0.02 ~ 1L,
      max_min_surp > 0.02 & max_min_surp <= 0.05 ~ 2L,
      max_min_surp > 0.05 & max_min_surp <= 0.1 ~ 3L,
      max_min_surp > 0.1 & max_min_surp <= 0.2 ~ 4L,
      max_min_surp > 0.2 ~ 5L,
      TRUE ~ NA_integer_
    ),
    # Update unique_2cent variables based on threshold
    # note: 12-16-2025 no obs affected but leave as sanity check
    unique_2cent_actual = if_else(max_min_actual_u < 0.015, 1L, unique_2cent_actual),
    unique_2cent_mean = if_else(max_min_mean_u < 0.015, 1L, unique_2cent_mean),
    unique_2cent_surp = if_else(max_min_surp < 0.015, 1L, unique_2cent_surp),
    # Scale by price
    max_min_actual_scale = max_min_actual_u / prcn2 * 100,
    max_min_mean_scale = max_min_mean_u / prcn2 * 100,
    max_min_surp_scale = max_min_surp / prcn2 * 100
  ) |>
  mutate(
    # Scaled buckets
    bucket_actual_scale = case_when(
      max_min_actual_scale <= 0.05 ~ 1L,
      max_min_actual_scale > 0.05 & max_min_actual_scale <= 0.25 ~ 2L,
      max_min_actual_scale > 0.25 & max_min_actual_scale <= 0.50 ~ 3L,
      max_min_actual_scale > 0.50 & max_min_actual_scale <= 1 ~ 4L,
      max_min_actual_scale > 1 ~ 5L,
      TRUE ~ NA_integer_
    ),
    bucket_mean_scale = case_when(
      max_min_mean_scale <= 0.05 ~ 1L,
      max_min_mean_scale > 0.05 & max_min_mean_scale <= 0.25 ~ 2L,
      max_min_mean_scale > 0.25 & max_min_mean_scale <= 0.50 ~ 3L,
      max_min_mean_scale > 0.50 & max_min_mean_scale <= 1 ~ 4L,
      max_min_mean_scale > 1 ~ 5L,
      TRUE ~ NA_integer_
    ),
    bucket_surp_scale = case_when(
      max_min_surp_scale <= 0.05 ~ 1L,
      max_min_surp_scale > 0.05 & max_min_surp_scale <= 0.25 ~ 2L,
      max_min_surp_scale > 0.25 & max_min_surp_scale <= 0.50 ~ 3L,
      max_min_surp_scale > 0.50 & max_min_surp_scale <= 1 ~ 4L,
      max_min_surp_scale > 1 ~ 5L,
      TRUE ~ NA_integer_
    )
  )


# Save intermediate dataset
message("Saving intermediate dataset...")
write_dta(sample2, glue("{data_path}/firm_qtr_2025_12_16.dta"))

# WINSORIZATION AND STANDARDIZATION --------------------------------------------
message("Performing winsorization and standardization")

# Define variables to winsorize
vars_to_winsor <- c(
  "abnormal_spread", "abnormal_depth", "abnormal_price_impact",
  "abnormal_volatility_rh", "MRT", "lnmve", "btm", "io", "dispersion",
  "percent_change_ibq", "percent_change_cshfdq", "abs_spiq_ibq", "ret_vol",
  "log_lagmins", "log_ea_count", "max_min_surp", "std_dev_surp","abs_ibes_surp_u_price",
  "max_min_surp_price", "std_dev_surp_price", "ln_n_articles", 
  "ibes_surp_u_price","zacks_surp_u_price","ciq_surp_u_price","fset_surp_u_price","bb_surp_u_price"
)

# Define variables to standardize
vars_to_standardize <- c(
  "lnmve_w1", "btm_w1", "io_w1", "unique_following", "dispersion_w1",
  "percent_change_ibq_w1", "percent_change_cshfdq_w1", "abs_spiq_ibq_w1",
  "ret_vol_w1", "log_lagmins_w1", "log_ea_count_w1", "abs_ibes_surp_u_price_w1",
  "max_min_surp_price_w1_100", "std_dev_surp_price_w1_100","ln_n_articles_w1",
  "ibes_surp_u_price_w1","zacks_surp_u_price_w1","ciq_surp_u_price_w1","fset_surp_u_price_w1","bb_surp_u_price_w1"
)

sample3 <- sample2 |>
  # Create helper variables
  mutate(
    car_0top1_100 = car_0top1 * 100,
    future_op_earn100 = future_op_earn * 100,
    future_op_cf100 = future_op_cf * 100,
    abs_ibes_surp_u_price = abs(ibes_surp_u_price),
    abs_mean_surp_price = abs(mean_surp_price)
  ) |>
  # Winsorize all specified variables
  mutate(
    across(all_of(vars_to_winsor),
           ~ winsorize_x(.x, cut = 0.01),
           .names = "{.col}_w1")
  ) |>
  # Rename MRT winsorized variable
  rename(mrt_32hr_w1 = MRT_w1) |>
  # Scale price variables
  mutate(
    max_min_surp_price_w1_100 = max_min_surp_price_w1 * 100,
    std_dev_surp_price_w1_100 = std_dev_surp_price_w1 * 100
  ) |> 
# Standardize control variables (create z-scores)
  mutate(
    across(all_of(vars_to_standardize),
           ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE),
           .names = "s{.col}")
  ) |> 
# Create interaction and indicator variables
  mutate(
    # Positive surprise indicator
    pos_surprise = as.integer(ibes_surp_u_price > 0),
    pos_sabs_ibes_surp_u_price_w1 = pos_surprise * sabs_ibes_surp_u_price_w1
  ) |>
  # Create EA busyness variables by quarter
  group_by(yearqtr) |>
  mutate(
    median_ea = median(log_ea_count, na.rm = TRUE),
    high_ea = as.integer(log_ea_count > median_ea),
    top_20_ea = quantile(log_ea_count, 0.8, na.rm = TRUE),
    high_ea_v2 = as.integer(log_ea_count > top_20_ea),
    bot_20_ea = quantile(log_ea_count, 0.2, na.rm = TRUE),
    low_ea_v2 = as.integer(log_ea_count < bot_20_ea)
  ) |>
  ungroup() |>
  mutate(
    extreme_busy = as.integer(high_ea_v2 == 1 | low_ea_v2 == 1)
  )

# Final export -----------------------------------------------------------------

# Export analysis-ready file
write_dta(sample3, glue("{data_path}/2025_12_16_firm_qtr_regdata.dta"))
write_parquet(sample3, glue("{data_path}/2025_12_16_firm_qtr_regdata.parquet"))

# Summary statistics -----------------------------------------------------------
message("\n=== Pipeline Summary ===")
message(sprintf("Final sample size: %d observations", nrow(sample3)))
message(sprintf("Years: %d-%d",
                min(year(sample3$datadate)),
                max(year(sample3$datadate))))
message(sprintf("Unique firms: %d", n_distinct(sample3$gvkey)))
message(sprintf("Mean coverage: %.2f FDPs", mean(sample3$coverage_number)))

# Uniqueness statistics
message("\nUniqueness statistics:")
message(sprintf("  Unique actual (2 cent): %.1f%%",
                100 * mean(sample3$unique_2cent_actual == 1, na.rm = TRUE)))
message(sprintf("  Unique mean (2 cent): %.1f%%",
                100 * mean(sample3$unique_2cent_mean == 1, na.rm = TRUE)))
message(sprintf("  Unique surprise (2 cent): %.1f%%",
                100 * mean(sample3$unique_2cent_surp == 1, na.rm = TRUE)))

# Table 1 outputs --------------------------------------------------------------
# Panel A rows 1-7 are the sample-selection cascade. Row 8 (Miss and Beat) is
# appended in 012, which is where that filter is applied.

panel_a <- tibble::tibble(
  section = c(
    "CCM Sample", "CCM Sample", "CCM Sample",
    "Common Sample", "Common Sample", "Common Sample", "Common Sample"
  ),
  description = c(
    "Merged CRSP-Compustat Dataset from 2002 to 2020",
    "with non-missing assets, sales, and common equity, and with sales>$25 million and assets > $100 million and price > $1",
    "with historical CUSIP same as header CUSIP",
    "with coverage by I/B/E/S",
    "with coverage by all five FDPs",
    "with control variables",
    "truncated sample"
  ),
  firm_quarter_obs = c(
    nrow(fdp_ccm0), nrow(fdp_ccm1), nrow(fdp_ccm),
    nrow(fdp_filtered1), nrow(fdp_filtered2), nrow(fdp_filtered),
    nrow(fdp_truncated)
  ),
  fdp_firm_quarter_obs = c(
    NA_integer_, NA_integer_, NA_integer_, NA_integer_,
    5L * nrow(fdp_filtered2), 5L * nrow(fdp_filtered),
    5L * nrow(fdp_truncated)
  )
)

write.csv(panel_a, glue("{output_path}/table-01-panel-a.csv"),
          row.names = FALSE, na = "")
message(glue("    ✓ Wrote Table 1 Panel A (rows 1-7): ",
             "{output_path}/table-01-panel-a.csv"))

# Panel B: distribution of CCM-sample firm-quarters by number of FDPs covering.
panel_b_counts <- fdp_ccm |>
  count(coverage_number, name = "n") |>
  arrange(coverage_number) |>
  mutate(
    fdp_coverage = as.character(coverage_number),
    pct = round(100 * n / sum(n), 2)
  ) |>
  select(fdp_coverage, n, pct)

panel_b <- bind_rows(
  panel_b_counts,
  tibble::tibble(
    fdp_coverage = "Total",
    n = sum(panel_b_counts$n),
    pct = round(sum(panel_b_counts$pct), 2)
  )
)

write.csv(panel_b, glue("{output_path}/table-01-panel-b.csv"),
          row.names = FALSE)
message(glue("    ✓ Wrote Table 1 Panel B: ",
             "{output_path}/table-01-panel-b.csv"))

message("\n=== Script completed successfully ===")



