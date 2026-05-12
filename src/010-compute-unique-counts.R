# ==============================================================================
# 010-compute-unique-counts.R
#
# Purpose:
#   Identify genuinely unique values across 5 forecast providers (IBES,
#   Zacks, CIQ, Bloomberg, FactSet) by systematically testing if values
#   differ by more than a tolerance threshold (1.5 cents by default).
#
# Algorithm:
#   Sequential pairwise comparison that eliminates values within tolerance
#   of each other until only genuinely different values remain. The
#   algorithm handles 3, 4, or 5 providers and tests all possible pairwise
#   combinations in sorted order.
#
# Inputs (from DATA_DIR):
#   all_five4.sas7bdat                       (output of 005)
#
# Outputs (to DATA_DIR):
#   unique_counts.parquet
#   unique_counts.dta
#
# Notes:
#   - Reads DATA_DIR from .env via the dotenv package.
#   - Pure compute -- no WRDS, no networking.
# ==============================================================================

# Load libraries ---------------------------------------------------------------
library(haven)
library(glue)
library(lubridate)
library(tidyverse)
library(dotenv)

# Load .env (project root, one level above src/) and read pipeline paths.
load_dot_env()
data_path <- Sys.getenv("DATA_DIR")


# Core Function: Identify Unique Values ----------------------------------------

#' Count Unique Values Across Providers With Tolerance
#'
#' This function identifies how many genuinely unique values exist across
#' multiple data providers after accounting for values that are within a
#' small tolerance of each other.
#'
#' The algorithm works by:
#' 1. Selecting relevant columns and filtering for complete cases
#' 2. Rounding values to specified precision
#' 3. Pivoting to long format for sequential comparison
#' 4. Applying 18 filter steps to eliminate values within tolerance
#' 5. Counting remaining unique values per firm-quarter
#'
#' @param data Dataframe containing provider columns
#' @param id_vars Character vector of ID variables (e.g., c("permno", "datadate"))
#' @param provider_cols Character vector of provider column names to compare
#' @param tolerance Numeric threshold for considering values "different" (default 0.015 = 1.5 cents)
#' @param rounding Integer number of decimal places to round to before comparison (default 2)
#' @param output_col_name Name for the output count column
#'
#' @return Dataframe with id_vars and count of unique values
#'
#' @examples
#' count_unique_values(
#'   data = data1,
#'   id_vars = c("permno", "datadate"),
#'   provider_cols = c("ibes_actual_u", "zacks_actual_u", "ciq_actual_u",
#'                     "bb_actual_u", "fset_actual_u"),
#'   tolerance = 0.015,
#'   output_col_name = "unique_2cent_actual"
#' )
count_unique_values <- function(data,
                                id_vars,
                                provider_cols,
                                tolerance = 0.015,
                                rounding = 2,
                                output_col_name = "unique_count") {

  # Validate inputs
  stopifnot("data must be a data frame" = is.data.frame(data))
  stopifnot("id_vars must be character vector" = is.character(id_vars))
  stopifnot("provider_cols must be character vector" = is.character(provider_cols))
  stopifnot("tolerance must be positive numeric" = is.numeric(tolerance) && tolerance > 0)

  # Step 1: Prepare long dataset with rounded values
  # This mimics the original: select columns, filter complete cases, round, pivot
  long_data <- data |>
    select(all_of(c(id_vars, provider_cols))) |>
    filter(if_all(-all_of(id_vars), ~ !is.na(.x))) |>
    mutate(across(all_of(provider_cols), ~ round(.x, rounding))) |>
    pivot_longer(
      cols = all_of(provider_cols),
      names_to = "provider",
      values_to = "value"
    ) |>
    group_by(across(all_of(id_vars))) |>
    arrange(across(all_of(id_vars)), value)

  # Step 2: Apply sequential filtering algorithm
  # This implements the complex logic from long1 -> long19
  filtered_data <- apply_sequential_filters(long_data, tolerance)

  # Step 3: Count remaining unique values per firm-quarter
  result <- filtered_data |>
    summarize(!!sym(output_col_name) := n(), .groups = "drop")

  return(result)
}


#' Apply Sequential Pairwise Comparison Filters
#'
#' This is the core algorithm that eliminates values within tolerance.
#' It systematically tests pairs of values in different positions and
#' eliminates values/groups that are too close together.
#'
#' @param data Long-format grouped dataframe
#' @param tolerance Minimum difference to keep both values
#'
#' @return Filtered dataframe with only genuinely unique values remaining
#'
#' @details
#' The algorithm handles these cases:
#' - 5 providers: Tests all pairwise combinations (positions 1-5)
#' - 4 providers: Tests relevant combinations (positions 1-4)
#' - 3 providers: Tests min, middle, max
#' - 2 providers: Tests final pair
#'
#' Testing order (by position pairs, where 1=min, 5=max):
#' 1. (1,5): Min vs Max - eliminate entire group if close
#' 2. (2,5): 2nd-min vs Max - check if max is close to 2nd-min
#' 3. (3,4): Middle pairs in 5-provider case
#' 4. Adjacent pairs: (1,2), (2,3), (3,4), (4,5)
#' 5. Final validation for 2- and 3-provider cases
apply_sequential_filters <- function(data, tolerance) {

  # Filter 1: Check if smallest and largest differ by > tolerance
  step1 <- data |>
    mutate(lag_diff = if_else(
      row_number() == n(),
      abs(value - lag(value, 4L)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 2: If we had 5 providers and eliminated none, we keep all 5
  step2 <- step1 |>
    filter(n() == 5 | (row_number() == n() & n() == 4))

  # Filter 3: For 5-provider case, check position 4 vs position 1
  step3 <- step2 |>
    mutate(lag_diff = if_else(
      row_number() == 4 & n() == 5,
      abs(value - lag(value, 3L)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 4: If we now have 3 remaining, keep only min and max
  step4 <- step3 |>
    filter(n() != 4 | (row_number() == n() & n() == 4) | (row_number() == 1 & n() == 4))

  # Filter 5: Check position 2 vs position 5 (for 5-provider case)
  step5 <- step4 |>
    mutate(lag_diff = if_else(
      row_number() == 2 & n() == 5,
      abs(value - lead(value, 3L)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 6: Again, if we have exactly 3 left, keep only edges
  step6 <- step5 |>
    filter(n() != 4 | (row_number() == n() & n() == 4) | (row_number() == 1 & n() == 4))

  # Filter 7: Check position 3 vs position 5 (5-provider case)
  step7 <- step6 |>
    mutate(lag_diff = if_else(
      row_number() == 3 & n() == 5,
      abs(value - lead(value, 2L)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 8: Remove position 3 if we have 4 remaining
  step8 <- step7 |>
    filter(n() != 4 | (row_number() != 3 & n() == 4))

  # Filter 9: Check adjacent pair in 3-provider case (position 2 vs 1)
  step9 <- step8 |>
    mutate(lag_diff = if_else(
      row_number() == 2 & n() == 3,
      abs(value - lag(value)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 10: Check position 3 vs position 1 in 5-provider case
  step10 <- step9 |>
    mutate(lag_diff = if_else(
      row_number() == 3 & n() == 5,
      abs(value - lag(value, 2L)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 11: Remove position 2 if we have 4 remaining
  step11 <- step10 |>
    filter(n() != 4 | (row_number() != 2 & n() == 4))

  # Filter 12: Check position 2 vs position 3 in 3-provider case (using lead)
  step12 <- step11 |>
    mutate(lag_diff = if_else(
      row_number() == 2 & n() == 3,
      abs(value - lead(value)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 13: Check adjacent pair in 5-provider case (position 2 vs 1)
  step13 <- step12 |>
    mutate(lag_diff = if_else(
      row_number() == 2 & n() == 5,
      abs(value - lag(value)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 14: Check adjacent pair in 4-provider case (position 3 vs 4)
  step14 <- step13 |>
    mutate(lag_diff = if_else(
      row_number() == 3 & n() == 4,
      abs(value - lead(value)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 15: Check adjacent pair in 5-provider case (position 4 vs 5)
  step15 <- step14 |>
    mutate(lag_diff = if_else(
      row_number() == 4 & n() == 5,
      abs(value - lead(value)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 16: Check adjacent pair in 5-provider case (position 3 vs 4)
  step16 <- step15 |>
    mutate(lag_diff = if_else(
      row_number() == 3 & n() == 5,
      abs(value - lead(value)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 17: Check adjacent pair in 5-provider case (position 3 vs 2)
  step17 <- step16 |>
    mutate(lag_diff = if_else(
      row_number() == 3 & n() == 5,
      abs(value - lag(value)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance)

  # Filter 18: Check adjacent pair in 4-provider case (position 3 vs 2)
  final_result <- step17 |>
    mutate(lag_diff = if_else(
      row_number() == 3 & n() == 4,
      abs(value - lag(value)),
      NaN
    )) |>
    filter(is.na(lag_diff) | lag_diff > tolerance) |>
    # Remove the temporary lag_diff column
    select(-lag_diff)

  return(final_result)
}


# Main Execution ---------------------------------------------------------------

# Load SAS data
message("Loading data from 005 script...")
data1 <- read_sas(glue("{data_path}/all_five4.sas7bdat"))
message(glue("Loaded {format(nrow(data1), big.mark = ',')} firm-quarter observations"))


# Compute unique counts for actuals --------------------------------------------
message("\n1. Computing unique actuals (all 5 providers)...")
unique_act <- count_unique_values(
  data = data1,
  id_vars = c("permno", "datadate"),
  provider_cols = c("ibes_actual_u", "zacks_actual_u", "ciq_actual_u",
                    "bb_actual_u", "fset_actual_u"),
  tolerance = 0.015,
  rounding = 2,
  output_col_name = "unique_2cent_actual"
)

message(glue("   Found {sum(unique_act$unique_2cent_actual > 1)} firm-quarters with multiple unique actuals"))



# Compute unique counts for means ----------------------------------------------
message("\n2. Computing unique consensus means (all 5 providers)...")
unique_mean <- count_unique_values(
  data = data1,
  id_vars = c("permno", "datadate"),
  provider_cols = c("ibes_mean_u", "zacks_mean_u", "ciq_mean_u",
                    "bb_mean_u", "fset_mean_u"),
  tolerance = 0.015,
  rounding = 2,
  output_col_name = "unique_2cent_mean"
)

message(glue("   Found {sum(unique_mean$unique_2cent_mean > 1)} firm-quarters with multiple unique means"))


# Compute unique counts for surprises ------------------------------------------
message("\n3. Computing unique surprises (all 5 providers)...")
unique_surp <- count_unique_values(
  data = data1,
  id_vars = c("permno", "datadate"),
  provider_cols = c("ibes_surp_u", "zacks_surp_u", "ciq_surp_u",
                    "bb_surp_u", "fset_surp_u"),
  tolerance = 0.015,
  rounding = 2,
  output_col_name = "unique_2cent_surp"
)

message(glue("   Found {sum(unique_surp$unique_2cent_surp > 1)} firm-quarters with multiple unique surprises"))


# Merge all unique counts back to main data -----------------------------------
message("\n4. Merging unique counts back to main dataset...")
data2 <- data1 |>
  left_join(unique_act, by = c("permno", "datadate")) |>
  left_join(unique_mean, by = c("permno", "datadate")) |>
  left_join(unique_surp, by = c("permno", "datadate"))


# Compute unique counts without Zacks -----------------------------------------
message("\n5. Computing unique counts excluding Zacks...")

unique_act_no_zacks <- count_unique_values(
  data = data1,
  id_vars = c("permno", "datadate"),
  provider_cols = c("ibes_actual_u", "ciq_actual_u", "bb_actual_u", "fset_actual_u"),
  tolerance = 0.015,
  output_col_name = "unique_act_no_zacks"
)

unique_mean_no_zacks <- count_unique_values(
  data = data1,
  id_vars = c("permno", "datadate"),
  provider_cols = c("ibes_mean_u", "ciq_mean_u", "bb_mean_u", "fset_mean_u"),
  tolerance = 0.015,
  output_col_name = "unique_mean_no_zacks"
)

unique_surp_no_zacks <- count_unique_values(
  data = data1,
  id_vars = c("permno", "datadate"),
  provider_cols = c("ibes_surp_u", "ciq_surp_u", "bb_surp_u", "fset_surp_u"),
  tolerance = 0.015,
  output_col_name = "unique_surp_no_zacks"
)

data3 <- data2 |>
  left_join(unique_act_no_zacks, by = c("permno", "datadate")) |>
  left_join(unique_mean_no_zacks, by = c("permno", "datadate")) |>
  left_join(unique_surp_no_zacks, by = c("permno", "datadate"))


# Compute unique counts without Bloomberg --------------------------------------
message("\n6. Computing unique counts excluding Bloomberg...")

unique_act_no_bb <- count_unique_values(
  data = data1,
  id_vars = c("permno", "datadate"),
  provider_cols = c("ibes_actual_u", "zacks_actual_u", "ciq_actual_u", "fset_actual_u"),
  tolerance = 0.015,
  output_col_name = "unique_act_no_bb"
)

unique_mean_no_bb <- count_unique_values(
  data = data1,
  id_vars = c("permno", "datadate"),
  provider_cols = c("ibes_mean_u", "zacks_mean_u", "ciq_mean_u", "fset_mean_u"),
  tolerance = 0.015,
  output_col_name = "unique_mean_no_bb"
)

unique_surp_no_bb <- count_unique_values(
  data = data1,
  id_vars = c("permno", "datadate"),
  provider_cols = c("ibes_surp_u", "zacks_surp_u", "ciq_surp_u", "fset_surp_u"),
  tolerance = 0.015,
  output_col_name = "unique_surp_no_bb"
)

data4 <- data3 |>
  left_join(unique_act_no_bb, by = c("permno", "datadate")) |>
  left_join(unique_mean_no_bb, by = c("permno", "datadate")) |>
  left_join(unique_surp_no_bb, by = c("permno", "datadate"))


# Write final output -----------------------------------------------------------
message("Writing final dataset to disk...")

write_dta(data4, glue("{data_path}/unique_counts.dta"))
message(glue("    ✓ Wrote Stata file: {data_path}/unique_counts.dta"))

arrow::write_parquet(data4, glue("{data_path}/unique_counts.parquet"))
message(glue("    ✓ Wrote Parquet file: {data_path}/unique_counts.parquet"))

