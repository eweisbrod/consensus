# ==============================================================================
# 016-export-sample-identifiers.R
#
# Purpose:
#   Export identifiers for every firm-quarter in the final regression sample
#   (94,030 firm-quarters), along with announcement timing fields and the
#   FDP-specific link identifiers. Required by the
#   Journal of Accounting Research Data and Code Sharing Policy.
#
# Inputs (from DATA_DIR):
#   2025_12_16_firm_qtr_regdata.parquet      (output of 011; the 94,030
#                                            firm-quarter common sample)
#
# Outputs (to OUTPUT_DIR):
#   sample-identifiers.csv                   (one row per firm-quarter
#                                            with all CCM, CRSP, TAQ, and
#                                            five-FDP link IDs plus
#                                            announcement timestamps)
#
# Notes:
#   - Reads DATA_DIR and OUTPUT_DIR from .env via the dotenv package.
#   - One row per (gvkey, datadate). Columns are the identifiers used as
#     join keys.
# ==============================================================================

library(dplyr)
library(glue)
library(arrow)
library(dotenv)

load_dot_env()
data_path   <- Sys.getenv("DATA_DIR")
output_path <- Sys.getenv("OUTPUT_DIR")


# Load final firm-quarter sample ----------------------------------------------

fq <- read_parquet(glue("{data_path}/2025_12_16_firm_qtr_regdata.parquet"))


# Select identifier columns ---------------------------------------------------
# Order groups columns by source so the CSV reads top-down: CCM/CRSP spine,
# fiscal calendar, announcement timing, then per-FDP link IDs (in the order
# 002 introduces each FDP), then TAQ.

ids <- fq |>
  select(
    # CCM / CRSP / Compustat spine
    gvkey, permno, cusip, ncusip, conm,
    crsp_ticker, crsp_comnam, sic,
    # Fiscal calendar
    datadate, yearqtr, fyearq, fqtr, rdq,
    # Announcement timing (IBES is the canonical reported time)
    ibes_anndats, ibes_anntims,
    # IBES link ID (002 Part 1)
    ibes_ticker,
    # FactSet link ID (002 Part 2): fsym_id is FactSet's permanent symbol
    # identifier.
    fsym_id,
    # Zacks link ID (002 Part 3): zid is Zacks's company identifier.
    zid,
    # Capital IQ link IDs (002 Part 4): ciq_companyid is Capital IQ's
    # company identifier (linked to gvkey via wrds_ciqsymbol). ciq_basis
    # records whether the CIQ consensus is on a NORM or GAAP basis.
    ciq_companyid, ciq_basis,
    # Bloomberg link IDs (002 Part 5): both forms ship because the BQL
    # full ticker is the Bloomberg-side join key while bb_ticker is the
    # short equity ticker.
    bb_ticker_full, bb_ticker,
    # TAQ link IDs
    sym_root, sym_suffix
  ) |>
  # ibes_anntims arrives as seconds-since-midnight; format as HH:MM:SS
  # so the CSV is human-readable.
  mutate(
    ibes_anntims = sprintf(
      "%02d:%02d:%02d",
      as.integer(as.numeric(ibes_anntims) %/% 3600),
      as.integer((as.numeric(ibes_anntims) %% 3600) %/% 60),
      as.integer(as.numeric(ibes_anntims) %% 60)
    )
  ) |>
  arrange(gvkey, datadate)


# Write CSV -------------------------------------------------------------------

write.csv(ids, glue("{output_path}/sample-identifiers.csv"),
          row.names = FALSE, na = "")

message(glue("    ✓ Wrote sample identifiers ({nrow(ids)} rows, ",
             "{ncol(ids)} columns): {output_path}/sample-identifiers.csv"))
message("\n=== Script completed successfully ===")
