# Consensus

Replication package for **"Consensus? An Examination of Differences in
Earnings Information Across Forecast Data Providers"** by Stephannie
Larocque, Jessica Watkins, and Eric Weisbrod (forthcoming, *Journal of
Accounting Research*).

The paper compares the earnings information produced by five major
forecast data providers (FDPs) — I/B/E/S, Capital IQ, Zacks, Bloomberg,
and FactSet — across 94,030 firm-quarters from 2002 to 2020.

## Pipeline

The pipeline is implemented in three languages with parquet files as the
common interchange format. Each numbered script runs in a fresh child
process via the appropriate `batch_run` helper from `src/utils.R` and
writes a per-script log to `log/`.

| Step | Script | Lang | Purpose |
|---|---|---|---|
| 001 | `001-prepare-raw-ciq-data.R` | R | Stage Capital IQ Xpressfeed data |
| 002 | `002-merge-fdp-data.sas` | SAS | Merge CCM + 5 FDPs |
| 003 | `003-append-determinants-other-controls.sas` | SAS | Add controls |
| 004 | `004-append-TAQ-IID-variables.sas` | SAS | Add TAQ / IID variables |
| 005 | `005-compute-fdp-quality-salience-variables.sas` | SAS | FDP quality / salience |
| 006 | `006-compute-daily-taq-vars.R` | R | Daily TAQ aggregation |
| 007 | `007-compute-abnormal-taq-vars.R` | R | Abnormal volatility / liquidity |
| 008 | `008-compute-mrt.R` | R | Market response timeliness (MRT) |
| 009 | `009-collect-ravenpack-data.R` | R | RavenPack EA article counts |
| 010 | `010-compute-unique-counts.R` | R | Unique-FDP counters |
| 011 | `011-create-firm-qtr-sample.R` | R | Firm-quarter regression sample |
| 012 | `012-create-firm-qtr-fdp-sample.R` | R | FDP-firm-quarter sample |
| 013 | `013-regression-analysis.do` | Stata | Tables 2, 3, 4, 5, 6 |
| 014 | `014-portfolio-returns.R` | R | Table 7 portfolio returns |
| 015 | `015-create-figures.R` | R | Figures  |
| 016 | `016-export-sample-identifiers.R` | R | Sample identifiers (JAR data sharing policy) |

## Quick start

1. Clone the repo.
2. Copy `.example-env` to `.env` and edit the paths to match your machine.
3. Install required packages (see [Prerequisites](#prerequisites)).
4. Open `src/run-all.R` in RStudio and run it (Ctrl+A, Ctrl+Enter), or
   step through each numbered script individually.

A clean run produces:
- One CSV per table panel in `OUTPUT_DIR`.
- One PNG per figure in `OUTPUT_DIR` .
- One log per script in `log/`.

## Prerequisites

- **R** (>= 4.5) — [cran.r-project.org](https://cran.r-project.org/),
  with packages `dplyr`, `tidyverse`, `arrow`, `haven`, `glue`, `dotenv`,
  `RPostgres`, `DBI`, `dbplyr`, `keyring`, `fixest`, `ggrepel`, `readxl`,
  `purrr`.
- **SAS** (>= 9.4).
- **Stata** (>= 17, for the `collect` framework). Install required Stata
  packages once per machine:
  ```stata
  ssc install estout
  ssc install reghdfe
  net install projectpaths, from("https://raw.githubusercontent.com/eweisbrod/projectpaths/main/src/") replace
  net install doenv, from("https://github.com/vikjam/doenv/raw/master/")
  project_paths_list, add project(consensus) path("C:/_git/consensus")
  ```
- **WRDS account** — credentials are stored in your OS keyring under
  service keys `WRDS_user` and `WRDS_pw`. Set them once with
  `keyring::key_set("WRDS_user")` and `keyring::key_set("WRDS_pw")` from
  R.


## Configuration

`.env` :

| Key | Purpose |
|---|---|
| `RAW_DATA_DIR` | Raw WRDS pulls + hand-collected inputs (Nexis Uni xlsx). Cache-aware — the pipeline skips files already present. |
| `DATA_DIR` | Derived intermediate datasets (parquet, sas7bdat, dta). Safe to delete and regenerate. |
| `OUTPUT_DIR` | Tables (CSV) and figures (PNG). |
| `WCT_DIR` | Per-day TAQ minute-summaries from `000-collect-wct-data.R`. Roughly 5,500 daily parquets for 2004-2025. |
| `SAS_WORK_DIR` | (Optional) override SAS's default WORK library — useful when the system temp drive is too small for the pipeline's intermediate datasets. |

## Raw inputs

The numbered pipeline (001-015) consumes raw data produced by separate
collection scripts in `src/` prefixed `000-*`. These are run once per
data refresh:

- `000-collect-bql-data.R` — Bloomberg BQL forecasts and actuals.
- `000-collect-ciq-data.R` — Capital IQ via Xpressfeed.
- `000-collect-factset.R` — FactSet via Snowflake.
- `000-collect-wct-data.R` — WRDS TAQMSEC daily WCT tables.
- `000-collect-master-ccm-dataset.sas` — Compustat fundq, CRSP dsf, and
  the CCM linktable from WRDS via SAS.

These collection scripts require subscription credentials and entitlements. 
The cached raw outputs are retained by the authors so that re-running the 
analysis pipeline does not require re-collecting from the upstream sources,
as upstream sources may experience vintange changes and data backfills over time.

Two inputs in `RAW_DATA_DIR` are hand-collected:

- `NexisUniArticlesByYear.xlsx` — Nexis Uni earnings-related media
  citation counts by FDP and year (used for Figure 2). Hand-collected
  by Stephannie Larocque.
- `fdp_identifiers.sas7bdat` — Bloomberg ticker map linking Russell 3000
  Bloomberg tickers to permno / gvkey / cusip (used by 002 to merge
  Bloomberg BQL data). Hand-collected by Stephannie Larocque and Notre
  Dame student RAs during 2020-2022 via a combination of CUSIP linking,
  fuzzy name matching, and manual lookup.

Notre Dame data analysts Brandon Greenawalt and James Ng provided
research assistance with running and validating several of the data
collection scripts.

## Logging

Every numbered pipeline script writes a per-script log to `log/` in the
project root:

| Language | Mechanism | Output file |
|---|---|---|
| R | `batch_run()` → `R CMD BATCH --vanilla` | `log/<script>.Rout` |
| SAS | `batch_run_sas()` → `sas -SYSIN -LOG` | `log/<script>-sas.log` |
| Stata | `batch_run_stata()` → `stata -b do` | `log/<script>-stata.log` |

A fresh `run-all.R` invocation overwrites the previous run's logs.
