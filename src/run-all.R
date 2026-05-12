# run-all.R — Run the full pipeline; each script writes its own log
# ============================================================================
# HOW TO RUN: Open this script in RStudio and run interactively
# (Ctrl+A, Ctrl+Enter), or step through line by line.
#
# Each numbered script is executed in a fresh child process via the
# appropriate batch_run helper from utils.R:
#   batch_run()       -> R CMD BATCH --vanilla, writes .Rout
#   batch_run_sas()   -> sas -SYSIN -LOG, writes -sas.log
#   batch_run_stata() -> stata -b do, writes -stata.log
#
# All three produce the same SAS-log shape: command echo + output
# interleaved + proc.time() / equivalent footer. Per-script logs land in
# log/. A fresh run overwrites the previous run's logs.
# ============================================================================


# Setup ------------------------------------------------------------------------

library(dotenv)
load_dot_env(".env")

source("src/utils.R")  # provides batch_run / batch_run_sas / batch_run_stata

dir.create("log", showWarnings = FALSE, recursive = TRUE)


# Pipeline steps -------------------------------------------------------------

# Each batch_run() spawns R CMD BATCH in a child process. open = FALSE so
# we don't pop up RStudio editor tabs for every script in the pipeline.
# Logs go to log/<script>.Rout — a fresh run overwrites the previous run.
#
# Three runner helpers from utils.R, one per language:
#   batch_run("src/foo.R")        -> log/foo.Rout         (R CMD BATCH)
#   batch_run_sas("src/foo.sas")  -> log/foo-sas.log      (sas -SYSIN -LOG)
#   batch_run_stata("src/foo.do") -> log/foo-stata.log    (stata -b do)

batch_run("src/001-prepare-raw-ciq-data.R",
          log_path = "log/001-prepare-raw-ciq-data.Rout", open = FALSE)

batch_run_sas("src/002-merge-fdp-data.sas",
              log_path = "log/002-merge-fdp-data-sas.log")

batch_run_sas("src/003-append-determinants-other-controls.sas",
              log_path = "log/003-append-determinants-other-controls-sas.log")

batch_run_sas("src/004-append-TAQ-IID-variables.sas",
              log_path = "log/004-append-TAQ-IID-variables-sas.log")

batch_run_sas("src/005-compute-fdp-quality-salience-variables.sas",
              log_path = "log/005-compute-fdp-quality-salience-variables-sas.log")

batch_run("src/006-compute-daily-taq-vars.R",
          log_path = "log/006-compute-daily-taq-vars.Rout", open = FALSE)

batch_run("src/007-compute-abnormal-taq-vars.R",
          log_path = "log/007-compute-abnormal-taq-vars.Rout", open = FALSE)

batch_run("src/008-compute-mrt.R",
          log_path = "log/008-compute-mrt.Rout", open = FALSE)

batch_run("src/009-collect-ravenpack-data.R",
          log_path = "log/009-collect-ravenpack-data.Rout", open = FALSE)

batch_run("src/010-compute-unique-counts.R",
          log_path = "log/010-compute-unique-counts.Rout", open = FALSE)

batch_run("src/011-create-firm-qtr-sample.R",
          log_path = "log/011-create-firm-qtr-sample.Rout", open = FALSE)

batch_run("src/012-create-firm-qtr-fdp-sample.R",
          log_path = "log/012-create-firm-qtr-fdp-sample.Rout", open = FALSE)

batch_run_stata("src/013-regression-analysis.do",
                log_path = "log/013-regression-analysis-stata.log")

batch_run("src/014-portfolio-returns.R",
          log_path = "log/014-portfolio-returns.Rout", open = FALSE)

batch_run("src/015-create-figures.R",
          log_path = "log/015-create-figures.Rout", open = FALSE)

batch_run("src/016-export-sample-identifiers.R",
          log_path = "log/016-export-sample-identifiers.Rout", open = FALSE)


cat("\nPipeline complete. Logs in: log/\n")
