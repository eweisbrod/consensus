# ==============================================================================
# 006-compute-daily-taq-vars.R
#
# Purpose:
#   Compute daily order-imbalance and intraday-volatility summaries from
#   WCT (Wide Consolidated Trades) minute-level parquet files. For each
#   year-month from 2004-01 to 2023-12, build a per-month .parquet of
#   firm-day OIB measures, then concatenate all months into a single
#   oib-data-all.parquet for downstream scripts.
#
# Inputs (from WCT_DIR):
#   wct_<YYYYMM>*_min.parquet                (per-day WCT minute data;
#                                            5,000+ files across the sample)
#
# Inputs (from WRDS, metadata only):
#   List of taqmsec.wct_<YYYYMMDD> tables    (used to enumerate the months
#                                            available; no data downloaded)
#
# Outputs (to WCT_DIR):
#   oib-data-<YYYYMM>.parquet                (per-month OIB summary;
#                                            one per month in the sample)
#   oib-data-all.parquet                     (concatenated all months;
#                                            consumed by 007)
#
# Notes:
#   - Reads WCT_DIR from .env via the dotenv package.
#   - Outputs land in WCT_DIR alongside the input WCT files. This is a
#     departure from the rest of the pipeline (where outputs go to DATA_DIR);
#     it is intentional because the OIB summaries are tightly coupled to
#     the WCT inputs and are not specific to this project (other research
#     projects use the same WCT directory).
#   - WRDS credentials read via the keyring package -- store them once with
#     keyring::key_set("WRDS_user") and key_set("WRDS_pw").
#   - monthly_oib() is cache-aware: if oib-data-<YYYYMM>.parquet already
#     exists in WCT_DIR, the month is skipped. Delete the file to force
#     recomputation.
# ==============================================================================

#--------------- Set Up --------------------------------------------------------
library(glue)
library(DBI)
library(dbplyr)
library(duckdb)
library(RPostgres)
library(slider)
library(tidyverse)
library(dotenv)

# Load .env (project root, one level above src/) and read pipeline paths.
load_dot_env()
wct_path <- Sys.getenv("WCT_DIR")


#Define monthly_oib function ---------------------------------------------------
#This function computes daily summaries of intraday volatility and order
#imbalances using DuckDB. Skips months whose output is already on disk.

monthly_oib <- function(yearmonth) {

  #read the path to your WCT directory from the environment
  wct_path = Sys.getenv("WCT_DIR")

  #cache-aware skip: if this month's output already exists, no work to do
  out_path <- glue::glue("{wct_path}/oib-data-{yearmonth}.parquet")
  if (file.exists(out_path)) {
    cat(glue::glue("'{out_path}' already exists -- skipping.\n\n"))
    return(invisible(NULL))
  }

    # Connect to a temporary DuckDB instance in memory
    duck_con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    
      # Record the start time
      start_time <- Sys.time()
      
    # Execute all the steps inside DuckDB  
     nobs <- DBI::dbExecute(duck_con, dbplyr::sql(glue::glue("
        COPY ( 
        WITH 
        -- 1) Read just that month's parquet and compute the 5‑min interval
        wct_month AS (
          SELECT 
            * EXCLUDE(time_m),
            FLOOR(
              (EXTRACT(EPOCH FROM CAST(min AS TIME))
               - EXTRACT(EPOCH FROM TIME '04:00:00')
              )/300
            ) AS interval
          FROM read_parquet('{wct_path}/wct_{yearmonth}*_min.parquet')
          WHERE price > 0
        ),
    -- 2) Interval opens/closes
    intervals AS (
      SELECT DISTINCT
        date,
        sym_root,
        COALESCE(sym_suffix, '') AS sym_suffix,
        interval,
        -- first/last minute stamp
        FIRST_VALUE(min) OVER w AS first_time,
        LAST_VALUE(min)  OVER w AS last_time,
        -- first/last price in that interval
        FIRST_VALUE(price) OVER w AS open_price,
        LAST_VALUE(price)  OVER w AS close_price
      FROM wct_month
      WINDOW w AS (
        PARTITION BY date, sym_root, sym_suffix, interval
        ORDER BY min
      )
    ),
    -- 3) Attach lag_price per symbol/day
    with_lag AS (
      SELECT
        *,
        CASE
          WHEN interval = MIN(interval)
            OVER (
              PARTITION BY date, sym_root, sym_suffix
              ORDER BY interval
            )
          THEN FIRST_VALUE(open_price)
            OVER (
              PARTITION BY date, sym_root, sym_suffix
              ORDER BY interval
            )
          ELSE LAG(close_price)
            OVER (
              PARTITION BY date, sym_root, sym_suffix
              ORDER BY interval
            )
        END AS lag_price
      FROM intervals
    ),
        -- 4) Sum of squared log‐returns
        returns AS (
          SELECT
            date, sym_root, sym_suffix,
            SUM(POWER(LOG(1 + (close_price - lag_price)/lag_price),2)) AS logret_ss_all,
            SUM(
              CASE 
                WHEN last_time >= '09:30:00' AND last_time < '16:00:00'
                THEN POWER(LOG(1 + (close_price - lag_price)/lag_price),2)
                ELSE 0
              END
            ) AS logret_ss_rh
          FROM with_lag
          GROUP BY date, sym_root, sym_suffix
        ),
        -- 5) Daily volumes & trades
        volumes AS (
      SELECT
        date,
        sym_root,
        COALESCE(sym_suffix,'')        AS sym_suffix,
    
        -- daily trade counts
        SUM(total_trades)  AS tot_trades,
        SUM(b_trades)      AS b_trades,
        SUM(s_trades)      AS s_trades,
        SUM(r_trades)      AS r_trades,
        SUM(rb_trades)     AS rb_trades,
        SUM(rs_trades)     AS rs_trades,
        SUM(lb_trades)     AS lb_trades,
        SUM(ls_trades)     AS ls_trades,
    
        -- daily share volumes
        SUM(total_shares)  AS tot_vol,
        SUM(b_shares)      AS b_vol,
        SUM(s_shares)      AS s_vol,
        SUM(r_shares)      AS r_vol,
        SUM(rb_shares)     AS rb_vol,
        SUM(rs_shares)     AS rs_vol,
        SUM(lb_shares)     AS lb_vol,
        SUM(ls_shares)     AS ls_vol,
    
        -- daily dollar volumes (all hours)
        SUM(b_dv)          AS b_dv_all,
        SUM(s_dv)          AS s_dv_all,
        SUM(r_dv)          AS r_dv_all,
        SUM(rb_dv)         AS rb_dv_all,
        SUM(rs_dv)         AS rs_dv_all,
        SUM(lb_dv)         AS lb_dv_all,
        SUM(ls_dv)         AS ls_dv_all,
    
        -- daily dollar volumes (regular hours 09:30–16:00)
        SUM(
          CASE 
            WHEN min >= '09:30:00' AND min < '16:00:00' 
            THEN b_dv ELSE 0 
          END
        ) AS b_dv_rh,
        SUM(
          CASE 
            WHEN min >= '09:30:00' AND min < '16:00:00' 
            THEN s_dv ELSE 0 
          END
        ) AS s_dv_rh,
        SUM(
          CASE 
            WHEN min >= '09:30:00' AND min < '16:00:00' 
            THEN r_dv ELSE 0 
          END
        ) AS r_dv_rh,
        SUM(
          CASE 
            WHEN min >= '09:30:00' AND min < '16:00:00' 
            THEN rb_dv ELSE 0 
          END
        ) AS rb_dv_rh,
        SUM(
          CASE 
            WHEN min >= '09:30:00' AND min < '16:00:00' 
            THEN rs_dv ELSE 0 
          END
        ) AS rs_dv_rh,
        SUM(
          CASE 
            WHEN min >= '09:30:00' AND min < '16:00:00' 
            THEN lb_dv ELSE 0 
          END
        ) AS lb_dv_rh,
        SUM(
          CASE 
            WHEN min >= '09:30:00' AND min < '16:00:00' 
            THEN ls_dv ELSE 0 
          END
        ) AS ls_dv_rh
    
      FROM wct_month
      GROUP BY date, sym_root, sym_suffix
    )
    -- Final join + compute OIBs
    SELECT
      v.date,
      v.sym_root,
      v.sym_suffix,
      v.b_dv_all,
      v.s_dv_all,
      v.rb_dv_all,
      v.rs_dv_all,
      v.lb_dv_all,
      v.ls_dv_all,
      r.logret_ss_all,
      r.logret_ss_rh,
    
      -- Order imbalance (all hours)
      CASE 
        WHEN (v.b_dv_all + v.s_dv_all) > 0 
          THEN (v.b_dv_all - v.s_dv_all) / (v.b_dv_all + v.s_dv_all)
        ELSE NULL
      END AS oib_all,
    
      CASE 
        WHEN (v.rb_dv_all + v.rs_dv_all) > 0 
          THEN (v.rb_dv_all - v.rs_dv_all) / (v.rb_dv_all + v.rs_dv_all)
        ELSE NULL
      END AS r_oib_all,
    
      CASE 
        WHEN (v.lb_dv_all + v.ls_dv_all) > 0 
          THEN (v.lb_dv_all - v.ls_dv_all) / (v.lb_dv_all + v.ls_dv_all)
        ELSE NULL
      END AS l_oib_all,
    
      -- Order imbalance (regular hours)
      CASE 
        WHEN (v.b_dv_rh + v.s_dv_rh) > 0 
          THEN (v.b_dv_rh - v.s_dv_rh) / (v.b_dv_rh + v.s_dv_rh)
        ELSE NULL
      END AS oib_rh,
    
      CASE 
        WHEN (v.rb_dv_rh + v.rs_dv_rh) > 0 
          THEN (v.rb_dv_rh - v.rs_dv_rh) / (v.rb_dv_rh + v.rs_dv_rh)
        ELSE NULL
      END AS r_oib_rh,
    
      CASE 
        WHEN (v.lb_dv_rh + v.ls_dv_rh) > 0 
          THEN (v.lb_dv_rh - v.ls_dv_rh) / (v.lb_dv_rh + v.ls_dv_rh)
        ELSE NULL
      END AS l_oib_rh
    
    FROM volumes AS v
    LEFT JOIN returns AS r
      USING (date, sym_root, sym_suffix)
    ORDER BY  v.date,v.sym_root, v.sym_suffix)
      TO '{wct_path}/oib-data-{yearmonth}.parquet'
      (FORMAT PARQUET);
      "))) 
     
    #record how long the DuckDB work took 
    end_time <- Sys.time()
    mins <- as.numeric(difftime(end_time, start_time, units = "mins"))
    
    #Output a completion message
    message(glue::glue(
      "'{wct_path}/oib-data-{yearmonth}.parquet' created with {nobs} rows in {round(mins, 2)} minutes.\n"
    ))

# Disconnect from DuckDB (shutting down the in-memory instance)
DBI::dbDisconnect(duck_con, shutdown = TRUE)


}


# Create a list of months to apply the function to  ----------------------------

#Disconnect from WRDS if there is an existing connection
if(exists("wrds")){
  dbDisconnect(wrds)  # because otherwise WRDS might time out
}

#Connect to WRDS
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user=keyring::key_get("WRDS_user"),
                  password=keyring::key_get("WRDS_pw"),
                  sslmode='require',
                  dbname='wrds')
wrds  # checking if connection exists


#Create a list all the wct tables in WRDS
table_list <- wrds  |> 
  DBI::dbListObjects(DBI::Id(schema = 'taqmsec')) |> 
  dplyr::pull(table) |> 
  purrr::map(~slot(.x, 'name'))  |> 
  dplyr::bind_rows()  |>  
  filter(str_detect(table, "^wct_\\d{8}$"))

#Filter the table_list to a subset to be collected
my_list <- table_list |> 
  filter(as.integer(substr(table, 5, 8)) >= 2004) |> 
  filter(as.integer(substr(table, 5, 8)) < 2024) |> 
  mutate(table = substr(table, 5, 10)) |>
  distinct() |> 
  pull(table) 


#Apply the monthly_oib function to the list of months --------------------------

#relevant libraries for parallel processing
library(purrr)
library(mirai)

#initialize 6 parallel workers
daemons(6)

#walk the month list in parallel
walk(my_list,in_parallel(\(x) monthly_oib(x), monthly_oib=monthly_oib))

#shut down the parallel workers
daemons(0)

#Combine monthly parquet files -------------------------------------------------


# Connect to a DuckDB instance in memory 
duck_con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

#combine all the monthly files and output to a single parquet
nobs <- DBI::dbExecute(duck_con, dbplyr::sql(glue::glue("
        COPY ( SELECT * 
          FROM read_parquet('{wct_path}/oib-data-*.parquet')
          ORDER BY  date,sym_root, sym_suffix)
          TO '{wct_path}/oib-data-all.parquet'
          (FORMAT PARQUET);
          ")))

# Disconnect from DuckDB (shutting down the in-memory instance)
DBI::dbDisconnect(duck_con, shutdown = TRUE)

