# ==============================================================================
# 008-compute-mrt.R
#
# Purpose:
#   Compute minute-by-minute Market-Response-Time (MRT) features around
#   each earnings announcement. For each firm-event in all_five3, build
#   60-minute-bucket aggregates of WCT-trade-minute parquet data
#   spanning a window from 16h pre-announcement to 32h post-announcement,
#   stitch them across years, and write a single mrt_all.parquet.
#
# Inputs (from WCT_DIR):
#   wct_<YYYYMMDD>_min.parquet               (per-day WCT minute files)
#
# Inputs (from DATA_DIR):
#   all_five3.sas7bdat                       (output of 004; provides
#                                            permno, best_anndats, best_anntims,
#                                            sym_root, sym_suffix per firm-qtr)
#
# Inputs (from RAW_DATA_DIR, cached):
#   crsp-dsi-dates.parquet                   (CRSP daily index dates from WRDS;
#                                            built first time, cached after)
#
# Working file (in WCT_DIR):
#   mrt.duckdb                               (persistent DuckDB workspace
#                                            where per-year tables are
#                                            staged before the final
#                                            UNION ALL into mrt_all.parquet)
#
# Outputs (to DATA_DIR):
#   mrt_all.parquet
#   mrt_all.dta
#
# Notes:
#   - Reads RAW_DATA_DIR, DATA_DIR, WCT_DIR from .env via the dotenv package.
#   - The CRSP-dsi WRDS pull is cached -- if the parquet exists in raw_data
#     the WRDS pull (and credential prompt) are skipped on re-runs. Delete
#     the file to force re-pull.
#   - WRDS credentials read via the keyring package -- store them once with
#     keyring::key_set("WRDS_user") and key_set("WRDS_pw").
#   - The persistent DuckDB file (mrt.duckdb) lives in WCT_DIR alongside
#     the WCT inputs because it's a large transient workspace that doesn't
#     belong in the JAR data package.
# ==============================================================================

#--------------- Set Up --------------------------------------------------------
library(glue)
library(DBI)
library(dbplyr)
library(duckdb)
library(RPostgres)
library(data.table)
library(tidyverse)
library(dotenv)

# Load .env (project root, one level above src/) and read pipeline paths.
load_dot_env()
data_path <- Sys.getenv("DATA_DIR")
raw_path  <- Sys.getenv("RAW_DATA_DIR")
wct_path  <- Sys.getenv("WCT_DIR")


# Create trading minute calendar -----------------------------------------------

# Cache-aware load of the CRSP-dsi date list -- skip the WRDS pull on re-runs.
crspdates_cache <- glue("{raw_path}/crsp-dsi-dates.parquet")
if (file.exists(crspdates_cache)) {
  message(glue("Loading cached crspdates from {crspdates_cache}"))
  crspdates <- arrow::read_parquet(crspdates_cache) |> as.data.table()
} else {
  wrds <- dbConnect(Postgres(),
                    host='wrds-pgdata.wharton.upenn.edu',
                    port=9737,
                    user=keyring::key_get("WRDS_user"),
                    password=keyring::key_get("WRDS_pw"),
                    sslmode='require',
                    dbname='wrds')
  wrds  # checking if connection exists

  # Create a trading calendar from crsp dsi dates
  crspdates <- tbl(wrds, in_schema("crsp", "dsi")) |>
    distinct(date) |>
    mutate(td_idx = row_number()) |>
    collect() |>
    as.data.table()

  arrow::write_parquet(crspdates, crspdates_cache,
                       compression = "gzip", compression_level = 5)
  DBI::dbDisconnect(wrds)
}


# Ensure 'date' is Date type
crspdates[, date := as.Date(date)]

# Filter to year >= 2003 and sort
crspdates <- crspdates[year(date) >= 2003]
setorder(crspdates, date)

# ---- 2. Build trade_min_calendar without hms, using sprintf ----
# intraday window 04:00 (240 minutes) to 19:59 (1199 minutes) inclusive
minute_of_day_seq <- seq(4 * 60, 19 * 60 + 59)  # 240 .. 1199

# cross join dates × minute_of_day
trade_min_calendar <- data.table::CJ(date = crspdates$date,
                                     minute_of_day = minute_of_day_seq,
                                     sorted = FALSE)

# ensure ordering
setkey(trade_min_calendar, date, minute_of_day)

# create string "HH:MM:SS" for matching WCT's min
trade_min_calendar[, min := sprintf("%02d:%02d:00",
                                    minute_of_day %/% 60,
                                    minute_of_day %% 60)]

# build full datetime
trade_min_calendar[, datetime := make_datetime(
  year = year(date),
  month = month(date),
  day = day(date),
  hour = minute_of_day %/% 60,
  min = minute_of_day %% 60,
  sec = 0
)]

# global sequential index n
trade_min_calendar[, n := .I]

# drop the helper column
trade_min_calendar[, minute_of_day := NULL]

setcolorder(trade_min_calendar, c("date", "min", "datetime", "n"))
trade_min_calendar

# Load FDP sample --------------------------------------------------------------

#Load allfive dataset and select dates and identifiers
data1 <- haven::read_sas(glue("{data_path}/all_five3.sas7bdat")) |>
  rename_with(tolower)


#Select only the variables I need and filter to 2004 onwards
data2 <- data1 |> 
  select(permno,best_anndats,best_dttm=best_anntims,sym_root,sym_suffix) |> 
  drop_na(best_dttm,sym_root) |> 
  filter(sym_root != "") |> 
  filter(year(best_anndats) > 2003) |> 
  filter(year(best_anndats) < 2024) 


# Define Monthly Collection Function -------------------------------------------

collect_mrt <- function(
  sample,                   # sample must contain sym_root, sym_suffix
  id_vars = NULL,           # any columns in `sample` to carry through
  dt_col  = "event_dttm",   # which datetime column to anchor on
  trade_min_calendar,       # calendar with date, min, datetime, n
  wct_parquet_dir,          # directory containing wct_YYYYMMDD_min.parquet
  interval = 60L,           # minutes per bin
  cushion_before = 960L,    # minutes for initial lag window
  main_window_start = 0L,
  main_window_end = 1919L,
  duckdb_path = ":memory:",
  output_path) {
  

  
  #—— Prep & sanity checks ——  
  setDT(sample)
  setDT(trade_min_calendar)
  #Make temporary join column
  trade_min_calendar[, join_col := datetime]
  
  #grouping columns for the expanded data.table
  group_cols <- c(id_vars, dt_col, "sym_root", "sym_suffix")
  
  #Connect to DuckDB
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = duckdb_path, read_only = FALSE)
  dbExecute(con, "SET external_threads = 10;")
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # build the full minute offset vector
  rel_offsets <- seq(main_window_start - cushion_before, main_window_end)
  if (length(rel_offsets) %% interval != 0) {
    stop("Window length (", length(rel_offsets),
         ") must be divisible by interval (", interval, ").")
  }
  cushion_bins <- cushion_before / interval
  if (cushion_bins != floor(cushion_bins)) {
    stop("cushion_before must be a multiple of interval.")
  }
  start_bin <- ceiling(main_window_start / interval)
  end_bin   <- floor(main_window_end / interval)
  bins      <- seq(start_bin, end_bin)
  
  # single grouping string for SQL
  key_cols <- paste(c(id_vars, dt_col, "sym_root","sym_suffix"), collapse = ", ")
  
  #pull unique years from the sample
  years <- unique(year(sample[,get(dt_col)]))
  
  walk(years, function(yr) {
    
    # Record the start time
    start_time <- Sys.time()
    
    events <- sample[
      year(get(dt_col)) == yr,
      c(id_vars, dt_col, "sym_root","sym_suffix"),
      with = FALSE
    ][, join_col := get(dt_col)]
    if (nrow(events)==0L) next
    
    setkey(trade_min_calendar, join_col)
    event_lookup <- trade_min_calendar[
      events,
      roll = -Inf,
      on   = .(join_col = join_col)
    ]
    event_lookup[, `:=`(
      evt_minute_n       = n,
      evt_trade_datetime = datetime
    )][, c("n","datetime","join_col") := NULL]
    
    # expand, grouping by group_cols
    expanded <- event_lookup[
      , .(
        minute_n = evt_minute_n + rel_offsets,
        td_min   = rel_offsets
      ),
      by = group_cols
    ]    
    
    # re-join to calendar by n to get date+min
    setkey(trade_min_calendar, n)
    expanded <- trade_min_calendar[expanded, on = .(n = minute_n), nomatch = 0L]
    expanded[, `:=`(
      td_bin     = floor(td_min / interval),
      in_cushion = td_min < main_window_start,
      sym_suffix = fifelse(is.na(sym_suffix)|sym_suffix=="","",sym_suffix),
      join_col = NULL
    )]
    
    
    # then write into DuckDB 
    DBI::dbWriteTable(con, "expanded_events",
                      expanded,
                      overwrite = TRUE)
    
    
    # Build all_wct view for exactly the needed days ---------------------------
    days <- sort(unique(expanded$date))
    patterns <- sprintf("'%s/wct_%s_min.parquet'",
                        normalizePath(wct_parquet_dir),
                        format(days, "%Y%m%d"))
    dbExecute(con, glue("
      CREATE OR REPLACE VIEW all_wct AS
        SELECT date,
               sym_root,
               COALESCE(NULLIF(sym_suffix,''),'') AS sym_suffix,
               min,
               price
        FROM read_parquet(ARRAY[{paste(patterns, collapse = ',')}])
    "))
    
    # C) MRT entirely in DuckDB, save as mrt_{yr} ------------------------------
    # build pivot SQL
    pivot_sql <- paste0(vapply(bins, function(b) {
      glue("MAX(CASE WHEN td_bin={b} THEN mrt ELSE 0 END) AS mrt_{b}")
    }, ""), collapse = ",\n        ")
    
    mrt_sql <- glue("
   CREATE OR REPLACE TABLE mrt_{yr} AS
      
 WITH 
  event_price_base AS (
  SELECT DISTINCT
    {key_cols},
    td_min,
    td_bin,
    in_cushion,
    date,
    min,
    w.price
  FROM expanded_events e
  LEFT JOIN all_wct w
  USING(date, sym_root, sym_suffix, min)
  ),

  -- close_price per bin
  interval_close AS (
  SELECT DISTINCT
    {key_cols},
    sym_root,
    sym_suffix,
    td_bin,
    LAST_VALUE(in_cushion) OVER w AS in_cushion,
    LAST_VALUE(price) OVER w AS close_price
  FROM event_price_base
  WHERE price > 0
  WINDOW w AS (
    PARTITION BY {key_cols}, td_bin
    ORDER BY td_min
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
    )
  ), 

  -- compute lags and returns
  with_lag AS (
      SELECT
        *,
        LAG(close_price)
            OVER (
              PARTITION BY {key_cols}
              ORDER BY td_bin
            )
        AS lag_price,
        LOG(1 + (close_price - lag_price)/lag_price) AS logret
      FROM interval_close
    ),
    -- merge the returns back to the base bins and filter to window
  returns AS (
  SELECT DISTINCT 
  e.*,
    COALESCE(w.logret,0) AS logret
  FROM (SELECT DISTINCT 
    {key_cols},
    td_bin
    FROM event_price_base
    WHERE in_cushion = FALSE) e LEFT JOIN with_lag w 
    USING({key_cols}, td_bin)
    ORDER BY e.{dt_col}, e.sym_root, e.td_bin),
   stats AS (
        SELECT
          {key_cols},
          td_bin,
          -- cumulative return
          EXP(
            SUM(logret) OVER cs
          ) - 1 AS cumret
        FROM returns
        WINDOW
          cs AS (
            PARTITION BY {key_cols}
            ORDER BY td_bin
            ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
          )
      ),
        fractions AS (
        SELECT
          {key_cols},
          td_bin,
          cumret,
          LAST_VALUE(cumret) OVER fs AS final_ret,
          CASE
            WHEN final_ret <> 0
            THEN LEAST(GREATEST(cumret/final_ret, -1),1)
            ELSE NULL
          END AS mrt
        FROM stats
        WINDOW
          fs AS (
            PARTITION BY {key_cols}
            ORDER BY td_bin
            ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
          )
      )

      SELECT
        {key_cols},
        MAX(final_ret) AS final_ret,
        {pivot_sql},
        -- sum all but last bin, +0.5
        SUM(
          CASE WHEN td_bin < {end_bin} THEN mrt ELSE 0 END
        ) + 0.5 AS MRT
      FROM fractions
      GROUP BY {key_cols};
    ")
    dbExecute(con, mrt_sql)
    
    #record how long the work took 
    end_time <- Sys.time()
    mins <- as.numeric(difftime(end_time, start_time, units = "mins"))
    
    #Output a completion message
    message(glue::glue(
      "Wrote mrt_{yr} in {round(mins, 2)} minutes.\n"))
    
    message(glue(""))
  })
  
  # D) UNION & DUMP one Parquet ---------------------------------------------
  union_sql <- paste0("SELECT * FROM mrt_", years, collapse = "\nUNION ALL\n")
  dbExecute(con, glue("
    CREATE OR REPLACE TABLE all_mrt AS
      {union_sql}
  "))
  dbExecute(con, glue("
    COPY all_mrt
      TO '{output_path}/mrt_all.parquet'
      (FORMAT PARQUET, COMPRESSION 'GZIP');
  "))
  message(glue("🚀 Wrote final MRT to {output_path}/mrt_all.parquet"))
  }
  



# ------------------------------------------------------------------------------

# Call function ----------------------------------------------------------------

collect_mrt(sample=data2,                   # sample must contain sym_root, sym_suffix
    id_vars = c("permno","best_anndats"),   # any columns in `sample` to carry through
    dt_col  = "best_dttm",   # which datetime column to anchor on
    trade_min_calendar=trade_min_calendar,       # calendar with date, min, datetime, n
    wct_parquet_dir=wct_path,          # directory containing wct_YYYYMMDD_min.parquet
    interval = 60L,           # minutes per bin
    cushion_before = 960L,    # minutes for initial lag window
    main_window_start = 0L,
    main_window_end = 1919L,
    duckdb_path = glue("{wct_path}/mrt.duckdb"),
    output_path = data_path)

# Read and check it ------------------------------------------------------------

check <- arrow::read_parquet(glue("{data_path}/mrt_all.parquet"))

#Write to stata
haven::write_dta(check,glue("{data_path}/mrt_all.dta"))
