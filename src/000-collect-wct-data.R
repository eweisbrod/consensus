# ==============================================================================
# 000-collect-wct-data.R
#
# Purpose:
#   Pull TAQMSEC daily WRDS consolidated trade & quote (WCT) tables from WRDS 
#   and aggregate them to per-minute summaries (buy/sell/retail/institutional/large 
#   trade counters, VWAP, last price). Writes one parquet file per trading day.
#
# Inputs:
#   WRDS TAQMSEC database tables (via Postgres):
#     taqmsec.wct_<YYYYMMDD>  (daily, one table per trading day)
#
# Outputs:
#   wct_<YYYYMMDD>_min.parquet  (one per trading day, 5535 files for trading
#                                days 2004-01-02 through 2025-12-31)
#
# Notes:
#   - Run by Eric Weisbrod.
#   - Run iteratively from 2025-07-30 through 2026-01-14 to cover the full
#     2004-2025 sample period; the year filter near the bottom of the script
#     was adjusted between runs to step through year ranges.
#   - Requires WRDS access (Postgres connection authenticated via keyring).
#   - Reads WCT_DIR from .env via the dotenv package to set the parquet output
#     path. WCT outputs are kept separate from RAW_DATA_DIR -- the per-trading-
#     day file count (5,000+) makes it impractical to consolidate them into the
#     main raw tier.
#   - Skipped on replication runs; outputs are pre-built.
# ==============================================================================

#--------------- Set Up --------------------------------------------------------
library(glue)
library(DBI)
library(dbplyr)
library(duckdb)
library(RPostgres)
library(tidyverse)
library(dotenv)

# Load .env (project root, one level above src/) and set the WCT output path.
load_dot_env()
wct_path <- Sys.getenv("WCT_DIR")



# Connect to a DuckDB instance in memory (or point to a file if you prefer persistence)
my_duckdb <- DBI::dbConnect(duckdb::duckdb(), ":memory:")


# Sign in to WRDS --------------------------------------------------------------


#If this is your first time using Keyring to log onto WRDS, uncomment and run
# keyring::key_set("WRDS_user")
# keyring::key_set("WRDS_pw")

if(exists("wrds")){
  dbDisconnect(wrds)  # because otherwise WRDS might time out
}

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user=keyring::key_get("WRDS_user"),
                  password=keyring::key_get("WRDS_pw"),
                  sslmode='require',
                  dbname='wrds')
wrds  # checking if connection exists



#Define Collection Function ----------------------------------------------------

#table_name = a wrds tbl of wct data
#tol is tolerance for floating point comparisons 
#wrds_conn = object name of wrds connection
#duck_conn = object name of duckdb connection
collect_wct <- function(table_name,tol=1e-8,wrds_conn=wrds,duck_conn=my_duckdb){ 
  
  # --- Part 1: Clean and compute derived columns ---
  
  # Record the start time
  start_time <- Sys.time()
  
  df_clean <- 
    tbl(wrds_conn, in_schema("taqmsec", table_name)) |> 
    filter(
      (
        # Regular hours: between 09:30:00 and 16:00:00,
        # and trade condition does not contain any undesired characters
        (time_m >= "09:30:00" & time_m <= "16:00:00" &
           !str_detect(tr_scond, "[OQ6MNVZ579AY]"))
        |
          # Extended hours: before 09:30:00 or after 16:00:00, and condition contains "T"
          ((time_m < "09:30:00" | time_m > "16:00:00") &
             str_detect(tr_scond, "T"))
      ) &
        size > 0 & price > 1 &
        !is.na(nbb) & !is.na(nbo) &
        ((nbo - nbb) > 0)
    ) |> 
    # Create the minute variable by truncating the time to the minute.
    mutate(
      size = sql("size::numeric"),
      min = floor_date(time_m, unit = "minute"),
      midpoint = (nbo + nbb) / 2,
      spread = nbo - nbb,
      tradelocation = (price - nbb) / spread,
      DV = size * price
    )  |> 
    group_by(sym_root, sym_suffix) |>
    window_order(time_m) |>
    mutate(
      lagprice = lag(price, order_by = time_m),  # lag price for each symbol),
      tick_raw = case_when(
        row_number() == 1 ~ NA_real_,
        price > lag(price, order_by = time_m) ~ 1,
        price < lag(price, order_by = time_m) ~ -1,
        TRUE ~ NA_real_
      )
    ) |>
    fill(tick_raw, .direction = "down") |>
    rename(tick = tick_raw) |>
    ungroup() 
    
  
  # --- Part 2: Create row-wise counters based on trade conditions ---
  
  
  df_counters <- df_clean  |> 
    mutate(
      # Define basic buy/sell classification:
      is_buy_direct   = tradelocation > 0.5,
      is_sell_direct  = tradelocation < 0.5,
      is_midpoint     = abs(tradelocation - 0.5) < tol,
      # For midpoint trades, use the computed tick (if available) to classify:
      is_buy_mid      = is_midpoint & (tick == 1),
      is_sell_mid     = is_midpoint & (!is.na(tick) & tick != 1),
      
      # Total trade counters (each row contributes one trade)
      total_trades = 1,
      total_shares = size,
      total_dv     = DV,
      
      # Large trades (DV at least 50K)
      l_trades = if_else(DV >= 50000, 1, 0),
      l_shares = if_else(DV >= 50000, size, 0),
      l_dv     = if_else(DV >= 50000, DV, 0),
      
      # BUY counters: count rows classified as a direct buy or a midpoint buy.
      b_trades = if_else(is_buy_direct | is_buy_mid, 1, 0),
      b_shares = if_else(is_buy_direct | is_buy_mid, size, 0),
      b_dv     = if_else(is_buy_direct | is_buy_mid, DV, 0),
      # Large-Buy (LB) counters for buys with DV >=50K
      lb_trades = if_else((is_buy_direct | is_buy_mid) & (DV >= 50000), 1, 0),
      lb_shares = if_else((is_buy_direct | is_buy_mid) & (DV >= 50000), size, 0),
      lb_dv     = if_else((is_buy_direct | is_buy_mid) & (DV >= 50000), DV, 0),
      
      # SELL counters: count rows classified as a direct sell or a midpoint sell.
      s_trades = if_else(is_sell_direct | is_sell_mid, 1, 0),
      s_shares = if_else(is_sell_direct | is_sell_mid, size, 0),
      s_dv     = if_else(is_sell_direct | is_sell_mid, DV, 0),
      # Large-Sell (LS) counters for sells with DV >=50K
      ls_trades = if_else((is_sell_direct | is_sell_mid) & (DV >= 50000), 1, 0),
      ls_shares = if_else((is_sell_direct | is_sell_mid) & (DV >= 50000), size, 0),
      ls_dv     = if_else((is_sell_direct | is_sell_mid) & (DV >= 50000), DV, 0),
      
      # Retail trade conditions.
      # For buys: check if exchange is 'D', tradelocation > 0.6 and the last two decimal digits of price
      # (computed via 100*(price modulo 0.01)) are neither 0 nor 1.
      retail_buy = if_else(ex == "D" &
                             tradelocation > 0.6 &
                             (100 * (price %% 0.01)) != 0 &
                             (100 * (price %% 0.01)) != 1, 1, 0),
      # For sells: similar check with tradelocation < 0.4.
      retail_sell = if_else(ex == "D" &
                              tradelocation < 0.4 &
                              (100 * (price %% 0.01)) != 0 &
                              (100 * (price %% 0.01)) != 1, 1, 0),
      
      # Retail counters: if a row qualifies as a retail trade, add to the retail counters.
      # Here we add these counts only if the row is otherwise classified as a buy or a sell.
      r_trades = if_else((is_buy_direct | is_buy_mid | is_sell_direct | is_sell_mid) &
                           (retail_buy == 1 | retail_sell == 1), 1, 0),
      r_shares = if_else((is_buy_direct | is_buy_mid | is_sell_direct | is_sell_mid) &
                           (retail_buy == 1 | retail_sell == 1), size, 0),
      r_dv     = if_else((is_buy_direct | is_buy_mid | is_sell_direct | is_sell_mid) &
                           (retail_buy == 1 | retail_sell == 1), DV, 0),
      
      # Break-out of retail: separate for buys and sells.
      rb_trades = if_else((is_buy_direct | is_buy_mid) & (retail_buy == 1), 1, 0),
      rb_shares = if_else((is_buy_direct | is_buy_mid) & (retail_buy == 1), size, 0),
      rb_dv     = if_else((is_buy_direct | is_buy_mid) & (retail_buy == 1), DV, 0),
      rs_trades = if_else((is_sell_direct | is_sell_mid) & (retail_sell == 1), 1, 0),
      rs_shares = if_else((is_sell_direct | is_sell_mid) & (retail_sell == 1), size, 0),
      rs_dv     = if_else((is_sell_direct | is_sell_mid) & (retail_sell == 1), DV, 0)
    )
  
  # --- Part 3: Aggregate the row-wise counters to a minute-level summary ---
  # On your Postgres WRDS connection, compute every minute‐summary in one pass:
  summary_minute <- df_counters |>
    window_order(time_m) |>
    mutate(
      # --- minute‐level sums via window‐functions ---
      total_trades = sql(
        "SUM(total_trades) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      total_shares = sql(
        "SUM(total_shares) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      total_dv = sql(
        "SUM(total_dv)     OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      
      b_trades = sql(
        "SUM(b_trades) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      b_shares = sql(
        "SUM(b_shares) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      b_dv = sql(
        "SUM(b_dv)     OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      
      s_trades = sql(
        "SUM(s_trades) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      s_shares = sql(
        "SUM(s_shares) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      s_dv = sql(
        "SUM(s_dv)     OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      
      r_trades = sql(
        "SUM(r_trades) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      r_shares = sql(
        "SUM(r_shares) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      r_dv = sql(
        "SUM(r_dv)     OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      
      rb_trades = sql(
        "SUM(rb_trades) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      rb_shares = sql(
        "SUM(rb_shares) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      rb_dv = sql(
        "SUM(rb_dv)     OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      
      rs_trades = sql(
        "SUM(rs_trades) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      rs_shares = sql(
        "SUM(rs_shares) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      rs_dv = sql(
        "SUM(rs_dv)     OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      
      l_trades = sql(
        "SUM(l_trades) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      l_shares = sql(
        "SUM(l_shares) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      l_dv = sql(
        "SUM(l_dv)     OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      
      lb_trades = sql(
        "SUM(lb_trades) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      lb_shares = sql(
        "SUM(lb_shares) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      lb_dv = sql(
        "SUM(lb_dv)     OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      
      ls_trades = sql(
        "SUM(ls_trades) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      ls_shares = sql(
        "SUM(ls_shares) OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      ls_dv = sql(
        "SUM(ls_dv)     OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      
      # --- VWAP and last‐price via window‐functions ---
      vwap = sql(
        "SUM(price * size) OVER (PARTITION BY date, sym_root, sym_suffix, min)
       /
       SUM(size)        OVER (PARTITION BY date, sym_root, sym_suffix, min)"
      ),
      price = sql(
        "FIRST_VALUE(price) OVER (
         PARTITION BY date, sym_root, sym_suffix, min
         ORDER BY time_m DESC
         ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING
       )"
      ),
      
      # tag the true “last” row per minute
      rn = sql(
        "ROW_NUMBER() OVER (
         PARTITION BY date, sym_root, sym_suffix, min
         ORDER BY time_m DESC
       )"
      )
    ) |>
    # keep only that one “last‐trade” row each minute
    filter(rn == 1) |>
    ungroup() |>
    # drop everything except your minute summary + time_m
    select(
      date, sym_root, sym_suffix, min, time_m,
      price, vwap,
      total_trades, total_shares, total_dv,
      b_trades, b_shares, b_dv,
      s_trades, s_shares, s_dv,
      r_trades, r_shares, r_dv,
      rb_trades, rb_shares, rb_dv,
      rs_trades, rs_shares, rs_dv,
      l_trades, l_shares, l_dv,
      lb_trades, lb_shares, lb_dv,
      ls_trades, ls_shares, ls_dv
    )
  
  # Write the summary_minute data frame to a temporary table in DuckDB.
  summary_minute_copy <- 
    summary_minute |>
    copy_to(duck_conn, df = _, name = "summary_min", 
            temporary = TRUE, 
            overwrite = TRUE)
  
  # Use DuckDB's COPY command to export the table to a Parquet file.
  nobs <-  dbExecute(duck_conn, 
                     glue("  
            COPY (SELECT * FROM summary_min
            ORDER BY date, sym_root, sym_suffix, min)
  TO '{wct_path}/{table_name}_min.parquet'
  (FORMAT PARQUET);
                 "))
  
  end_time <- Sys.time()
  mins <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  message(glue::glue(
    "'{wct_path}/{table_name}_min.parquet' created with {nobs} rows in {round(mins, 2)} minutes.\n"
  ))
  
  
  
}
################################################################################

################################################################################
# Apply the collection function 

#Create a list all the wct tables
table_list <- wrds  |> 
  DBI::dbListObjects(DBI::Id(schema = 'taqmsec')) |> 
  dplyr::pull(table) |> 
  purrr::map(~slot(.x, 'name'))  |> 
  dplyr::bind_rows()  |>  
  filter(str_detect(table, "^wct_\\d{8}$"))

#Filter the table_list to a subset that still needs to be collected
my_list <- table_list |> 
  filter(as.integer(substr(table, 5, 8)) >= 2024) |> 
  filter(as.integer(substr(table, 5, 8)) < 2025) |> 
  #if you wanted to test run a smaller subset like a month
  #you could uncomment something like below
  filter(str_detect(table, "^wct_20240102")) |> 
           pull(table) 

# Run the collect_wct function on each table name in the final list
walk(my_list,collect_wct)
  
  
# Disconnect from DuckDB (shutting down the in-memory instance)
DBI::dbDisconnect(my_duckdb, shutdown = TRUE)



