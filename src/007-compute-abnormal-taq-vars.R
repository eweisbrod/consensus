# ==============================================================================
# 007-compute-abnormal-taq-vars.R
#
# Purpose:
#   Compute abnormal order-imbalance and intraday-volatility measures for
#   each firm-quarter event by comparing event-window (days 0,+1) to
#   non-event-window (days -41,-11) averages of the daily OIB summaries
#   built by 006.
#
# Inputs (from WCT_DIR):
#   oib-data-all.parquet                     (output of 006)
#
# Inputs (from DATA_DIR):
#   all_five3.sas7bdat                       (output of 004; provides
#                                            best_anndats_adj per firm-qtr)
#
# Inputs (from RAW_DATA_DIR, cached):
#   taqmclink-20250731.parquet               (CRSP-TAQ link from WRDS;
#                                            built first time, cached after)
#
# Inputs (from WRDS, fresh per run):
#   crsp.dsi                                 (trading-day calendar; tiny,
#                                            re-pulled each run)
#
# Outputs (to DATA_DIR):
#   abnormal-oib-data.parquet
#   abnormal-oib-data.dta
#
# Notes:
#   - Reads RAW_DATA_DIR, DATA_DIR, WCT_DIR from .env via the dotenv package.
#   - taqmclink WRDS pull is cached -- if the parquet exists, the WRDS pull
#     is skipped on re-runs. Delete the file to force re-pull.
#   - WRDS credentials read via the keyring package -- store them once with
#     keyring::key_set("WRDS_user") and key_set("WRDS_pw").
# ==============================================================================

#--------------- Set Up --------------------------------------------------------
library(glue)
library(DBI)
library(dbplyr)
library(duckdb)
library(RPostgres)
library(data.table)
library(hms)
library(tidyverse)
library(dotenv)

# Load .env (project root, one level above src/) and read pipeline paths.
load_dot_env()
data_path <- Sys.getenv("DATA_DIR")
raw_path  <- Sys.getenv("RAW_DATA_DIR")
wct_path  <- Sys.getenv("WCT_DIR")


#Load Data ---------------------------------------------------------------------


#Load OIB data
oib_data <- arrow::read_parquet(glue("{wct_path}/oib-data-all.parquet"))


#Define Trading Dates Function -------------------------------------------------

td_window <- function(data,trading_calendar,event_col, begin_window, end_window) {
  
  #Convert column names to symbols for dynamic programming
  event_col <- ensym(event_col)
  
  #Max trading date
  max_td <- max(trading_calendar$date)-end_window
  
  data |> 
    #Filter out out of bounds event dates (the !! makes any variable input read as column)
    filter(!!event_col <= max_td) |> 
    #Find closest trading date equal to or after the event date specified
    inner_join(trading_calendar, by = join_by(closest(!!event_col <= date))) |> 
    #Drop date variable to avoid duplicate "date" variables
    select(-date) |> 
    #Rename td_idx to evt_idx to differentiate  
    rename(evt_idx=td_idx) |>
    #Expand rows for the range of trading days specified by the user
    expand_grid(rel_td = seq(begin_window, end_window)) |> 
    #Create "rel_td" which adds the offset to the initial trading day number which will be used in subsequent merge
    mutate(td_idx = evt_idx + rel_td) |> 
    #Join again with trading_dates to get actual dates
    left_join(trading_calendar, by = c("td_idx")) |> 
    #Arrange rows for readability
    arrange(across(everything()))
  }




#Find the event and nonevent windows -------------------------------------------

#Load allfive dataset and select dates and identifiers
data1 <- haven::read_sas(glue("{data_path}/all_five3.sas7bdat")) |>
  select(permno,best_anndats_adj)



#Get relative trading dates ----------------------------------------------------

#Cache-aware load of the CRSP-TAQ linking file -- if the parquet exists in
#raw_data, skip the WRDS pull.
taqmclink_path <- glue("{raw_path}/taqmclink-20250731.parquet")
have_taqmclink <- file.exists(taqmclink_path)

if (have_taqmclink) {
  message(glue("Loading cached taqmclink from {taqmclink_path}"))
  taqmclink <- arrow::read_parquet(taqmclink_path)
}

# Connect to WRDS for the trading calendar (and for taqmclink if not cached)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user=keyring::key_get("WRDS_user"),
                  password=keyring::key_get("WRDS_pw"),
                  sslmode='require',
                  dbname='wrds')
wrds  # checking if connection exists

if (!have_taqmclink) {
  taqmclink <- tbl(wrds, in_schema("wrdsapps", "taqmclink")) |>
    select(permno, date, sym_root) |>
    collect()
  arrow::write_parquet(taqmclink, taqmclink_path,
                       compression = "gzip", compression_level = 5)
}

# Create a trading calendar from crsp dsi dates
trading_calendar <- tbl(wrds,in_schema("crsp", "dsi")) |>
  distinct(date) |>
  mutate(td_idx = row_number()) |>
  collect()

#Pull event trading days
p0_p1_dates <- td_window(data1,trading_calendar,"best_anndats_adj",0,1) |> 
  distinct()

#Pull non-event trading days
m41_m11_dates <- td_window(data1,trading_calendar,"best_anndats_adj",-41,-11) |> 
  distinct()

#Compute non-event benchmarks --------------------------------------------------

#Join the order imbalance data to the non-event window
joined_data <- m41_m11_dates |>
  inner_join(taqmclink, join_by(permno,date)) |> 
  left_join(oib_data |> 
              select(sym_root,sym_suffix,date,logret_ss_all,logret_ss_rh,
                     b_dv_all, rb_dv_all, s_dv_all, rs_dv_all, lb_dv_all,
                     ls_dv_all), 
            by = c("sym_root", "date")) |> 
  mutate(ib_dv_all = b_dv_all - rb_dv_all,
         is_dv_all = s_dv_all - rs_dv_all) |> 
  filter(!is.na(sym_root))  |>  #Filter out any illogical cases
filter(rb_dv_all < b_dv_all,
       rs_dv_all < s_dv_all) |> 
  distinct()

#Figure out the sum of each in rolling two day window
nonevt_data <- joined_data |>
  arrange(permno,sym_root,sym_suffix, best_anndats_adj, date) |>
  group_by(permno,sym_root,sym_suffix, best_anndats_adj) |>
  mutate(
    #Calculate OIB for each day in the window
    ret_oib = if_else(rb_dv_all+rs_dv_all>0,(rb_dv_all-rs_dv_all)/(rb_dv_all+rs_dv_all), NA),
    inst_oib = if_else(ib_dv_all+is_dv_all>0,(ib_dv_all-is_dv_all)/(ib_dv_all+is_dv_all), NA),
    large_oib = if_else(lb_dv_all+ls_dv_all>0,(lb_dv_all-ls_dv_all)/(lb_dv_all+ls_dv_all), NA)
  )|>
  filter(!is.na(logret_ss_all)) |> 
  #Summarize back down to revision level
  summarise(n_nonevt = n(),
            avg_nonevt_ret_oib = mean(ret_oib, na.rm=T),
            avg_nonevt_inst_oib = mean(inst_oib, na.rm=T),
            avg_nonevt_large_oib = mean(large_oib,na.rm=T),
            avg_nonevt_ss_all = mean(logret_ss_all, na.rm=T),
            avg_nonevt_ss_rh = mean(logret_ss_rh, na.rm=T),
            avg_nonevt_retail_share = mean(((rb_dv_all+rs_dv_all)/(b_dv_all+s_dv_all)), na.rm=T),
            .groups = "drop")


#Now collect announcement window data ------------------------------------------

evt_data <- p0_p1_dates |> 
  inner_join(taqmclink, join_by(permno,date)) |> 
  left_join(oib_data |> 
              select(sym_root,sym_suffix,date,logret_ss_all,logret_ss_rh,
                     b_dv_all, rb_dv_all, s_dv_all, rs_dv_all, lb_dv_all,
                     ls_dv_all), 
            by = c("sym_root", "date")) |> 
  mutate(ib_dv_all = b_dv_all - rb_dv_all,
         is_dv_all = s_dv_all - rs_dv_all,
         ret_oib = if_else(rb_dv_all+rs_dv_all>0,(rb_dv_all-rs_dv_all)/(rb_dv_all+rs_dv_all), NA),
         inst_oib = if_else(ib_dv_all+is_dv_all>0,(ib_dv_all-is_dv_all)/(ib_dv_all+is_dv_all), NA),
         large_oib = if_else(lb_dv_all+ls_dv_all>0,(lb_dv_all-ls_dv_all)/(lb_dv_all+ls_dv_all), NA)
         ) |> 
  filter(!is.na(sym_root))  |>  #Filter out any illogical cases
  filter(rb_dv_all < b_dv_all,
         rs_dv_all < s_dv_all) |> 
  distinct() |> 
  group_by(permno,sym_root,sym_suffix, best_anndats_adj) |>
  #Summarize back down to revision level
  summarise(n_evt = n(),
            avg_evt_ret_oib = mean(ret_oib, na.rm=T),
            avg_evt_inst_oib = mean(inst_oib, na.rm=T),
            avg_evt_large_oib = mean(large_oib,na.rm=T),
            avg_evt_ss_all = mean(logret_ss_all, na.rm=T),
            avg_evt_ss_rh = mean(logret_ss_rh, na.rm=T),
            avg_evt_retail_share = mean(((rb_dv_all+rs_dv_all)/(b_dv_all+s_dv_all)), na.rm=T),
            .groups = "drop")
 

#Merge in prior month data and calculate abnormal measures
abnormal_data <- evt_data |> 
  inner_join(nonevt_data |> filter(n_nonevt >= 5),
             by=c("permno","sym_root","sym_suffix","best_anndats_adj")) |> 
  mutate(ab_retail_imbalance = avg_evt_ret_oib - avg_nonevt_ret_oib,
         ab_inst_imbalance = avg_evt_inst_oib - avg_nonevt_inst_oib,
         ab_large_imbalance = avg_evt_large_oib - avg_nonevt_large_oib,
         ab_ret_share = avg_evt_retail_share - avg_nonevt_retail_share,
         abnormal_volatility_all = if_else(avg_nonevt_ss_all > 0,
                                           log(avg_evt_ss_all / avg_nonevt_ss_all),
                                           NaN),
         abnormal_volatility_rh = if_else(avg_nonevt_ss_rh > 0,
                                          log(avg_evt_ss_rh / avg_nonevt_ss_rh),
                                          NaN)
  )

summary(abnormal_data)


abnormal_data |> group_by(permno,best_anndats_adj) |> summarise(obs = n()) |> filter(obs>1)
#about 9,200 dups

abnormal_data |> group_by(permno,sym_root,sym_suffix,best_anndats_adj) |> summarise(obs = n()) |> filter(obs>1)
#no dups in sym_root, sym_suffix

#Save
arrow::write_parquet(abnormal_data,glue("{data_path}/abnormal-oib-data.parquet"))
haven::write_dta(abnormal_data,glue("{data_path}/abnormal-oib-data.dta"))

