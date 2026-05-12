# ==============================================================================
# 009-collect-ravenpack-data.R
#
# Purpose:
#   Pull RavenPack News Analytics article-level data from WRDS for each
#   firm-quarter event window in our sample, count articles by source
#   (IBES / FactSet / Zacks / CIQ / BB) and by mention type (actual,
#   total), and write a per-firm-quarter article-counts parquet.
#
# Inputs (from DATA_DIR):
#   all_five4.sas7bdat                       (output of 005; provides
#                                            permno + best_anndats_adj +
#                                            ticker / cusip / gvkey for
#                                            each firm-quarter event)
#
# Inputs (from WRDS):
#   ravenpack_full.rpa_full_equities_<YYYY>  (one schema per year 2002-2024;
#                                            heavy WRDS pulls, cached locally)
#   wrdsapps.rpfun_entity_mapping            (RavenPack <-> permno linker)
#   crsp.stocknames                          (permno-cusip metadata)
#   crsp.dsi                                 (trading-day calendar)
#
# Outputs (to DATA_DIR):
#   rpack_EA_articles_<YYYY>.parquet         (per-year cache of the raw
#                                            article rows for our sample;
#                                            cache-aware -- skip if exists)
#   rpack_EA_article_counts.parquet          (final aggregated per-firm-qtr
#                                            article counts)
#
# Notes:
#   - Reads DATA_DIR from .env via the dotenv package.
#   - Each per-year RavenPack pull is wrapped with an existence check, so
#     re-runs skip already-downloaded years. Delete the per-year parquet
#     to force re-pull.
#   - WRDS credentials read via the keyring package -- store them once with
#     keyring::key_set("WRDS_user") and key_set("WRDS_pw").
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
data_path <- Sys.getenv("DATA_DIR")


# Top-level cache check: if the final aggregate is already on disk, skip
# the entire script (including the WRDS connection). This both saves time
# on re-runs and lets the pipeline proceed when the cached parquets are
# present even if the WRDS metadata schema has shifted underneath us.
final_output <- glue("{data_path}/rpack_EA_article_counts.parquet")
if (file.exists(final_output)) {
  message(glue("rpack_EA_article_counts.parquet already exists at ",
               "{final_output} -- skipping 009. Delete the file to force ",
               "a re-pull."))
  quit(save = "no", status = 0)
}


# load input data --------------------------------------------------------------


#Load allfive dataset and select dates and identifiers
data0 <- haven::read_sas(glue("{data_path}/all_five4.sas7bdat")) |>
  rename_with(tolower)



data1 <- data0 |> 
  select(best_anndats_adj,crsp_ticker,conm,cusip,gvkey,permno,datadate,
         ends_with("actual_u"),ends_with("mean_u")) |> 
  filter(year(datadate) > 2001) 

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




#Get relative trading dates ----------------------------------------------------

if(exists("wrds")){
  dbDisconnect(wrds)  # because otherwise WRDS might time out
}

# Connect to WRDS
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user=keyring::key_get("WRDS_user"),
                  password=keyring::key_get("WRDS_pw"),
                  sslmode='require',
                  dbname='wrds')
wrds  # checking if connection exists

# Create a trading calendar from crsp dsi dates
trading_calendar <- tbl(wrds,in_schema("crsp", "dsi")) |> 
  distinct(date) |> 
  mutate(td_idx = row_number()) |> 
  collect()

#Pull event trading days
p0_p1_data <- td_window(data1,trading_calendar,"best_anndats_adj",0,1) |> 
  distinct()

# Download RavenPack Entity Map and Link with Input Data -----------------------

# NOTE: The cached rpack_EA_articles_<YYYY>.parquet files in DATA_DIR were
# pulled on 2025-08-22 against the table reference below. As of WRDS at
# the time of writing, ravenpack_common.wrds_all_mapping no longer
# resolves -- the WRDS-side reference data has been reorganized as part
# of RavenPack's broader RPNA 4.0 -> RPA 1.0 product migration, which
# discontinued ISIN as a key field in the granular data and added a
# real `cusip` column to the reference tables. The script as written
# uses the legacy RPNA shape (filter on ISIN, derive ncusip via substring),
# which matches how the cached parquets were produced.
#
# To re-pull from a fresh WRDS connection today, you would replace the
# table reference with one of the current options -- e.g.
# ravenpack_common.wrds_rpa_all_mappings (provides `cusip` directly, used
# in our other repo's working RavenPack code) or the canonical
# WRDS_COMPANY_NAMES table -- and adjust downstream joins from
# ncusip-from-ISIN to cusip8 = substr(cusip, 1, 8). The original code used
# in the paper during 2025 is preserved here.


rpack_entity_map <- tbl(wrds,
                        in_schema("ravenpack_common",
                                  "wrds_all_mapping")) |>
  filter(substr(isin, 1, 2) == "US") |>
  mutate(ncusip = substr(isin, 3, 10)) |>
  select(ncusip, ticker, rp_entity_id) |>
  distinct() |>
  collect()

stocknames <- tbl(wrds,in_schema("crsp","stocknames"))

permno_ncusip_link <- stocknames |> 
  select(permno, ncusip = cusip) |>
  filter(if_all(everything(),\(x) !is.na(x)))|>
  collect() |> 
  bind_rows(stocknames |> 
              select(permno, ncusip) |>
              filter(if_all(everything(),\(x) !is.na(x)))|>
              collect()
  ) |> 
  distinct()

data2 <- p0_p1_data |> 
  inner_join(permno_ncusip_link, by="permno") 

lookup_data <- data2 |> 
  inner_join(rpack_entity_map, 
             by=c("ncusip"="ncusip", "crsp_ticker"="ticker")) |> 
  mutate(across(ends_with("mean_u"),
                ~ as.character(sprintf("%.2f",abs(round(.x,2)))))) |> 
  mutate(across(ends_with("actual_u"),
                ~ as.character(sprintf("%.2f",abs(round(.x,2)))))) |> 
  #filter(if_all(everything(), ~ !is.na(.x))) |> 
  select(-ncusip) |> 
  distinct()




# Download Raw RavenPack Data for EA Windows -----------------------------------

tictoc::tic()
#use walk function to loop through years in a faster vectorized way
# walk is like lappy but it returns no output
# that is ok here because we will save each year of output to disk as part of
# the function 
walk(2002:2024, ~{

  out_path <- glue("{data_path}/rpack_EA_articles_{.x}.parquet")
  if (file.exists(out_path)) {
    message(glue("rpack_EA_articles_{.x} already exists -- skipping."))
    return(invisible(NULL))
  }

  # Record the start time
  start_time <- Sys.time()

  #Reference the RavenPack dataset for the given year
  rpack_dataset <- tbl(wrds,
                       in_schema("ravenpack_full",
                                 paste0("rpa_full_equities_", .x)))
  #Filter the lookup data to the same year
  lookup <- lookup_data |> filter(year(date) == .x)
  #convert RavenPack timestamps to EST and merge on entity ID and EST date
  merged_data <- rpack_dataset |> 
    mutate(
    timestamp_est =  sql("timestamp_utc at time zone 'Etc/UTC' at time zone
                         'America/New_York'"),
    date_est = date(timestamp_est)
    ) |> 
    #Use copy_inline to upload the lookup data to wrds
    inner_join(copy_inline(wrds,lookup),
               by=c("rp_entity_id"="rp_entity_id", "date_est"="date"))|> 
    #use collect to download the merged data back down
    collect()
  
  #write the downloaded data to a parquet file
  write_parquet(merged_data,glue("{data_path}/rpack_EA_articles_{.x}.parquet"))
  
  #cleanup
  rm(merged_data)
  gc()
  
  #record how long the work took 
  end_time <- Sys.time()
  mins <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  #Output a completion message
  message(glue::glue(
    "Wrote rpack_EA_articles_{.x} in {round(mins, 2)} minutes.\n"))

})
tictoc::toc()

# Read and Combine Annual Raw RavenPack Data -----------------------------------

rpack_data <- lapply(2002:2024, \(x) read_parquet(glue("{data_path}/rpack_EA_articles_{x}.parquet"))) |> 
  list_rbind()

# search the headline text for the mean and actual values
tictoc::tic() 
rpack2 <- rpack_data |> select(gvkey,permno,
                               best_anndats_adj,datadate,rp_story_id,
                               headline, ends_with("mean_u"),
                               ends_with("actual_u")) |> 
  #select only story_id and headline and then use distinct to remove duplicates
  distinct() |> 
  mutate(ibes_mean_article = if_else(str_detect(str_to_lower(headline),as.character(ibes_mean_u)),1,0),
         zacks_mean_article = if_else(str_detect(str_to_lower(headline),as.character(zacks_mean_u)),1,0),
         ciq_mean_article = if_else(str_detect(str_to_lower(headline),as.character(ciq_mean_u)),1,0),
         bb_mean_article = if_else(str_detect(str_to_lower(headline),as.character(bb_mean_u)),1,0),
         fset_mean_article = if_else(str_detect(str_to_lower(headline),as.character(fset_mean_u)),1,0),
         ibes_actual_article = if_else(str_detect(str_to_lower(headline),as.character(ibes_actual_u)),1,0),
         zacks_actual_article = if_else(str_detect(str_to_lower(headline),as.character(zacks_actual_u)),1,0),
         ciq_actual_article = if_else(str_detect(str_to_lower(headline),as.character(ciq_actual_u)),1,0),
         bb_actual_article = if_else(str_detect(str_to_lower(headline),as.character(bb_actual_u)),1,0),
         fset_actual_article = if_else(str_detect(str_to_lower(headline),as.character(fset_actual_u)),1,0),
         #pmax is one if either mean or actual is cited
         ibes_total1_article = pmax(ibes_mean_article,ibes_actual_article),
         #pmin is only equal to one if both mean and actual are cited
         ibes_total2_article = pmin(ibes_mean_article,ibes_actual_article),
         zacks_total1_article = pmax(zacks_mean_article,zacks_actual_article),
         zacks_total2_article = pmin(zacks_mean_article,zacks_actual_article),
         ciq_total1_article = pmax(ciq_mean_article,ciq_actual_article),
         ciq_total2_article = pmin(ciq_mean_article,ciq_actual_article), 
         bb_total1_article = pmax(bb_mean_article,bb_actual_article),
         bb_total2_article = pmin(bb_mean_article,bb_actual_article),
         fset_total1_article = pmax(fset_mean_article,fset_actual_article),
         fset_total2_article = pmin(fset_mean_article,fset_actual_article)
         # miss_article = if_else(str_detect(str_to_lower(headline),"miss"),1,0),
         # beat_article = if_else(str_detect(str_to_lower(headline),"beat"),1,0) 
         )|> 
  group_by(permno,best_anndats_adj) |> 
  #count the articles 
  summarize(
    ea_articles = n(),
    ibes_mean_articles = sum(ibes_mean_article),
    ibes_actual_articles = sum(ibes_actual_article),
    ibes_total1_articles = sum(ibes_total1_article),
    ibes_total2_articles = sum(ibes_total2_article),
    zacks_mean_articles = sum(zacks_mean_article),
    zacks_actual_articles = sum(zacks_actual_article),
    zacks_total1_articles = sum(zacks_total1_article),
    zacks_total2_articles = sum(zacks_total2_article),
    ciq_mean_articles = sum(ciq_mean_article),
    ciq_actual_articles = sum(ciq_actual_article),
    ciq_total1_articles = sum(ciq_total1_article),
    ciq_total2_articles = sum(ciq_total2_article),
    bb_mean_articles = sum(bb_mean_article),
    bb_actual_articles = sum(bb_actual_article),
    bb_total1_articles = sum(bb_total1_article),
    bb_total2_articles = sum(bb_total2_article),
    fset_mean_articles = sum(fset_mean_article),
    fset_actual_articles = sum(fset_actual_article),
    fset_total1_articles = sum(fset_total1_article),
    fset_total2_articles = sum(fset_total2_article),
    .groups = "drop"
  ) 
tictoc::toc()

rpack3 <- lookup_data |> 
  select(permno,best_anndats_adj) |> 
  distinct() |> 
  left_join(rpack2, by=c("permno","best_anndats_adj")) |> 
  mutate(across(ends_with("articles"), ~ replace_na(.x,0)))


#save as parquet
write_parquet(rpack3,glue("{data_path}/rpack_EA_article_counts.parquet"))
