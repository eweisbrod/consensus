# ==============================================================================
# 001-prepare-raw-ciq-data.R
#
# Purpose:
#   Prepare the raw Capital IQ parquet files into a single firm-quarter dataset
#   by merging consensus estimates, periods, primary earnings, trading items,
#   security identifiers, numeric data, and analysis data.
#
# Inputs (from RAW_DATA_DIR):
#   ciqEstimateConsensus.parquet
#   ciqEstimatePeriod.parquet
#   ciqEstimatePrimaryEarnings.parquet
#   ciqTradingItem.parquet
#   ciqSecurity.parquet
#   ciqEstimateAnalysisData_sel.parquet
#   ciqEstimateNumericData_<YYYY>.parquet  (per-year files; processed into
#                                            sample-numeric-data.parquet by the
#                                            DuckDB block at the top of this
#                                            script)
#
# Outputs (to DATA_DIR):
#   sample-numeric-data.parquet  (intermediate; created by the DuckDB block)
#   raw_ciq_data.dta
#
# Notes:
#   - Reads RAW_DATA_DIR and DATA_DIR from .env via the dotenv package.
#   - Reading all CIQ parquets into memory at once requires ~40 GB of RAM.
#   - This could be avoided with an on-disk duckdb instance. 
# ==============================================================================

# Setup ------------------------------------------------------------------------

#disable scientific notation
options(scipen = 999)


#Load libraries
library(haven)
library(glue)
library(arrow)
library(duckdb)
library(DBI)
library(tidyverse)
library(dotenv)


# Load .env (project root, one level above src/) and read pipeline paths.
load_dot_env()
raw_data_dir <- Sys.getenv("RAW_DATA_DIR")
data_path    <- Sys.getenv("DATA_DIR")

# Shared helpers (provides batch_run, FF12 / FF49, etc.)
source("src/utils.R")


# Merge together the numeric files for the sample years ------------------------

# Connect to a DuckDB instance in memory (or point to a file if you prefer persistence)
duck_con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Make a list of needed parquet files
years <- 2002:2024

file_list <- glue("'{raw_data_dir}/ciqEstimateNumericData_{years}.parquet'") |>
  paste(collapse = ", ")


#Define the full query string, keep only dataitemids we are using
numeric_data_nobs <- dbExecute(duck_con, sql(glue("
  COPY (SELECT *
  FROM read_parquet([{file_list}])
  WHERE DATAITEMID in (100173,100271,100278,
  100179,100277,100284,
  100177,100275,100282,
  100330,100358,100360))
  TO '{data_path}/sample-numeric-data.parquet'
  (FORMAT PARQUET);
")))



# Disconnect from DuckDB (shutting down the in-memory instance)
DBI::dbDisconnect(duck_con, shutdown = TRUE)



# Read input data --------------------------------------------------------------

#NOTE: it will take about 40GB of RAM to read all data at once

ciqEstimateConsensus<- read_parquet(glue("{raw_data_dir}/ciqEstimateConsensus.parquet")) |>
  rename_with(tolower)

ciqEstimatePeriod<- read_parquet(glue("{raw_data_dir}/ciqEstimatePeriod.parquet")) |>
  rename_with(tolower)


ciqEstimatePrimaryEarnings<- read_parquet(glue("{raw_data_dir}/ciqEstimatePrimaryEarnings.parquet")) |>
  rename_with(tolower)

ciqTradingItem<- read_parquet(glue("{raw_data_dir}/ciqTradingItem.parquet")) |>
  rename_with(tolower)

ciqSecurity<- read_parquet(glue("{raw_data_dir}/ciqSecurity.parquet")) |>
  rename_with(tolower)

ciqEstimateNumericData<- read_parquet(glue("{data_path}/sample-numeric-data.parquet")) |>
  rename_with(tolower)

ciqEstimateAnalysisData <- read_parquet(glue("{raw_data_dir}/ciqEstimateAnalysisData_sel.parquet")) |>
  rename_with(tolower)


# Step 1: Merge Estimateconsensusid to Estimateperiodid ------------------------
# retain only completed quarterly periods 

#Start with the Estimate Consensus File
ciq1 <- ciqEstimateConsensus |> 
  #Filter period type =2 , quarterly data
  inner_join(filter(ciqEstimatePeriod,periodtypeid==2),
             join_by(estimateperiodid)
             ) |> 
  filter(accountingstandardid==2, parentflag==0,primaryparentconsolflag==1) |> 
  #remove missing advancedates because we always need actuals
  drop_na(advancedate)



#Step 2: Merge with primary trading items of primary securities ----------------
ciq2 <- ciq1 |>
  inner_join(
  # filter tradingitem table to only primary trading item for each securityid  
    ciqTradingItem |>select(tradingitemid,securityid,trading_primary=primaryflag),
    #merge with this subset of trading items using tradingitemid,
    join_by(tradingitemid)
    ) |> 
  inner_join(
  # filter security table to only primary security for each company  
    ciqSecurity |>  select(companyid,securityid,sec_primary=primaryflag),
    join_by(companyid,securityid)
    )



#Step 3: Merge in the DataItemID for primary earnings --------------------------

ciq3 <- ciq2 |> 
  inner_join(ciqEstimatePrimaryEarnings,
             join_by(companyid,tradingitemid,
                     advancedate >= startdate,
                     advancedate <= enddate)
             ) |> 
  #drop un-needed start and end dates after merging
  rename(primary_startdate=startdate,primary_enddate=enddate)  |>  
  #rename dataitemid from the primary earnings file to denote that this
  #value of dataitemid is the ID for the consensus estimate (estimateVarID)
  rename(estimateVarID = dataitemid)  |>  
  #create an ID identifying the dataitem of the actual based on which dataitemid
  #was used for the estimate
  mutate(actualVarID = case_when(estimateVarID ==  100173~100179,
                                 estimateVarID ==  100271~100277,
                                 estimateVarID ==  100278~100284)) |> 
  #create an ID identifying the dataitem for following based on which dataitemid
  #was used for the estimate
  mutate(followVarID = case_when(estimateVarID ==  100173~100177,
                                 estimateVarID ==  100271~100275,
                                 estimateVarID ==  100278~100282)) |> 
  #create an ID identifying the dataitem of the surprise based on which dataitemid
  #was used for the estimate
  mutate(surpVarID = case_when(estimateVarID ==  100173~100330,
                               estimateVarID ==  100271~100358,
                               estimateVarID ==  100278~100360)) 



#Step 4: Merge in the Number for the Consensus Estimate ------------------------


ciq4 <- ciq3 |> 
#rename some variables in the numeric data file
inner_join(ciqEstimateNumericData |> 
             select(estimateVarID=dataitemid,estimateconsensusid,
                    consensusEPS=dataitemvalue,consensusSFactor=splitfactor,
                    effectivedate, todate), 
           join_by(estimateVarID,
                   estimateconsensusid,
                   advancedate >= effectivedate, 
                   advancedate <= todate)
           ) |>
  #remove the un-needed daterange
  rename(consensus_effective=effectivedate, consensus_to=todate)


#Step 5: Merge in the Number for the Actual ------------------------------------


ciq5 <- ciq4 |> 
  #rename some variables in the numeric data file
  inner_join(ciqEstimateNumericData |> 
               select(actualVarID=dataitemid,estimateconsensusid,
                      actualEPS=dataitemvalue,actualSFactor=splitfactor,
                      effectivedate, todate), 
             join_by(actualVarID,
                     estimateconsensusid,
                     #advancedate >= effectivedate, 
                     advancedate <= todate)
  ) |>
  #remove the un-needed daterange
  rename(actual_effective=effectivedate,actual_to=todate) |>
  distinct()


# Step 6: Merge in Following ---------------------------------------------------


ciq6 <- ciq5 |> 
  #rename some variables in the numeric data file
  inner_join(ciqEstimateNumericData |> 
               select(followVarID=dataitemid,estimateconsensusid,
                      numEstimates=dataitemvalue,
                      effectivedate, todate), 
             join_by(followVarID,
                     estimateconsensusid,
                     advancedate >= effectivedate, 
                     advancedate <= todate)
  ) |>
  #remove the un-needed daterange
  rename(follow_effective=effectivedate, follow_to=todate)


# Step 7: Merge in Surprise ----------------------------------------------------


ciq7 <- ciq6 |> 
  #select and rename some variables in the surprise data file
  #left join because we might not need this
  left_join(ciqEstimateAnalysisData |>  
               select(surpVarID=dataitemid,estimateconsensusid,
                      ciqSurp=dataitemvalue, isnmflag), 
             join_by(surpVarID,estimateconsensusid)
             ) |> 
  rename(ciq_mean_a=consensusEPS,
         ciq_actual_a=actualEPS,
         ciq_following=numEstimates) |>
  mutate(ciq_mean_u = ciq_mean_a*consensusSFactor,
         ciq_actual_u = ciq_actual_a*consensusSFactor)

#write data to disk
write_dta(ciq7,glue("{data_path}/raw_ciq_data.dta"))


