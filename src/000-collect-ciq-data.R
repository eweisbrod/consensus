# ==============================================================================
# 000-collect-ciq-data.R
#
# Purpose:
#   Download raw Capital IQ data from the Xpressfeed data feed and save to
#   parquet files.
#
# Inputs:
#   Xpressfeed database tables (via ODBC):
#     XPRESSFEED.CIQESTIMATECONSENSUS
#     XPRESSFEED.CIQESTIMATEPERIOD
#     XPRESSFEED.CIQTRADINGITEM
#     XPRESSFEED.CIQSECURITY
#     XPRESSFEED.CIQESTIMATEPRIMARYEARNINGS
#     XPRESSFEED.CIQESTIMATENUMERICDATA
#     XPRESSFEED.CIQCOMPANY
#     XPRESSFEED.CIQESTIMATEANALYSISDATA
#
# Outputs:
#   ciqEstimateConsensus.parquet
#   ciqEstimatePeriod.parquet
#   ciqTradingItem.parquet
#   ciqSecurity.parquet
#   ciqEstimatePrimaryEarnings.parquet
#   ciqEstimateAnalysisData_sel.parquet
#   ciqEstimateNumericData/ciqEstimateNumericData_1987-1997.parquet
#   ciqEstimateNumericData/ciqEstimateNumericData_<YYYY>.parquet  (1998-2025)
#   ciqCompany/ciqCompany_<lower>_<upper>.parquet  (chunked by COMPANYID, step 10M)
#
# Notes:
#   - Run by James Ng, Notre Dame data scientist.
#   - Last executed: 2025-06-17.
#   - Requires ODBC access to the Xpressfeed Capital IQ data feed.
#   - Skipped on replication runs; outputs are pre-built.
# ==============================================================================

rm(list=ls())
options(scipen=999)
library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
library(lubridate)
library(data.table)
library(tictoc)
library(stringr)
library(readr)
library(arrow)
con <- dbConnect(odbc::odbc(), "panjiva", timeout = 10, pwd=rstudioapi::askForPassword("Database password"))
##################################################################################

con_ciqEstimateConsensus <- tbl(con, in_schema('XPRESSFEED', 'CIQESTIMATECONSENSUS')) 
prvw_ciqEstimateConsensus <- con_ciqEstimateConsensus %>% head(10) %>% collect()
tic()
nrows_ciqEstimateConsensus <- con_ciqEstimateConsensus %>% select(ESTIMATECONSENSUSID) %>% collect()
toc()

tic()
df_ciqEstimateConsensus <- con_ciqEstimateConsensus %>% collect()
toc()
write_parquet(df_ciqEstimateConsensus, 'D:\\jessie_ciq_estimates\\ciqEstimateConsensus.parquet')

tic()
con_ciqEstimatePeriod <- tbl(con, in_schema('XPRESSFEED', 'CIQESTIMATEPERIOD')) 
df_ciqEstimatePeriod <- con_ciqEstimatePeriod %>% collect()
toc()
write_parquet(df_ciqEstimatePeriod, 'D:\\jessie_ciq_estimates\\ciqEstimatePeriod.parquet')

tic()
con_ciqTradingPeriod <- tbl(con, in_schema('XPRESSFEED', 'CIQTRADINGITEM')) 
df_ciqTradingPeriod <- con_ciqTradingPeriod %>% collect()
toc()
write_parquet(df_ciqTradingPeriod, 'D:\\jessie_ciq_estimates\\ciqTradingItem.parquet')

tic()
con_ciqSecurity <- tbl(con, in_schema('XPRESSFEED', 'CIQSECURITY')) 
df_ciqSecurity <- con_ciqSecurity %>% collect()
toc()
write_parquet(df_ciqSecurity, 'D:\\jessie_ciq_estimates\\ciqSecurity.parquet')


tic()
con_ciqEstimatePrimaryEarnings <- tbl(con, in_schema('XPRESSFEED', str_to_upper('ciqEstimatePrimaryEarnings'))) 
df_ciqEstimatePrimaryEarnings <- con_ciqEstimatePrimaryEarnings %>% collect()
toc()
write_parquet(df_ciqEstimatePrimaryEarnings, 'D:\\jessie_ciq_estimates\\ciqEstimatePrimaryEarnings.parquet')


# pull ciqEstimateNumericData by effectivedate year
# 1.5 - 2 minutes per year
con_ciqEstimateNumericData <- tbl(con, in_schema('XPRESSFEED', str_to_upper('ciqEstimateNumericData'))) 
con_ciqEstimateNumericData <- con_ciqEstimateNumericData %>% mutate(year = year(EFFECTIVEDATE)) 

pull_ciqEstimateNumericData <- function(wantyears) {
  tic()
  df <- con_ciqEstimateNumericData %>% filter(year %in% wantyears) %>% collect()
  toc()
  return(df)
}

years1987_1997 <- seq(1987,1997)
df <- pull_ciqEstimateNumericData(years1987_1997)
write_parquet(df,  'D:\\jessie_ciq_estimates\\ciqEstimateNumericData\\ciqEstimateNumericData_1987-1997.parquet')

for(yr in seq(1998, 2025)) {
  print(yr)
  df <-  pull_ciqEstimateNumericData(yr)
  print(summary(df$year))
  print(dim(df))
  write_parquet(df, paste0('D:\\jessie_ciq_estimates\\ciqEstimateNumericData\\ciqEstimateNumericData_', yr,'.parquet'))
  rm(df)
}

tic()
con_ciqCompany <- tbl(con, in_schema('XPRESSFEED', 'CIQCOMPANY')) 

ciqCompany_nrows <- con_ciqCompany %>% summarise(nrows=n()) %>% collect()
ciqCompany_head <- con_ciqCompany %>% head(100) %>% collect()
ciqCompany_companyid_stats <- con_ciqCompany %>% summarise(min_id = min(COMPANYID),
                                                           max_id = max(COMPANYID),
                                                           median_id = median(COMPANYID)) %>% collect()
ciqCompany_companyid_na <- con_ciqCompany %>% 
  filter(is.na(COMPANYID)) %>%
  summarise(nrows = n()) %>% 
  collect()  

maxid <- ciqCompany_companyid_stats$max_id
step <- 10000000
for(i in seq(1, maxid, step)) {
  upperbound <- i + step - 1
  lowerbound <- i
  outfile <- paste0('D:\\jessie_ciq_estimates\\ciqCompany\\ciqCompany_',lowerbound,'_',upperbound,'.parquet')
  if(file.exists(outfile)) {next}
  print(paste(lowerbound, upperbound))
  df <- con_ciqCompany %>% filter(COMPANYID >= lowerbound & COMPANYID <= upperbound) %>% collect()
  write_parquet(df, outfile)
  gc()
}

# ciqEstimateAnalysisData <- tbl(capiq,in_schema("public","ciqestimateanalysisdata"))
# Hopefully this is the last one.  Also, if this table is too big, you can filter it to only dataitemid in  (100330,100358,100360)

con_ciqEstimateAnalysisData <- tbl(con,in_schema("XPRESSFEED","CIQESTIMATEANALYSISDATA"))
# preview
head_ciqEstimateAnalysisData <- con_ciqEstimateAnalysisData %>% head(10) %>% collect()
nrows_ciqEstimateAnalysisData <- con_ciqEstimateAnalysisData %>% summarise(n=n()) %>% collect()

# it is too big
df_ciqEstimateAnalysisData_wanted <- con_ciqEstimateAnalysisData %>%
  filter(DATAITEMID %in% c (100330,100358,100360)) %>%
  collect()
write_parquet(df_ciqEstimateAnalysisData_wanted, 'D:\\jessie_ciq_estimates\\ciqEstimateAnalysisData_sel.parquet')
