# ==============================================================================
# 000-collect-factset.R
#
# Purpose:
#   Download raw FactSet consensus and actuals data via Snowflake (ODBC) and 
#   save to .rda and .csv files, one per fiscal year.
#
# Inputs:
#   FactSet database tables (via ODBC):
#     FE_V4.FE_BASIC_CONH_QF
#     FE_V4.FE_BASIC_ACT_QF
#     FE_V4.FE_SEC_ENTITY_HIST
#     FE_V4.FE_SEC_MAP
#     SYM_V1.SYM_CUSIP
#     SYM_V1.SYM_COVERAGE
#     SYM_V1.SYM_TICKER_EXCHANGE
#
# Outputs:
#   output_rda/quarterly_consensus_basic_<YYYY>.rda
#   output_csv/quarterly_consensus_basic_<YYYY>.csv
#
# Notes:
#   - Run by Brandon Greenawalt, Notre Dame data scientist.
#   - Last executed: 2025-08-28.
#   - Requires ODBC access to the FactSet data feed.
#   - Skipped on replication runs; outputs are pre-built.
# ==============================================================================

#install.packages(c("DBI", "dplyr","dbplyr","odbc"))
rm(list=ls())
gc()
library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
library(lubridate)


con <- dbConnect(odbc::odbc(), "factset", timeout = 10)



## Connections to Estimates
fe_basic_conh_qf <- tbl(src = con, in_schema("FE_V4","FE_BASIC_CONH_QF"))
fe_sec_coverage <- tbl(con,in_schema("FE_V4","FE_SEC_COVERAGE"))
fe_sec_entity <- tbl(con,in_schema("FE_V4","FE_SEC_ENTITY"))
fe_sec_entity_hist <- tbl(con,in_schema("FE_V4","FE_SEC_ENTITY_HIST"))
fe_sec_map <- tbl(con,in_schema("FE_V4","FE_SEC_MAP"))
fe_basic_act_qf <- tbl(con,in_schema("FE_V4","FE_BASIC_ACT_QF"))
fe_advanced_conh_qf <- tbl(con,in_schema("FE_V4","FE_ADVANCED_CONH_QF"))
fe_advanced_act_qf <- tbl(con,in_schema("FE_V4","FE_ADVANCED_ACT_QF"))



## Connections to symbology
sym_entity <- tbl(con,in_schema("SYM_V1","SYM_ENTITY"))
sym_cusip <- tbl(con,in_schema("SYM_V1","SYM_CUSIP"))
sym_coverage <- tbl(con,in_schema("SYM_V1","SYM_COVERAGE")) 
sym_ticker_exchange <- tbl(con,in_schema("SYM_V1","SYM_TICKER_EXCHANGE")) 
sym_ticker_region <- tbl(con,in_schema("SYM_V1","SYM_TICKER_REGION")) 
sym_cusip <- tbl(con,in_schema("SYM_V1","SYM_CUSIP"))


chist <- left_join(sym_cusip, fe_sec_map, by ="FSYM_ID")  %>% 
  left_join(fe_sec_entity_hist, by ="FSYM_ID") %>% 
  mutate(END_DATE = ifelse(is.na(END_DATE), as.Date('2050-11-13'), END_DATE)) %>% 
  #filter(!!is.na(FSYM_COMPANY_ID)) %>% 
  rename(FSYM_ID_CUSIP = FSYM_ID) #%>% 
  #filter(FSYM_ID_CUSIP == "XG83DP-S") %>% 
  #left_join(sym_entity, by = "FACTSET_ENTITY_ID")

  



actual <- fe_basic_act_qf %>% 
  select(-CURRENCY, -ADJDATE)


  



timeline <- fe_basic_conh_qf %>%
  filter(FE_ITEM %in% c("EPS", 'SALES')) %>% 
  mutate(year=extract(NULL %year from% FE_FP_END)) %>% 
  group_by(year) %>% 
  summarise(count=n()) %>% 
  collect() %>% 
  arrange(year)





y=2003
for(y in timeline$year){
  output <- left_join(fe_basic_conh_qf, select(sym_coverage, -CURRENCY), by ="FSYM_ID")  %>%
    rename(Estimate_CURRENCY=CURRENCY) %>%
    left_join(actual, by = c("FSYM_ID", "FE_ITEM", "FE_FP_END")) %>%
    mutate(year=extract(NULL %year from% FE_FP_END)) %>%
    #filter(FSYM_ID == 'T0SQZH-R') %>% 
    filter(year== y) %>% 
    left_join(sym_ticker_exchange, by=c("FSYM_PRIMARY_LISTING_ID"="FSYM_ID")) %>% 
    left_join(chist, sql_on = c("LHS.FSYM_SECURITY_ID = RHS.FSYM_ID_CUSIP and LHS.FE_FP_END >= RHS.START_DATE and LHS.FE_FP_END <= RHS.END_DATE")) %>%
    filter(FE_ITEM %in% c("EPS", 'SALES')) %>% 
    select(CUSIP, FSYM_ID, FE_ITEM, FE_FP_END, CONS_START_DATE, CONS_END_DATE, REPORT_DATE, Estimate_CURRENCY, ADJDATE, ACTUAL_VALUE, FE_MEAN,
            FE_MEDIAN, FE_NUM_EST, FE_LOW, FE_HIGH, FE_STD_DEV, FE_UP, FE_DOWN, # Estimate INFO
            primary_TICKER_EXCHANGE=TICKER_EXCHANGE, PROPER_NAME) %>%
    collect()
    
    
  
  print(y)
  save(output, file=paste0("output_rda/quarterly_consensus_basic_",y,".rda"))
  write.csv(output, file = paste0("output_csv/quarterly_consensus_basic_",y,".csv"), na = "", row.names = FALSE)
}

