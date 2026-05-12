# ==============================================================================
# 000-collect-bql-data.R
#
# Purpose:
#   Combine raw BQL/BDH Excel output downloaded from Bloomberg and export to
#   Stata .dta datasets.
#
# Inputs:
#   data/BDH best target price _ analyst rating/analyst rating/*.xlsx
#   data/BDH best target price _ analyst rating/target price/*.xlsx
#   data/BQL_A F/BQL_A/*.xlsx
#   data/BQL_A F/BQL_F/*.xlsx
#   data/BDH GAAP EPS actuals/*.xlsx
#   data/BQL GAAP_E_and_A/*.xlsx
#   data/BQL Market_E_and_A/*.xlsx
#
# Outputs:
#   Analyst_Rating.dta
#   Target_Price.dta
#   BQL_A.dta
#   BQL_F.dta
#   BDH_GAAP_EPS_actuals.dta
#   BQL_GAAP_E_and_A.dta
#   BQL_Market_E_and_A.dta
#
# Notes:
#   - Run by Brandon Greenawalt, Notre Dame data scientist.
#   - Bloomberg .xlsx inputs are licensed and not redistributable.
#   - Last executed: BQL_F.dta 2022-05-10, BQL_Market_E_and_A.dta 2022-05-17.
#   - Skipped on replication runs; outputs are pre-built.
# ==============================================================================


# ---- Section 1: Analyst Rating (BDH) -----------------------------------------

library(lubridate)
library(dplyr)


all_estimate_count <- data.frame()


path <- "data/BDH best target price _ analyst rating/analyst rating/"

files <- list.files(path)

l=1
for(l in 1:length(files)){
  ticker_dates <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, skip = 3)
  ticker_names <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, n_max = 1)
  ticker_names2 <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, skip = 1,  n_max = 1)

  ticker_names <- unlist(ticker_names)[unlist(ticker_names)!="ANR"] %>%
    na.omit() %>%
    .[-1]
  ticker_names2 <- unlist(ticker_names2)[unlist(ticker_names)!="ANR"] %>%
    na.omit() %>%
    .[-1]


  iter <- seq(1,ncol(ticker_dates),2)

  if(length(iter) != length(ticker_names)){
    diff <- length(iter) - length(ticker_names)
    t_iter <- c(1:length(ticker_names))[-1:diff]
  }else{
    t_iter <- 1:length(ticker_names)
  }


  tmp <- foreach(i = iter, t=t_iter, .combine = rbind) %dopar% {
    df <- data.frame(dates = lubridate::as_datetime(unlist(ticker_dates[,i])),
                     count = unlist(ticker_dates[,i+1]),
                     ticker_full = ticker_names[t],
                     ticker = ticker_names2[t],
                     stringsAsFactors = FALSE)

    if(all(is.na(df$count))){
      df <- df[1,]
    }else{
      df <- df[!is.na(df$count),]
    }




    return(df)

  }

  all_estimate_count <- rbind(all_estimate_count, tmp)
  rm(tmp)
  gc()
  cat("\n\n")
  print(l)
  cat("\n\n")

}

nrow(all_estimate_count)

haven::write_dta(data = all_estimate_count, path = "Analyst_Rating.dta")



# ---- Section 2: Target Price (BDH) -------------------------------------------


library(doParallel)


registerDoParallel(8)

all_estimate_count <- data.frame()


path <- "data/BDH best target price _ analyst rating/target price/"

files <- list.files(path)

l=1
for(l in 1:length(files)){
  ticker_dates <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, skip = 3, )
  ticker_names <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, n_max = 1)
  ticker_names2 <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, skip = 1,  n_max = 1)

  ticker_names <- unlist(ticker_names)[unlist(ticker_names)!="ANR"] %>%
    na.omit() %>%
    .[-1]
  ticker_names2 <- unlist(ticker_names2)[unlist(ticker_names)!="ANR"] %>%
    na.omit() %>%
    .[-1]



  iter <- seq(1,ncol(ticker_dates),2)

  if(length(iter) != length(ticker_names)){
    diff <- length(iter) - length(ticker_names)
    t_iter <- c(1:length(ticker_names))[-1:diff]
  }else{
    t_iter <- 1:length(ticker_names)
  }


  tmp <- foreach(i = iter, t=t_iter, .combine = rbind) %dopar% {
    df <- data.frame(dates = lubridate::as_datetime(unlist(ticker_dates[,i])),
                     count = unlist(ticker_dates[,i+1]),
                     ticker_full = ticker_names[t],
                     ticker = ticker_names2[t],
                     stringsAsFactors = FALSE)

    if(all(is.na(df$count))){
      df <- df[1,]
    }else{
      df <- df[!is.na(df$count),]
    }




    return(df)

  }

  all_estimate_count <- rbind(all_estimate_count, tmp)
  rm(tmp)
  gc()
  cat("\n\n")
  print(l)
  cat("\n\n")

}


haven::write_dta(data = all_estimate_count, path = "Target_Price.dta")




# ---- Section 3: BQL_A --------------------------------------------------------


library(doParallel)


registerDoParallel(8)

all_estimate_count <- data.frame()



path <- "data/BQL_A F/BQL_A/"

files <- list.files(path)

l=1
for(l in 1:length(files)){
  ticker_dates <- readxl::read_xlsx(paste0(path, files[l]), col_names = TRUE, skip = 2) %>%
    .[,-1:-3]

  ticker_names <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE,skip = 1, n_max = 1)

  ticker_names <- unlist(ticker_names)[unlist(ticker_names)!="ANR"]
  ticker_names2 <- unlist(ticker_names)[unlist(ticker_names)!="ANR"] %>%
    gsub(pattern = " US Equity", replacement = "", x = .)

  iter <- (seq(1,ncol(ticker_dates), 9))
  t_iter <- 1:length(ticker_names)

  i=iter[1]

  tmp <- foreach(i = iter, .combine = rbind) %dopar% {
    df <- tryCatch({data.frame(dates = lubridate::as_datetime(unlist(ticker_dates[,i])),
                     Revenue = unlist(ticker_dates[,i+1]),
                     ActualEPS = unlist(ticker_dates[,i+3]),
                     CompEPS = unlist(ticker_dates[,i+5]),
                     AdjEPS = unlist(ticker_dates[,i+7]),
                     ticker_full = ticker_names[i+1],
                     ticker = ticker_names2[i+1],
                     stringsAsFactors = FALSE)},
                   error = function(cond){return(data.frame())})

    if(nrow(df)!=0){
      df <- df[apply(df[,2:5], 1, function(x){!all(is.na(x))}),]
    }





    return(df)

  }

  all_estimate_count <- rbind(all_estimate_count, tmp)
  rm(tmp)
  gc()
  cat("\n\n")
  print(l)
  cat("\n\n")

}


haven::write_dta(data = all_estimate_count, path = "BQL_A.dta")



# ---- Section 4: BQL_F --------------------------------------------------------


library(doParallel)


registerDoParallel(8)

all_estimate_count <- data.frame()


path <- "data/BQL_A F/BQL_F/"

files <- list.files(path)

l=1
for(l in 1:length(files)){
  ticker_dates <- readxl::read_xlsx(paste0(path, files[l]), col_names = TRUE, skip = 2) %>%
    .[,-1:-3]
  ind <- grepl(pattern = "DATES", x = colnames(ticker_dates))

  (1:length(ind))[ind]
  ticker_names <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, skip=1, n_max = 1)

  ticker_names <- unlist(ticker_names)[unlist(ticker_names)!="ANR"] %>%
    unique() %>%
    na.omit()
  ticker_names2 <- unlist(ticker_names)[unlist(ticker_names)!="ANR"] %>%
    gsub(pattern = " US Equity", replacement = "", x = .) %>%
    unique() %>%
    na.omit()


  iter <- (1:ncol(ticker_dates))[ind]
  #iter <- seq(1,ncol(ticker_dates),8)
  t_iter <- 1:length(ticker_names)


  tmp <- foreach(i = iter, t=t_iter, .combine = rbind) %dopar% {
    df <- tryCatch({data.frame(dates = lubridate::as_datetime(unlist(ticker_dates[,i])),
                     MeanEPS = unlist(ticker_dates[,i+1]),
                     CountEPS = unlist(ticker_dates[,i+2]),
                     MedianEPS = unlist(ticker_dates[,i+3]),
                     StdEPS = unlist(ticker_dates[,i+4]),
                     MeanRevenue = unlist(ticker_dates[,i+5]),
                     CountRevenue = unlist(ticker_dates[,i+6]),
                     ticker_full = ticker_names[t],
                     ticker = ticker_names2[t],
                     stringsAsFactors = FALSE)},
                   error = function(cond){return(data.frame())})



    if(nrow(df)!=0){
      df <- df[apply(df[,2:6], 1, function(x){!all(is.na(x))}),]
    }



    return(df)

  }

  all_estimate_count <- rbind(all_estimate_count, tmp)
  rm(tmp)
  gc()
  cat("\n\n")
  print(l)
  cat("\n\n")

}

haven::write_dta(data = all_estimate_count, path = "BQL_F.dta")



# ---- Section 5: BDH GAAP EPS Actuals -----------------------------------------


library(dplyr)
library(doParallel)


registerDoParallel(8)
###        BDH GAAP EPS actuals


all_estimate_count <- data.frame()


path <- "data/BDH GAAP EPS actuals/"

files <- list.files(path)

l=1
for(l in 1:length(files)){
  ticker_dates <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, skip = 1)[-1,]
  ticker_names <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, n_max = 1)
  #ticker_names2 <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, skip = 1,  n_max = 1)

  ticker_names <- unlist(ticker_names)
  ticker_names2 <- unlist(ticker_names) %>%
    gsub(pattern = " US Equity|US EQUITY", replacement = "", x = .)


  iter <- (1:length(ticker_names))[!is.na(ticker_names)]


  tmp <- foreach(i = iter, .combine = rbind) %dopar% {
    df <- data.frame(dates = lubridate::as_datetime(unlist(ticker_dates[,i])),
                     value = unlist(ticker_dates[,i+1]),
                     ticker_full = ticker_names[i],
                     ticker = ticker_names2[i],
                     stringsAsFactors = FALSE)

    if(all(is.na(df$value))){
      df <- df[1,]
    }else{
      df <- df[!is.na(df$value),]
    }




    return(df)

  }

  all_estimate_count <- rbind(all_estimate_count, tmp)
  rm(tmp)
  gc()
  cat("\n\n")
  print(l)
  cat("\n\n")

}

nrow(all_estimate_count)

haven::write_dta(data = all_estimate_count, path = "BDH_GAAP_EPS_actuals.dta")



# ---- Section 6: BQL GAAP E and A ---------------------------------------------


all_estimate_count <- data.frame()


path <- "data/BQL GAAP_E_and_A/"

files <- list.files(path)

l=1
for(l in 1:length(files)){
  ticker_dates <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, skip = 3)
  ticker_names <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, n_max = 1)
  #ticker_names2 <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, skip = 1,  n_max = 1)

  ticker_names <- unlist(ticker_names)
  ticker_names2 <- unlist(ticker_names) %>%
    gsub(pattern = " US Equity|US EQUITY", replacement = "", x = .)


  iter <- (1:length(ticker_names))[!is.na(ticker_names)]


  tmp <- foreach(i = iter, .combine = rbind) %dopar% {
    df <- tryCatch({data.frame(dates = lubridate::as_datetime(unlist(ticker_dates[,i])),
                     MeanEPS = unlist(ticker_dates[,i+1]),
                     ActualEPS = unlist(ticker_dates[,i+2]),
                     ticker_full = ticker_names[i],
                     ticker = ticker_names2[i],
                     stringsAsFactors = FALSE)},
                   error = function(cond){return(data.frame())})

    if((all(is.na(df$MeanEPS)) & all(is.na(df$ActualEPS)))){
      df <- df[1,]
    }else{
      ind <- (!is.na(df$MeanEPS) | !is.na(df$ActualEPS))
      df <- df[ind,]
    }




    return(df)

  }

  all_estimate_count <- rbind(all_estimate_count, tmp)
  rm(tmp)
  gc()
  cat("\n\n")
  print(l)
  cat("\n\n")

}

nrow(all_estimate_count)

haven::write_dta(data = all_estimate_count, path = "BQL_GAAP_E_and_A.dta")




# ---- Section 7: BQL Market E and A -------------------------------------------


all_estimate_count <- data.frame()


path <- "data/BQL Market_E_and_A/"

files <- list.files(path)

l=1
for(l in 1:length(files)){
  ticker_dates <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, skip = 3)
  ticker_names <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, n_max = 1)
  #ticker_names2 <- readxl::read_xlsx(paste0(path, files[l]), col_names = FALSE, skip = 1,  n_max = 1)

  ticker_names <- unlist(ticker_names)
  ticker_names2 <- unlist(ticker_names) %>%
    gsub(pattern = " US Equity|US EQUITY", replacement = "", x = .)


  iter <- (1:length(ticker_names))[!is.na(ticker_names)]


  tmp <- foreach(i = iter, .combine = rbind) %dopar% {
    df <- data.frame(dates = lubridate::as_datetime(unlist(ticker_dates[,i])),
                     MeanEPS = unlist(ticker_dates[,i+1]),
                     ActualEPS = unlist(ticker_dates[,i+2]),
                     ticker_full = ticker_names[i],
                     ticker = ticker_names2[i],
                     stringsAsFactors = FALSE)

    if((all(is.na(df$MeanEPS)) & all(is.na(df$ActualEPS)))){
      df <- df[1,]
    }else{
      ind <- (!is.na(df$MeanEPS) | !is.na(df$ActualEPS))
      df <- df[ind,]
    }




    return(df)

  }

  all_estimate_count <- rbind(all_estimate_count, tmp)
  rm(tmp)
  gc()
  cat("\n\n")
  print(l)
  cat("\n\n")

}

nrow(all_estimate_count)

haven::write_dta(data = all_estimate_count, path = "BQL_Market_E_and_A.dta")
