#downloads
#adjusts
#create's csv
#called by postStaging.R

library(base)
#library(curl)
library(downloader)
library(lubridate)
library(parallel)
library(HelpersMG)
library(tidyquant)
library(microbenchmark)
library(data.table)
library(PerformanceAnalytics)
library(stringr)
library(quantmod)
library(zoo)

#source("functions.R")

source("TradingDates.R")

setSize=5
numQuarters=9
predictionIntervalTestSize = 50
desiredSize = 400

#tried to add a test=TRUE, and days=0 and checking if nrow(data.frame(data)<days/2) but wasn't working out for me
batch_get_symbols <- function(data,first.date,last.date) {
  names(data) <- data
  
  set <- mclapply(data, function (x)
  {
    data <- tq_get(x, from = first.date, to=last.date)
    
      if(nrow(data.frame(data))==1)
      {
        return(NA)
      }
    
    return (data)
  })
  #already named
  #names(set) <- subset_data
  return(set)
}

extract_year <- function (x_date)
{
  #https://stackoverflow.com/questions/36568070/extract-year-from-date
  substring(x_date,1,4) #This takes string and only keeps the characters beginning in position 7 to position 10
}

#source("lists.R")

download = 1
if (file.exists("adjustedDF.csv")) {
  timestamp <- as.Date(file.info("adjustedDF.csv")$ctime)
  if(timestamp == Sys.Date())
  {
    download=0
  }
  else{(download=1)}
  
} else {
  download=1
}

if(download)
{
  
  if (file.exists("nasdaqtraded.txt")) {
    timestamp <- as.Date(file.info("nasdaqtraded.txt")$ctime)
    if(timestamp == Sys.Date())
    {
      #do nothing
    }
    else{(wget("ftp://ftp.nasdaqtrader.com/SymbolDirectory/nasdaqtraded.txt"))}
    
  } else {
    wget("ftp://ftp.nasdaqtrader.com/SymbolDirectory/nasdaqtraded.txt")
  }
  
  
  if (file.exists("mfundslist.txt")) {
    timestamp <- as.Date(file.info("mfundslist.txt")$ctime)
    if(timestamp == Sys.Date())
    {
      #do nothing
    }
    else{(wget("ftp://ftp.nasdaqtrader.com/SymbolDirectory/mfundslist.txt"))}
    
  } else {
    wget("ftp://ftp.nasdaqtrader.com/SymbolDirectory/mfundslist.txt")
  }
    
  nasdaqTraded <- as.character(head(read.csv("nasdaqtraded.txt",sep="|")$Symbol,-2))
  mfunds <- as.character(head(read.csv("mfundslist.txt",sep="|")$Fund.Symbol,-2))
  indexes <- c("^SP500TR")
  
  length(nasdaqTraded)
  length(mfunds)
  #replaces * with -
  #https://stackoverflow.com/questions/11936339/replace-specific-characters-within-strings
  #nasdaqTraded <- unlist(mclapply(nasdaqTraded,function (x) {gsub("[$]","-",x)}))
  
  #remove after * and .
  #https://datascience.stackexchange.com/questions/8922/removing-strings-after-a-certain-character-in-a-given-text
  nasdaqTraded <- unlist(mclapply(nasdaqTraded,function (x) {gsub("[$].*","",x)}))
  nasdaqTraded <- unlist(mclapply(nasdaqTraded,function (x) {gsub("[.].*","",x)}))
  nasdaqTraded <- unique(nasdaqTraded)
  length(nasdaqTraded)
  
  mfunds <- unlist(mclapply(mfunds,function (x) {gsub("[$].*","",x)}))
  mfunds <- unlist(mclapply(mfunds,function (x) {gsub("[.].*","",x)}))
  mfunds <- unique(nasdaqTraded)
  length(mfunds)
  
  symbols <- unique(c(nasdaqTraded,mfunds))
  length(symbols)
  #print(head(nasdaqTraded))
  #mfunds <- as.character(head(read.csv("nasdaqtraded.txt",sep="|")$Symbol,-2))
  
  last.date <- as.Date(Sys.Date())
  first.date <- last.date %m-% months(3*numQuarters)
  
  first.date.minus.1week <- first.date %m-% weeks(1)
  
  years <- seq(as.integer(extract_year((first.date))):as.integer(extract_year(last.date)))+as.integer(extract_year(first.date))-1
  
  dates <- lapply(years, function (x){
    TradingDates(x)
  })
  
  trading_dates <- data.frame(as.Date(unlist(dates)))[,,drop=FALSE]
  colnames(trading_dates) <- "Date"
  rownames(trading_dates) <- trading_dates$Date
  
  trading_dates <- subset(trading_dates, rownames(trading_dates) <=  last.date & rownames(trading_dates) >= first.date)
  #print(first.date)
  #marketNames <- c("Nasdaq","Mutual")
  
  #observe({(input$symbols)})
  
  #set.seed(10)
  sampleSymbols <- sample(symbols, predictionIntervalTestSize)
  
  ptm <- proc.time()
  symbolSet <- batch_get_symbols(sampleSymbols,first.date.minus.1week,first.date)
  timed <- proc.time() - ptm
  print(timed)
  
  symbolSet <- symbolSet[!is.na(symbolSet)]
  
  df = predictionIntervalTestSize - 1
    
  tscores <- qt(c(.005, .995), df)
  proportion = length(symbolSet)/predictionIntervalTestSize
  print(proportion)
  propSdev = sqrt(proportion*(1-proportion)/predictionIntervalTestSize)
  predictionInterval = proportion + (tscores * propSdev*sqrt(1+1/predictionIntervalTestSize))
  limits = predictionInterval * predictionIntervalTestSize
  #predictionInterval * 30
  
  sampleSize = ceiling(desiredSize/predictionInterval)[1]
  
  print(paste("estimated length of time for min",desiredSize, ": ",as.integer(sampleSize*timed[3]/predictionIntervalTestSize/60),"minutes",ceiling(((sampleSize*timed[3]/predictionIntervalTestSize/60)-as.integer(sampleSize*timed[3]/predictionIntervalTestSize/60))*60),"seconds"))
  
  #set.seed(10)
  sampleSymbols <- sample(nasdaqTraded, sampleSize)
  
  symbolSet <- batch_get_symbols(sampleSymbols,first.date.minus.1week,first.date)
  
  #final list for full quarter pulls
  symbolSet <- symbolSet[!is.na(symbolSet)]
  length(symbolSet)
  
  #full quarters
  ptm <- proc.time()
  
  sampleSymbols <- c(names(symbolSet),indexes)
  symbolSet <- batch_get_symbols(sampleSymbols,first.date,last.date)
  
  timed <- proc.time() - ptm
  print(timed)
  
  threshold <- floor(nrow(trading_dates)*.99)
  
  symbolSet <- symbolSet[!is.na(symbolSet)]
  
  #do na check inside batch_get_symbols?  Too many possible errors, but it would leave that function error free (harder to troubleshoot)
  symbolSet <- mclapply(symbolSet, function(x)
  {#x=symbolSet[1]
    if(nrow(data.frame(x)) < threshold)
    {
      return(NA)
    }
    return(x)
  })
  
  symbolSet <- symbolSet[!is.na(symbolSet)]
  length(symbolSet)
  
  symbolSet <- mclapply(symbolSet, setNames, c("Symbol","Date","Open","High","Low","Close","Volume","Adjusted"))
  
  ptm <- proc.time()
  adjusted <- mclapply(names(symbolSet),function (x)
  {
    set <- data.frame(quantmod::adjustOHLC(as.xts(as.data.table(symbolSet[[x]][,c("Date","Open","High","Low","Close","Volume","Adjusted")])),use.Adjusted=TRUE,symbol.name=names(symbolSet[x])))
    d <- data.frame(as.Date(rownames(set)))[,,drop=FALSE]
    colnames(d) <- "Date"
    set <- cbind(d,set)
    return (set)
  })
  #View(adjusted)
  
  timed <- proc.time() - ptm
  print(timed)
  names(adjusted) <- names(symbolSet)
  
  adjusted <- adjusted[!is.na(adjusted)]
  length(adjusted)
  
  symbolSetDF <- do.call(rbind, symbolSet)
  
  adjustedDF <- rbindlist(as.list(adjusted),idcol="Symbol")
  
  #adjustedDF <- do.call(rbind, adjusted)

  fwrite(file="adjustedDF.csv",adjustedDF)
  fwrite(file="trading_dates.csv",trading_dates)
  
}
print("staging complete")
