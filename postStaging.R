#loads CSV
#creates Pivot
#creates trading calendar
#interpolates

library(dplyr)
library(reshape2)
library(imputeTS)
library(PerformanceAnalytics)
library(zoo)

source("staging.R")
source("TradingDates.R")

colMax <- function(data) sapply(data, max, na.rm = TRUE)

adjustedDF <- read.csv(file="adjustedDF.csv", header=TRUE,colClasses=c('character','Date','numeric','numeric','numeric','numeric','numeric','numeric'))

trading_dates <- read.csv(file="trading_dates.csv", header=TRUE,colClasses=c('Date'))

adjusted_pvt <- reshape2::dcast(adjustedDF, Date ~ Symbol,value.var='Adjusted',fun.aggregate = mean, fill=NULL)
adjusted_pvt <- merge.data.frame(x=trading_dates[,'Date',drop=F],y=data.frame(adjusted_pvt),by='Date',all.x=T)
adjusted_pvt <- na_interpolation(adjusted_pvt,options=LINEAR)

adjusted <- group_split(adjustedDF %>% group_by(Symbol))

#group_keys(adjustedDF %>% group_by(Symbol))
#skip date
names(adjusted) <- colnames(adjusted_pvt[-1])

#remove columns with NA's
adjusted_pvt <- adjusted_pvt[,colSums(is.na(adjusted_pvt))<nrow(adjusted_pvt)]

#filter out bad daily returns (over 500%)

rownames(adjusted_pvt) <- adjusted_pvt$Date
#uniques <- length(unique(combined[,y]))
#if(uniques>=5){uniques=0}

adjusted_pvt_returns <- CalculateReturns(adjusted_pvt[,2:ncol(adjusted_pvt)], method="discrete")
rownames(adjusted_pvt_returns) <- adjusted_pvt$Date
adjusted_pvt_returns <- tail(adjusted_pvt_returns,-1)

max_daily_ret <- colMax(adjusted_pvt_returns)

adjusted_pvt_returns <- adjusted_pvt_returns[names(adjusted_pvt_returns)[which(max_daily_ret<=5)]]
adjusted_pvt <- adjusted_pvt[colnames(adjusted_pvt_returns)]
adjusted <- adjusted[colnames(adjusted_pvt_returns)]

totalReturns <- Return.cumulative(adjusted_pvt_returns)
median(totalReturns)
hist(totalReturns)

#filter  by excessive total returns

outliers <- totalReturns[,which(totalReturns>15),drop=FALSE]

print(c(("outlier's"),paste(colnames(outliers),outliers)))
totalReturns <- totalReturns[,which(totalReturns<=15),drop=FALSE]

adjusted_pvt_returns <- adjusted_pvt_returns[,colnames(totalReturns)]
adjusted_pvt <- adjusted_pvt[colnames(adjusted_pvt_returns)]
adjusted <- adjusted[colnames(adjusted_pvt_returns)]

bottomTop10pct <- quantile(totalReturns,probs=c(.025,.975))
print(bottomTop10pct)
bottom_pct <- totalReturns[,which(totalReturns<=bottomTop10pct[1])]
top_pct <- totalReturns[,which(totalReturns>=bottomTop10pct[2])]

fwrite(file="top_pct.csv",list(names(top_pct)))
fwrite(file="bottom_pct.csv",list(names(bottom_pct)))
#fwrite(file="adjusted_pvt_returns.csv",adjusted_pvt_returns)
#https://stackoverflow.com/questions/20748721/write-xts-zoo-object-to-csv-with-index
#write.zoo(adjusted_pvt_returns,file="adjusted_pvt_returns.csv", index.name = "Date", row.names=TRUE)
saveRDS(adjusted_pvt_returns, file= 'adjusted_pvt_returns.RData')
saveRDS(adjusted, file= 'adjusted.RData')
file.remove(file="outliers.csv")
fwrite(file="outliers.csv",outliers)
fwrite(file="totalReturns.csv",totalReturns)
saveRDS(adjusted_pvt,file='adjusted_pvt.RData')