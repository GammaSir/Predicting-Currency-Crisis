# This file focus on preparing a version for regression
setwd("/users/mac/desktop/Predicting-Currency-Crisis")

library(foreign)
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)
library(ggplot2)

version1 <- read.dta("data/Merge_final.dta")
version1 <- tbl_df(select(version1,Country:country))

# use export and import data to generate cagdp data.
version1 <- mutate(version1,ca=export-import)
  # version1 <- select(version1,-diff_reserve)
version1$cagdp <- version1$ca/version1$gdp
version1$time <- as.numeric(version1$time)
version1$time <- as.Date(version1$time,origin=ymd(19600101))
# cuttail the time period
version1 <- filter(version1,time>ymd(19800101))


# currency pressure index

## monthly percentage change in exchange rate
# version2 <- mutate(version1,ExRate_percent=(ExRate-lag(ExRate))/lag(ExRate)*100)
# version2 <- mutate(version2,reserve_percent=(reserve-lag(reserve))/lag(reserve)*100)
# alpha <- sd(version2$reserve_percent,na.rm=TRUE)/(sd(version2$ExRate_percent,na.rm=TRUE)+sd(version2$reserve_percent,na.rm = TRUE))   # alpha is the weight for ExRate_percent, and 1-alpha is the weight for reserve_percent


# monthly percentage change in ExRate and reserve
version3 <- group_by(version1,Country)
version4 <- mutate(version3,ExRate_percent=(ExRate-lag(ExRate))/lag(ExRate)*100)
version4 <- group_by(version4,Country)
version4 <- mutate(version4,reserve_percent=(reserve-lag(reserve))/lag(reserve)*100)

# ratio for ExRate and reserve (alpha)
version5 <- group_by(version4,Country)
alpha <- summarise(version5,alpha=sd(reserve_percent,na.rm=TRUE)/(sd(ExRate_percent,na.rm=TRUE)+sd(reserve_percent,na.rm = TRUE)))   # alpha is the weight for ExRate_percent, and 1-alpha is the weight for reserve_percent
version6 <- merge(version5,alpha,by="Country",all=TRUE)

version7 <- mutate(version6,pressure_index=ExRate_percent*alpha-(1-alpha)*reserve_percent)   # create currency pressure index

########

# Then we need to identify the crisis period.
# Pediods in which  the index is above its mean by more than three standard deveations are defined as crises.

version7 <- group_by(version7,Country)
version8 <- mutate(version7,index_sd=sd(pressure_index,na.rm=TRUE)) # calculate standard deviations of pressure index by country
version8 <- group_by(version8,Country)
version9 <- mutate(version8,crisis=(pressure_index>(mean(pressure_index,na.rm=TRUE)+index_sd*3))) # dummy for currency crisis

# !!! IT SEEMS THAT THE CRISIS CRITERIA IS TOO STRICT HERE.
# try this: 
# version9[which(version9$crisis==TRUE),]

dim(version9[which(version9$crisis==TRUE),])

version_check <- version9[which(version9$crisis==TRUE),c("Country","time","crisis")]

version9 <- group_by(version9)
# version10 <- mutate(version9,crisis_24m=FALSE)
# for (x in c(0:24)){
#   if (version10$crisis_24m == FALSE){
#     version10$crisis_24m <- lead(version10$crisis_24m,x)
#   }
# }
version10 <- mutate(version9,crisis_24m=(lead(crisis,1)==TRUE | lead(crisis,2)==TRUE | lead(crisis,3)==TRUE | lead(crisis,4)==TRUE | lead(crisis,5)==TRUE | lead(crisis,6)==TRUE | lead(crisis,7)==TRUE | lead(crisis,8)==TRUE | lead(crisis,9)==TRUE | lead(crisis,10)==TRUE | lead(crisis,11)==TRUE | lead(crisis,12)==TRUE | lead(crisis,13)==TRUE | lead(crisis,14)==TRUE | lead(crisis,15)==TRUE | lead(crisis,16)==TRUE | lead(crisis,17)==TRUE | lead(crisis,18)==TRUE | lead(crisis,19)==TRUE | lead(crisis,20)==TRUE | lead(crisis,21)==TRUE | lead(crisis,22)==TRUE | lead(crisis,23)==TRUE | lead(crisis,24)==TRUE))


###########

# for version 10, the variable units are as listed:
# cagdp, not necessary
# credit, level, US dollar
# export, annual %
# import, annual %
# industry_production, annual %
# inflation, annual %
# interest, long-term interest rates, level
# M1, level, 2010=100
# M3, level, 2010=100
# reserve, MLN/SDR, level
# ShareP, stock-market-price-index, level, 2010=100
# ExRate, combined data. 1940-1998, 1998-now, level, currency/$
# ca, THIS VARIABLE IS WRONGLY CALCULATED!!!
# ExRate_percent, monthly %
# reserve_percent, monthly %
# pressure_index, level
# crisis, dummy
# crisis_24m, dummy


# need to be calculated:
# deviation of real interest rate from trend
# excess of real M1 balances
# annual % for ExRate, DONE
# annual % for reserve, DONE
# annual % for M3/M1
# M3/reserve, levels
# ... still many, check table-1 of Berg and Pattilo 1999

version10 <- group_by(version10,Country)
version11 <- mutate(version10,ExRate_annual=(ExRate-lag(ExRate,12))/lag(ExRate,12)*100,reserve_annual=(reserve-lag(reserve,12))/lag(reserve,12)*100)
version12 <- mutate(version11,realinterest=interest-inflation)

# lm <- lm(version12$realinterest~version12$time)
# mutate(version12,realinterest_deviation=lm$residuals)

# to calculate the threshold


# !!!! NOTICE!!!!! ExRate is contrary to other variables!
threshold <- function(x){
  ratio <- 0
  index <- 0
  range <- max(x,na.rm=TRUE)-min(x,na.rm=TRUE)
  for(i in seq(0,1,0.005)){
    th <- min(x,na.rm = TRUE)+range*i # threshold
    dummy <- (x<th) # crisis defined by this threshold
    A <- sum(version10$crisis_24m & dummy,na.rm = TRUE)
    B <- sum(!version10$crisis_24m & dummy,na.rm = TRUE)
    C <- sum(version10$crisis_24m & !dummy,na.rm = TRUE)
    D <- sum(!version10$crisis_24m & !dummy,na.rm = TRUE)
    if (!(A+B)==0 & (A/(A+B))>ratio){
      if(A/(A+B)==1){
        print(i)
        print("NOTICE!!! The signal/(signal+noise) ratio is 1 !")
        next
      }
      ratio <- A/(A+B)
      index <- i
    }
  }
  print(paste("ratio is: ",ratio))
  print(paste("percentile is: ",index))
  th <- min(x,na.rm = TRUE)+index*range
  print(paste("threshold is: ",th))
  return(th)
}

version12 <- group_by(version12,Country)
th_try <- summarise(version12,th=threshold(ExRate_annual))
th_reserve <- summarise(group_by(version12),threshold(export))



