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
version1 <- select(version1,-diff_reserve)
version1$cagdp <- version1$ca/version1$gdp
version1$time <- as.numeric(version1$time)
version1$time <- as.Date(version1$time,origin=ymd(19600101))
# cuttail the time period
version1 <- filter(version1,time>ymd(19800101))


# currency pressure index

## monthly percentage change in exchange rate
version2 <- mutate(version1,ExRate_percent=(ExRate-lag(ExRate))/lag(ExRate)*100)
version2 <- mutate(version2,reserve_percent=(reserve-lag(reserve))/lag(reserve)*100)
alpha <- sd(version2$reserve_percent,na.rm=TRUE)/(sd(version2$ExRate_percent,na.rm=TRUE)+sd(version2$reserve_percent,na.rm = TRUE))   # alpha is the weight for ExRate_percent, and 1-alpha is the weight for reserve_percent




version3 <- group_by(version1,Country)
version4 <- mutate(version3,ExRate_percent=(ExRate-lag(ExRate))/lag(ExRate)*100)
version4 <- group_by(version4,Country)
version4 <- mutate(version4,reserve_percent=(reserve-lag(reserve))/lag(reserve)*100)

version5 <- group_by(version4,Country)
alpha <- summarise(version5,sd(reserve_percent,na.rm=TRUE)/(sd(ExRate_percent,na.rm=TRUE)+sd(reserve_percent,na.rm = TRUE)))





version2$currency_index <- alpha*version2$ExRate_percent - (1-alpha)*version2$reserve_percent


qplot(version2$time,version2$currency_index)
