library(data.table)
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(zoo)
#################

# CA/GDP per quarterly from OECD
ca_gdp <- read_excel("/Users/mac/desktop/data2/CA-GDP, level from OECD, quarterly.xlsx",skip=5)
ca_gdp_2 <- ca_gdp[-c(1,40),-2]
names(ca_gdp_2)[c(1,2)] <- c("Country","Unit")
ca_gdp_3 <- gather(ca_gdp_2,time,cagdp,-c(Country,Unit)) # transform from wide to long
ca_gdp_3$cagdp <- as.numeric(ca_gdp_3$cagdp) # data type
ca_gdp_3$Country <- as.factor(ca_gdp_3$Country) # data type
ca_gdp_3$Country <- str_trim(ca_gdp_3$Country)
glimpse(ca_gdp_3)

  ca_gdp_4 <- rep(ca_gdp_3,3) # to triple quarter data, making it equal to month number
  ### transform time "Q1-1959" to "1959-Q1"
  time_1 <- str_split(ca_gdp_4$time,"-")
  time_2 <- data.frame(time_1)
  time_3 <- t(time_2)
  time_4 <- paste(time_3[,2],time_3[,1],sep="-")
  ca_gdp_4$time <- time_4

  ### creat new time tag, lablled by month not by quarter
  ca_gdp_5 <- ca_gdp_4[order(ca_gdp_4$Country,ca_gdp_4$time),]
  ntime <- ymd(19590701)
  ntime_2 <- c(ntime+months(0:680))
  ntime_3 <- rep(ntime_2,38)
  ca_gdp_5$time <- ntime_3

#######################
  
# export from OECD, monthly
export <- read_excel("/Users/mac/desktop/data2/export from OECD, monthly.xlsx",skip=5)
export_2 <- export[-c(1,40:42),-2]
names(export_2)[c(1,2)] <- c("Country","Unit")
export_3 <- gather(export_2,time,exp,-c(Country,Unit)) # transform from wide to long
export_3$exp <- as.numeric(export_3$exp) # data type
export_3$Country <- as.factor(export_3$Country) # data type
glimpse(export_3)
export_3$Country <- str_trim(export_3$Country) # trim the blanks in Country names
export_3 <- export_3[-c(1:38),]
export_3$Country <- as.factor(export_3$Country)

  export_4 <- export_3[order(export_3$Country,export_3$time),]
  ntime <- ymd(19560101)
  ntime_2 <- c(ntime+months(0:723))
  ntime_3 <- rep(ntime_2,38)
  export_4$time <- ntime_3
  
str(export_4)
tail(export_4)

# import from OECD, monthly
import <- read_excel("/Users/mac/desktop/data2/import from OECD, monthly.xlsx",skip=5)
import_2 <- import[-c(1,40:42),-2]
names(import_2)[c(1,2)] <- c("Country","Unit")
import_3 <- gather(import_2,time,exp,-c(Country,Unit)) # transform from wide to long
import_3$exp <- as.numeric(import_3$exp) # data type
import_3$Country <- as.factor(import_3$Country) # data type
glimpse(import_3)
import_3$Country <- str_trim(import_3$Country) # trim the blanks in Country namesexport_3 <- export_3[-c(1:38),]
import_4 <- import_3[order(import_3$Country,import_3$time),]
import_4$Country <- as.factor(import_4$Country)

  ntime <- ymd(19560101)
  ntime_2 <- c(ntime+months(0:724))
  ntime_3 <- rep(ntime_2,38)
  import_4$time <- ntime_3

str(import_4)
tail(import_4)
  
# industrial production from OECD, monthly
IndustryProduction <- read_excel("/Users/mac/desktop/data2/industrial production from OECD, monthly.xlsx",skip=5)
IndustryProduction_2 <- IndustryProduction[-c(1,33:36),-2]
names(IndustryProduction_2)[c(1,2)] <- c("Country","Unit")
IndustryProduction_3 <- gather(IndustryProduction_2,time,exp,-c(Country,Unit)) # transform from wide to long
IndustryProduction_3$exp <- as.numeric(IndustryProduction_3$exp) # data type
IndustryProduction_3$Country <- as.factor(IndustryProduction_3$Country) # data type
glimpse(IndustryProduction_3)
IndustryProduction_3$Country <- str_trim(IndustryProduction_3$Country) # trim the blanks in Country names
IndustryProduction_3$Country <- as.factor(IndustryProduction_3$Country)
IndustryProduction_4 <- IndustryProduction_3[-c(1:31),]
IndustryProduction_4 <- IndustryProduction_4[order(IndustryProduction_4$Country,IndustryProduction_4$time),]

  ntime <- ymd(19510101)
  ntime_2 <- c(ntime+months(0:783))
  ntime_3 <- rep(ntime_2,31)
  IndustryProduction_4$time <- ntime_3

str(IndustryProduction_4)
tail(IndustryProduction_4)

# interest rate from OECD, monthly
InterestRate <- read_excel("/Users/mac/desktop/data2/interest rate from OECD, monthly.xlsx",skip=5)
InterestRate_2 <- InterestRate[-c(1,39:41),-2]
names(InterestRate_2)[c(1,2)] <- c("Country","Unit")
InterestRate_3 <- gather(InterestRate_2,time,exp,-c(Country,Unit)) # transform from wide to long
InterestRate_3$exp <- as.numeric(InterestRate_3$exp) # data type
InterestRate_3$Country <- as.factor(InterestRate_3$Country) # data type
glimpse(InterestRate_3)
InterestRate_3$Country <- str_trim(InterestRate_3$Country) # trim the blanks in Country names
InterestRate_4 <- InterestRate_3[,-2]
InterestRate_4 <- InterestRate_4[order(InterestRate_4$Country,InterestRate_4$time),]
InterestRate_4$Country <- as.factor(InterestRate_4$Country)

  ntime <- ymd(19530401)
  ntime_2 <- c(ntime+months(0:757))
  ntime_3 <- rep(ntime_2,37)
  InterestRate_4$time <- ntime_3

str(InterestRate_4)
tail(InterestRate_4)

# M1 from OECD, monthly
M1 <- read_excel("/Users/mac/desktop/data2/M1 from OECD, monthly.xlsx",skip=5)
M1_2 <- M1[-c(1,30),-2]
names(M1_2)[c(1,2)] <- c("Country","Unit")
M1_3 <- gather(M1_2,time,M1,-c(Country,Unit)) # transform from wide to long
M1_3$M1 <- as.numeric(M1_3$M1) # data type
M1_3$Country <- as.factor(M1_3$Country) # data type
glimpse(M1_3)
M1_3$Country <- str_trim(M1_3$Country) # trim the blanks in Country names
M1_4 <- M1_3[,-2]
M1_4 <- M1_4[order(M1_4$Country,M1_4$time),]
M1_4$Country <- as.factor(M1_4$Country)

ntime <- ymd(19510501)
ntime_2 <- c(ntime+months(0:780))
ntime_3 <- rep(ntime_2,28)
M1_4$time <- ntime_3

str(M1_4)
tail(M1_4)

# M3 from OECD, monthly
M3 <- read_excel("/Users/mac/desktop/data2/M3 from OECD.xlsx",skip=5)
M3_2 <- M3[-c(1,30),-2]
names(M3_2)[c(1,2)] <- c("Country","Unit")
M3_3 <- gather(M3_2,time,M3,-c(Country,Unit)) # transform from wide to long
M3_3$M3 <- as.numeric(M3_3$M3) # data type
M3_3$Country <- as.factor(M3_3$Country) # data type
glimpse(M3_3)
M3_3$Country <- str_trim(M3_3$Country) # trim the blanks in Country names
M3_4 <- M3_3[,-2]
M3_4 <- M3_4[order(M3_4$Country,M3_4$time),]
M3_4$Country <- as.factor(M3_4$Country)

  ntime <- ymd(19510501)
  ntime_2 <- c(ntime+months(0:780))
  ntime_3 <- rep(ntime_2,28)
  M3_4$time <- ntime_3

str(M3_4)
tail(M3_4)

# share prices from OECD, monthly
ShareP <- read_excel("/Users/mac/desktop/data2/share prices from OECD.xlsx",skip=5)
ShareP_2 <- ShareP[-c(1,45:47),-2]
names(ShareP_2)[c(1,2)] <- c("Country","Unit")
ShareP_3 <- gather(ShareP_2,time,ShareP,-c(Country,Unit)) # transform from wide to long
ShareP_3$ShareP <- as.numeric(ShareP_3$ShareP) # data type
ShareP_3$Country <- as.factor(ShareP_3$Country) # data type
glimpse(ShareP_3)
ShareP_3$Country <- str_trim(ShareP_3$Country) # trim the blanks in Country names
ShareP_4 <- ShareP_3[,-2]
ShareP_4 <- ShareP_4[order(ShareP_4$Country,ShareP_4$time),]
ShareP_4$Country <- as.factor(ShareP_4$Country)

  ntime <- ymd(19500101)
  ntime_2 <- c(ntime+months(0:796))
  ntime_3 <- rep(ntime_2,43)
  ShareP_4$time <- ntime_3

str(ShareP_4)
tail(ShareP_4)

# Exchange rate from IMF, daily
# Has transformed to monthly
# This is different.
ExRate <- read_excel("/Users/mac/desktop/data2/Exchange_Rate_Report from IMF, daily.xlsx",skip=1)
ExRate2 <- ExRate[-c(4350:4389),]
names(ExRate2) <- str_trim(names(ExRate2))
ExRate3 <- gather(ExRate2,Country,ExRate,-Date) # make wide to long
ExRate3$Time <- cut(ExRate3$Date,breaks="month") # cut the time period by month
ExRate4 <- aggregate(ExRate3$ExRate,by=list(ExRate3$Country,ExRate3$Time),FUN = mean,na.rm=TRUE) # take average by country and time
names(ExRate4) <- c("Country","Time","ExRate")
ExRate4$Country <- as.factor(ExRate4$Country)

  ntime <- ymd(19990101)
  ntime_2 <- c(ntime+months(0:203))
  ntime_3 <- rep(ntime_2,54)
  ExRate4$time <- ntime_3

ExRate4 <- ExRate4[,-2]
str(ExRate4)
tail(ExRate4)

# Exchange Rate from Reinhart Database, 194X-1998
ExchangeRate <- read_excel("/Users/mac/desktop/data2/ExchangeRate.xlsx",skip=2,sheet = 2)
ExchangeRate <- ExchangeRate[-c(637:676),-95]
names(ExchangeRate)[1] <- "time"
ExchangeRate2 <- gather(ExchangeRate,Country,ExRate,-time)
ExchangeRate2$Country <- as.factor(ExchangeRate2$Country)

  ntime <- ymd(19460101) # new time series original
  ntime_2 <- c(ntime+months(0:635)) # new time series period for a single country
  ntime_3 <- rep(ntime_2,93) # new time series for all countries
  ExchangeRate2$time <- ntime_3

str(ExchangeRate2)
tail(ExchangeRate2)

###############################

# Domestic Credit from BIS, quarterly
credit<-read_excel("/Users/mac/desktop/Data2/credit.xlsx",sheet=3)
credit<-credit[-c(1,2),]
names<-colnames(credit)
select1<-'Non financial sector - All sectors - Market value - US Dollar - Adjusted for breaks'
name_index<-grep(select1,names,fixed=TRUE)
length(name_index)
credit2<-credit[,c(1,name_index)]
length(colnames(credit2))
names_credit2<-colnames(credit2)
select2<-' - Non financial sector - All sectors - Market value - US Dollar - Adjusted for breaks'
names_credit3<-sub(select2,'',names_credit2,fixed=FALSE)
credit3<-credit2
names(credit3)[1:48]<-names_credit3
credit4<-credit3
names(credit4) <- str_trim(names(credit4))
credit4$`Quarterly Values` <- as.numeric(credit4$`Quarterly Values`)
credit4$`Quarterly Values` <- as.Date(credit4$`Quarterly Values`,origin="1899-12-30")
credit5 <- credit4[,-c(2,3,4)]
names(credit5)[1] <- "Time"
credit6 <- gather(credit5,Country,credit,-Time)

  ### this subsection to make quarterly data to monthly data
  credit7 <- rep(credit6,3) # make number of quarter = number of months
  credit8 <- credit7[order(credit7$Country,credit7$Time),]
  ntime <- ymd(19400701) # new time series original
  ntime_2 <- c(ntime+months(0:908)) # new time series period for a single country
  ntime_3 <- rep(ntime_2,44) # new time series for all countries
  credit8$Time <- ntime_3
  
credit8$credit <- as.numeric(credit8$credit)
str(credit8)
tail(credit8)
###########################

# GDP quarterly
  gdp <- fread("/Users/mac/desktop/data2/GDP quarterly from OECD.csv")
  names(gdp)[1] <- "Country"
  gdp$`Flag Codes`<-NULL
  gdp2 <- spread(gdp,TIME,Value)
  gdp3 <- gather(gdp2,time,gdp,-c(1:5))
  gdp4 <- gdp3[,-c(2:5)]
  # the following transform quarterly to monthly
  gdp5 <- rep(gdp4,3)
  gdp6 <- gdp5[order(gdp5$Country,gdp5$time),]
  ntime <- ymd(19600401) # new time series original
  ntime_2 <- c(ntime+months(0:671)) # new time series period for a single country
  ntime_3 <- rep(ntime_2,50) # new time series for all countries
  gdp6$time <- ntime_3
  
str(gdp6)
tail(gdp6)

# inflation from OECD, monthly
inflation <- fread("/Users/mac/desktop/data2/inflation from OECD, monthly.csv")
names(inflation)[1] <- "Country"
inflation$`Flag Codes`<-NULL
inflation2 <- spread(inflation,TIME,Value)
inflation3 <- gather(inflation2,time,inflation,-c(1:5))
inflation4 <- inflation3[,-c(2:5)]
inflation4$Country <- as.factor(inflation4$Country)

  inflation4 <- inflation4[order(inflation4$Country,inflation4$time),]
  ntime <- ymd(19510501) # new time series original
  ntime_2 <- c(ntime+months(0:780)) # new time series period for a single country
  ntime_3 <- rep(ntime_2,49) # new time series for all countries
  inflation4$time <- ntime_3

str(inflation4)
tail(inflation4)

# reserve from OECD, monthly
reserve <- fread("/Users/mac/desktop/data2/reserve from OECD, monthly.csv")
names(reserve)[1] <- "Country"
reserve$`Flag Codes`<-NULL
reserve2 <- spread(reserve,TIME,Value)
reserve3 <- gather(reserve2,time,reserve,-c(1:5))
reserve4 <- reserve3[,-c(2:5)]
reserve4$Country <- as.factor(reserve4$Country)

  reserve4 <- reserve4[order(reserve4$Country,reserve4$time),]
  ntime <- ymd(19550501) # new time series original
  ntime_2 <- c(ntime+months(0:719)) # new time series period for a single country
  ntime_3 <- rep(ntime_2,43) # new time series for all countries
  reserve4$time <- ntime_3

str(reserve4)
tail(reserve4)


####################
#transform all the data into excel
# str(ca_gdp_5)
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.7.0_60\\jre')
# library(xlsx)
# write.xlsx(ca_gdp_5,'ca_gdp.xlsx')
# write.xlsx(credit8,'domestic credit.xlsx')
# write.xlsx(ExchangeRate2,'ExchangeRate.xlsx')
# write.xlsx(export_4,'export.xlsx')
# write.xlsx(ExRate4,'ExRate.xlsx')
# write.xlsx(gdp5,'GDP.xlsx')
# write.xlsx(import_4,'import.xlsx')
# write.xlsx(IndustryProduction_4,'IndustryProduction.xlsx')
# write.xlsx(inflation4,'inflation.xlsx')
# write.xlsx(InterestRate_4,'InterestRate.xlsx')
# write.xlsx(M1_4,'M1.xlsx')
# write.xlsx(M3_4,'M3.xlsx')
# write.xlsx(reserve4,'reserve.xlsx')
# write.xlsx(ShareP_4,'sharePrice.xlsx')




