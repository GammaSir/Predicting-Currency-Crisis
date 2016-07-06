# exchange rate of hungary to USD
hun <- read_excel("/Users/mac/desktop/data2/Hungary to USD.xlsx")
hun <- hun[-1,]
hun <- hun[,c(1,4)]
names(hun)[1] <- "time"
hun$time <- as.numeric(hun$time)
hun$time <- as.Date(hun$time, origin= "1899-12-30")
hun2 <- hun
hun2$time <- cut(hun2$time,breaks="month") # cut the time period by month
hun2 <- aggregate(hun2$USD,by=list(hun2$time),FUN = mean,na.rm=TRUE) # take average by country and time
names(hun2) <- c("time","exUSD")


# exchange rate of Poland to USD
library(readxl)
pol<-read_excel('Poland ER from OECD2.xlsx',skip=1)
library(reshape)
library(lubridate)
pol<-pol[-1,]
names(pol)[1]<-'country'
md<-melt(pol,id=('country'))
names(md)[c(2,3)]<-c('time','ER')
ntime <- ymd(19910101) # new time series original
ntime_2 <- c(ntime+months(0:304)) # new time series period for a single country
md$time <- ntime_2
Poland<-md
poland2 <- Poland[which(Poland$time<ymd(19990101)),]
poland2$Country <- tolower(poland2$Country)

# exchange rate of Russia to USD
rus<-read_excel('Russia ER from OECD2.xlsx',skip=1)
rus<-rus[-1,]
names(rus)[1]<-'country'
rus2<-melt(rus,id=('country'))
names(rus2)[c(2,3)]<-c('time','ER')
ntime<-ymd(19920601)
ntime_2<-c(ntime+months(0:286))
rus2$time<-ntime_2
rus3 <- rus2[which(rus2$time<ymd(19990101)),]


# combine ExchangeRate and ExRate and Poland,Russia and Hungary
ExRate7<-ExRate6[order(ExRate6$Country,ExRate6$time),]
ER<-rbind(ExchangeRate3,ExRate7)
names(rus2)[c(1,3)]<-c('Country','ExRate')
names(Poland)[c(1,3)]<-c('Country','ExRate')
ER2<-rbind(ER,poland2)
ER3<-rbind(ER2,rus3)
hun2$Country<-'hungary'
names(hun2)[2]<-'ExRate'
ER4<-rbind(ER3,hun2)
ER5<-ER4[order(ER4$Country,ER4$time),]