## !!! Notice: before excute this, make sure that all variables names are correct, and additional columns have been deleted.
library(reshape)
library(foreign)
ca_gdp_7 <- ca_gdp_6[,-2]
export_6 <- export_5[,-2]
names(export_6) <- c("Country","time","export")
import_6 <- import_5[,-2]
names(import_6)[c(2,3)] <- c("time","import")
IndustryProduction_6 <- IndustryProduction_5[,-2]
names(IndustryProduction_6)[3] <-"industry_production"
names(export_5)[3] <- "export"
names(credit9)[1]<-'time'
names(InterestRate_5) <- c("Country","time","interest")

merge1 <- merge(ca_gdp_7,credit9,by=c('Country','time'),all= TRUE)
merge2 <- merge(merge1,ExchangeRate3,by=c('Country','time'),all=TRUE)
merge3 <- merge(merge2,export_6,by=c('Country','time'),all=TRUE)
merge4 <- merge(merge3,gdp8,by=c('Country','time'),all=TRUE)
merge5 <- merge(merge4,import_6,by=c('Country','time'),all=TRUE)
merge6 <- merge(merge5,IndustryProduction_6,by=c('Country','time'),all=TRUE)
merge7 <- merge(merge6,inflation8,by=c('Country','time'),all=TRUE)
merge8 <- merge(merge7,InterestRate_5,by=c('Country','time'),all=TRUE)
merge9 <- merge(merge8,M1_5,by=c('Country','time'),all=TRUE)
merge10 <- merge(merge9,M3_5,by=c('Country','time'),all=TRUE)
merge11 <- merge(merge10,reserve8,by=c('Country','time'),all=TRUE)
merge12 <- merge(merge11,ShareP_5,by=c('Country','time'),all=TRUE)
merge13 <- merge(merge12,ER5,by=c('Country','time'),all=TRUE)
merge14 <- merge13
merge14$ExRate.x <- NULL
merge15 <- rename(merge14,c(ExRate.y='ExRate'))
merge16<-merge15
merge16$Country <- tolower(merge16$Country)

write.dta(merge16,"Merge_Final.dta")
