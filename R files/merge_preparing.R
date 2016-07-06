setwd("/Users/mac/desktop/Data4")
library(foreign)
# yesha <- c(ca_gdp5,credit8, ExchangeRate2, export_4, import_4, ExRate4,gdp6,)

## change the names of data.frames

unique(ca_gdp_5$Country)
#ca_gdp_6 <- subset(ca_gdp_5,Country==c("United States","Greece","Iceland","Ireland"))
ca_gdp_6 <- subset(ca_gdp_5,Country=="United States" | Country=="Greece" | Country=="Iceland" |
                     Country=="Ireland" | Country=="Japan" | Country=="Korea" | Country=="Spain" |
                     Country=="Hungary" | Country=="Mexico" | Country=="Poland" | Country=="Turkey" |
                     Country=="Brazil" | Country=="Indonesia" | Country=="Russia" |
                     Country=="South Africa" | Country=="Colombia")
ca_gdp_6$Country <- as.character(ca_gdp_6$Country)
ca_gdp_6$Country <- tolower(ca_gdp_6$Country)


credit9 <- subset(credit8,Country=="United States" | Country=="Greece" | Country=="Iceland" |
                    Country=="Ireland" | Country=="Japan" | Country=="Korea" | Country=="Spain" |
                    Country=="Hungary" | Country=="Mexico" | Country=="Poland" | Country=="Turkey" |
                    Country=="Brazil" | Country=="Indonesia" | Country=="Russia" |
                    Country=="South Africa" | Country=="Colombia")
credit9$Country <- as.character(credit9$Country)
credit9$Country <- tolower(credit9$Country)

unique(ExchangeRate2$Country)
ExchangeRate3 <- subset(ExchangeRate2,Country=="GREECE" | Country=="ICELAND" | Country=="IRELAND" | 
                          Country=="JAPAN" | Country=="KOREA" | Country=="SPAIN" |
                          Country=="HUNGARY" | Country=="MEXICO" | Country=="POLAND" | Country=="TURKEY" | 
                          Country=="BRAZIL" | Country=="INDONESIA" | Country=="RUSSIA" |
                          Country=="SOUTH AFRICA" | Country=="COLOMBIA")
ExchangeRate3$Country <- as.character(ExchangeRate3$Country)
ExchangeRate3$Country <- tolower(ExchangeRate3$Country)
ExchangeRate3$ExRate <- as.numeric(ExchangeRate3$ExRate)


export_5 <- subset(export_4,Country=="United States" | Country=="Greece" | Country=="Iceland" |
                     Country=="Ireland" | Country=="Japan" | Country=="Korea" | Country=="Spain" | 
                     Country=="Hungary" | Country=="Mexico" | Country=="Poland" | Country=="Turkey" |
                     Country=="Brazil" | Country=="Indonesia" | Country=="Russia" | 
                     Country=="South Africa" | Country=="Colombia")
export_5$Country <- as.character(export_5$Country)
export_5$Country <- tolower(export_5$Country)

# this is different
ExRate5<-ExRate4
ExRate5$Country <- as.character(ExRate5$Country)
for (i in 1:length(ExRate5$Country)) {
  if(ExRate5$Country[i]=="U.S. dollar   (USD)"){
    ExRate5$Country[i]<-"United States"
  }
  if(ExRate5$Country[i]=="GRC"){
    ExRate5$Country[i]<-"Greece"
  }
  if(ExRate5$Country[i]=="Icelandic krona   (ISK)"){
    ExRate5$Country[i]<-"Iceland"
  }
  if(ExRate5$Country[i]=="IRL"){
    ExRate5$Country[i]<-"Ireland"
  }
  if(ExRate5$Country[i]=="Japanese yen   (JPY)"){
    ExRate5$Country[i]<-"Japan"
  }
  if(ExRate5$Country[i]=="Korean won   (KRW)"){
    ExRate5$Country[i]<-"Korea"
  }
  if(ExRate5$Country[i]=="ESP"){
    ExRate5$Country[i]<-"Spain"
  }
  if(ExRate5$Country[i]=="Hungarian forint   (HUF)"){
    ExRate5$Country[i]<-"Hungary"
  }
  if(ExRate5$Country[i]=="Mexican peso   (MXN)"){
    ExRate5$Country[i]<-"Mexico"
  }
  if(ExRate5$Country[i]=="Polish zloty   (PLN)"){
    ExRate5$Country[i]<-"Poland"
  }
  if(ExRate5$Country[i]=="TUR"){
    ExRate5$Country[i]<-"Turkey"
  }
  if(ExRate5$Country[i]=="Brazilian real   (BRL)"){
    ExRate5$Country[i]<-"Brazil"
  }
  if(ExRate5$Country[i]=="Indonesian rupiah   (IDR)"){
    ExRate5$Country[i]<-"Indonesia"
  }
  if(ExRate5$Country[i]=="Russian ruble   (RUB)"){
    ExRate5$Country[i]<-"Russia"
  }
  if(ExRate5$Country[i]=="South African rand   (ZAR)"){
    ExRate5$Country[i]<-"South Africa"
  }
  if(ExRate5$Country[i]=="Colombian peso   (COP)"){
    ExRate5$Country[i]<-"Colombia"
  }
}
ExRate5$Country <- as.factor(ExRate5$Country)
ExRate6<- subset(ExRate5,Country=="United States" | Country=="Greece" | Country=="Iceland" | 
                   Country=="Ireland" | Country=="Japan" | Country=="Korea" | Country=="Spain" |
                   Country=="Hungary" | Country=="Mexico" | Country=="Poland" | Country=="Turkey" |
                   Country=="Brazil" | Country=="Indonesia" | Country=="Russia" | 
                   Country=="South Africa" | Country=="Colombia")
ExRate6$Country <- as.character(ExRate6$Country)
ExRate6$Country <- tolower(ExRate6$Country)
ExRate6$time <- as.Date(ExRate6$time)
str(ExRate6)

# gdp countries uses abbreviations
unique(gdp6$Country)
gdp7<-gdp6
gdp7$Country <- as.character(gdp7$Country)
for (i in 1:length(gdp7$Country)) {
  if(gdp7$Country[i]=="USA"){
    gdp7$Country[i]<-"United States"
  }
  if(gdp7$Country[i]=="GRC"){
    gdp7$Country[i]<-"Greece"
  }
  if(gdp7$Country[i]=="ISL"){
    gdp7$Country[i]<-"Iceland"
  }
  if(gdp7$Country[i]=="IRL"){
    gdp7$Country[i]<-"Ireland"
  }
  if(gdp7$Country[i]=="JPN"){
    gdp7$Country[i]<-"Japan"
  }
  if(gdp7$Country[i]=="KOR"){
    gdp7$Country[i]<-"Korea"
  }
  if(gdp7$Country[i]=="ESP"){
    gdp7$Country[i]<-"Spain"
  }
  if(gdp7$Country[i]=="HUN"){
    gdp7$Country[i]<-"Hungary"
  }
  if(gdp7$Country[i]=="MEX"){
    gdp7$Country[i]<-"Mexico"
  }
  if(gdp7$Country[i]=="POL"){
    gdp7$Country[i]<-"Poland"
  }
  if(gdp7$Country[i]=="TUR"){
    gdp7$Country[i]<-"Turkey"
  }
  if(gdp7$Country[i]=="BRA"){
    gdp7$Country[i]<-"Brazil"
  }
  if(gdp7$Country[i]=="IDN"){
    gdp7$Country[i]<-"Indonesia"
  }
  if(gdp7$Country[i]=="RUS"){
    gdp7$Country[i]<-"Russia"
  }
  if(gdp7$Country[i]=="ZAF"){
    gdp7$Country[i]<-"South Africa"
  }
  if(gdp7$Country[i]=="COL"){
    gdp7$Country[i]<-"Colombia"
  }
}
unique(gdp7$Country)
gdp8 <- subset(gdp7,Country=="United States" | Country=="Greece" | Country=="Iceland" |
                 Country=="Ireland" | Country=="Japan" | Country=="Korea" | Country=="Spain" | 
                 Country=="Hungary" | Country=="Mexico" | Country=="Poland" | Country=="Turkey" | 
                 Country=="Brazil" | Country=="Indonesia" | Country=="Russia" | 
                 Country=="South Africa" | Country=="Colombia")
gdp8$Country <- as.factor(gdp8$Country)
unique(gdp8$Country)
gdp8$Country <- as.character(gdp8$Country)
gdp8$Country <- tolower(gdp8$Country)


import_5 <- subset(import_4,Country=="United States" | Country=="Greece" | Country=="Iceland" | 
                     Country=="Ireland" | Country=="Japan" | Country=="Korea" | Country=="Spain" | 
                     Country=="Hungary" | Country=="Mexico" | Country=="Poland" | Country=="Turkey" |
                     Country=="Brazil" | Country=="Indonesia" | Country=="Russia" | 
                     Country=="South Africa" | Country=="Colombia")
import_5$Country <- as.character(import_5$Country)
import_5$Country <- tolower(import_5$Country)


IndustryProduction_5 <- subset(IndustryProduction_4,Country=="United States" | Country=="Greece" |
                                 Country=="Iceland" | Country=="Ireland" | Country=="Japan" | 
                                 Country=="Korea" | Country=="Spain" | Country=="Hungary" | 
                                 Country=="Mexico" | Country=="Poland" | Country=="Turkey" | 
                                 Country=="Brazil" | Country=="Indonesia" | Country=="Russia" | 
                                 Country=="South Africa" | Country=="Colombia")
IndustryProduction_5$Country <- as.character(IndustryProduction_5$Country)
IndustryProduction_5$Country <- tolower(IndustryProduction_5$Country)



# inflation uses abbreviations
inflation7<-inflation4
inflation7$Country <- as.character(inflation7$Country)
for (i in 1:length(inflation7$Country)) {
  if(inflation7$Country[i]=="USA"){
    inflation7$Country[i]<-"United States"
  }
  if(inflation7$Country[i]=="GRC"){
    inflation7$Country[i]<-"Greece"
  }
  if(inflation7$Country[i]=="ISL"){
    inflation7$Country[i]<-"Iceland"
  }
  if(inflation7$Country[i]=="IRL"){
    inflation7$Country[i]<-"Ireland"
  }
  if(inflation7$Country[i]=="JPN"){
    inflation7$Country[i]<-"Japan"
  }
  if(inflation7$Country[i]=="KOR"){
    inflation7$Country[i]<-"Korea"
  }
  if(inflation7$Country[i]=="ESP"){
    inflation7$Country[i]<-"Spain"
  }
  if(inflation7$Country[i]=="HUN"){
    inflation7$Country[i]<-"Hungary"
  }
  if(inflation7$Country[i]=="MEX"){
    inflation7$Country[i]<-"Mexico"
  }
  if(inflation7$Country[i]=="POL"){
    inflation7$Country[i]<-"Poland"
  }
  if(inflation7$Country[i]=="TUR"){
    inflation7$Country[i]<-"Turkey"
  }
  if(inflation7$Country[i]=="BRA"){
    inflation7$Country[i]<-"Brazil"
  }
  if(inflation7$Country[i]=="IDN"){
    inflation7$Country[i]<-"Indonesia"
  }
  if(inflation7$Country[i]=="RUS"){
    inflation7$Country[i]<-"Russia"
  }
  if(inflation7$Country[i]=="ZAF"){
    inflation7$Country[i]<-"South Africa"
  }
  if(inflation7$Country[i]=="COL"){
    inflation7$Country[i]<-"Colombia"
  }
}
unique(inflation7$Country)
inflation8 <- subset(inflation7,Country=="United States" | Country=="Greece" | Country=="Iceland" |
                       Country=="Ireland" | Country=="Japan" | Country=="Korea" | Country=="Spain" |
                       Country=="Hungary" | Country=="Mexico" | Country=="Poland" | Country=="Turkey" |
                       Country=="Brazil" | Country=="Indonesia" | Country=="Russia" | 
                       Country=="South Africa" | Country=="Colombia")
inflation8$Country <- as.factor(inflation8$Country)
unique(inflation8$Country)
inflation8$Country <- as.character(inflation8$Country)
inflation8$Country <- tolower(inflation8$Country)



InterestRate_5 <- subset(InterestRate_4,Country=="United States" | Country=="Greece" | 
                           Country=="Iceland" | Country=="Ireland" | Country=="Japan" | 
                           Country=="Korea" | Country=="Spain" | Country=="Hungary" | 
                           Country=="Mexico" | Country=="Poland" | Country=="Turkey" | 
                           Country=="Brazil" | Country=="Indonesia" | Country=="Russia" | 
                           Country=="South Africa" | Country=="Colombia")
InterestRate_5$Country <- as.character(InterestRate_5$Country)
InterestRate_5$Country <- tolower(InterestRate_5$Country)


M1_5 <- subset(M1_4,Country=="United States" | Country=="Greece" | Country=="Iceland" |
                 Country=="Ireland" | Country=="Japan" | Country=="Korea" | Country=="Spain" | 
                 Country=="Hungary" | Country=="Mexico" | Country=="Poland" | Country=="Turkey" | 
                 Country=="Brazil" | Country=="Indonesia" | Country=="Russia" | 
                 Country=="South Africa" | Country=="Colombia")
M1_5$Country <- as.character(M1_5$Country)
M1_5$Country <- tolower(M1_5$Country)



M3_5 <- subset(M3_4,Country=="United States" | Country=="Greece" | Country=="Iceland" |
                 Country=="Ireland" | Country=="Japan" | Country=="Korea" | Country=="Spain" |
                 Country=="Hungary" | Country=="Mexico" | Country=="Poland" | Country=="Turkey" | 
                 Country=="Brazil" | Country=="Indonesia" | Country=="Russia" | 
                 Country=="South Africa" | Country=="Colombia")
M3_5$Country <- as.character(M3_5$Country)
M3_5$Country <- tolower(M3_5$Country)


# reserve uses abbreviations
reserve7 <- reserve4
reserve7$Country <- as.character(reserve7$Country)
for (i in 1:length(reserve7$Country)) {
  if(reserve7$Country[i]=="USA"){
    reserve7$Country[i]<-"United States"
  }
  if(reserve7$Country[i]=="GRC"){
    reserve7$Country[i]<-"Greece"
  }
  if(reserve7$Country[i]=="ISL"){
    reserve7$Country[i]<-"Iceland"
  }
  if(reserve7$Country[i]=="IRL"){
    reserve7$Country[i]<-"Ireland"
  }
  if(reserve7$Country[i]=="JPN"){
    reserve7$Country[i]<-"Japan"
  }
  if(reserve7$Country[i]=="KOR"){
    reserve7$Country[i]<-"Korea"
  }
  if(reserve7$Country[i]=="ESP"){
    reserve7$Country[i]<-"Spain"
  }
  if(reserve7$Country[i]=="HUN"){
    reserve7$Country[i]<-"Hungary"
  }
  if(reserve7$Country[i]=="MEX"){
    reserve7$Country[i]<-"Mexico"
  }
  if(reserve7$Country[i]=="POL"){
    reserve7$Country[i]<-"Poland"
  }
  if(reserve7$Country[i]=="TUR"){
    reserve7$Country[i]<-"Turkey"
  }
  if(reserve7$Country[i]=="BRA"){
    reserve7$Country[i]<-"Brazil"
  }
  if(reserve7$Country[i]=="IDN"){
    reserve7$Country[i]<-"Indonesia"
  }
  if(reserve7$Country[i]=="RUS"){
    reserve7$Country[i]<-"Russia"
  }
  if(reserve7$Country[i]=="ZAF"){
    reserve7$Country[i]<-"South Africa"
  }
  if(reserve7$Country[i]=="COL"){
    reserve7$Country[i]<-"Colombia"
  }
}
unique(reserve7$Country)
reserve8 <- subset(reserve7,Country=="United States" | Country=="Greece" | Country=="Iceland" |
                     Country=="Ireland" | Country=="Japan" | Country=="Korea" | 
                     Country=="Spain" | Country=="Hungary" | Country=="Mexico" | 
                     Country=="Poland" | Country=="Turkey" | Country=="Brazil" |
                     Country=="Indonesia" | Country=="Russia" | 
                     Country=="South Africa" | Country=="Colombia")
reserve8$Country <- as.factor(reserve8$Country)
unique(reserve8$Country)
reserve8$Country <- as.character(reserve8$Country)
reserve8$Country <- tolower(reserve8$Country)


ShareP_5 <- subset(ShareP_4,Country=="United States" | Country=="Greece" | Country=="Iceland" | 
                     Country=="Ireland" | Country=="Japan" | Country=="Korea" | 
                     Country=="Spain" | Country=="Hungary" | Country=="Mexico" | 
                     Country=="Poland" | Country=="Turkey" | Country=="Brazil" | 
                     Country=="Indonesia" | Country=="Russia" | Country=="South Africa" | 
                     Country=="Colombia")
ShareP_5$Country <- as.character(ShareP_5$Country)
ShareP_5$Country <- tolower(ShareP_5$Country)

### Export to Stata dta files.

write.dta(ca_gdp_6,"ca_gdp.dta")
write.dta(credit9,"credit.dta")
write.dta(ExchangeRate3,"ExchangeRate.dta")
write.dta(export_5,"export.dta")
write.dta(ExRate6,"ExRate.dta")
write.dta(gdp8,"gdp.dta")
write.dta(import_5,"import.dta")
write.dta(IndustryProduction_5,"IndustryProduction.dta")
write.dta(inflation8,"inflation.dta")
write.dta(InterestRate_5,"interestrate.dta")
write.dta(M1_5,"m1.dta")
write.dta(M3_5,"m3.dta")
write.dta(reserve8,"reserve.dta")
write.dta(ShareP_5,"shareprice.dta")
