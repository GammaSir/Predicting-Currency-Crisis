# This file focus on preparing a version for regression
version1 <- tbl_df(select(merge_final,Country:country))

# use export and import data to generate cagdp data.
version1 <- mutate(version1,ca=export-import)
version1$cagdp <- version1$ca/version1$gdp
# cuttail the time period
version1 <- filter(version1,time>ymd(19800101))


# try to plot the reserve change, inapplicable
# brazil$reserve_ln <- log(brazil$reserve)
# brazil <- filter(brazil,time>ymd(19800101),time<ymd(19900101))
# brazil$time2 <- as.numeric(brazil$time)
# qplot(brazil$time2,log(brazil$reserve))+geom_smooth()
#   scale_x_continuous(breaks = seq(3683,7274,by=200 ))


ma <- rollmeanr(version1$reserve,12)
ma2 <- rep(NA,6976)
ma2[12:6976] <- ma
version1$ma <- ma2
version1$diff_reserve <- (version1$reserve - version1$ma)/version1$ma
version2 <- filter(version1,diff_reserve < -0.3)

# x.Date <- as.Date(paste(2004, rep(1:4, 4:1), sample(1:28, 10), sep = "-"))
# x <- zoo(rnorm(12), x.Date)

# rollmean(x, 3)
# rollmeanr(x,3)
# rollmax(x, 3)
# rollmedian(x, 3)
# rollsum(x, 3)
# 
# xm <- zoo(matrix(1:12, 4, 3), x.Date[1:4])
# rollmean(xm, 3)
# rollmax(xm, 3)
# rollmedian(xm, 3)
# rollsum(xm, 3)
# 
# rollapply(xm, 3, mean) # uses rollmean
# rollapply(xm, 3, function(x) mean(x)) # does not use rollmean