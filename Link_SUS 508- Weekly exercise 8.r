#Art Link
source('https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/Library.r')
TS=read.csv('https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/PIT%20aiport%20climate%20data.csv')
TS$A=as.POSIXct(paste0(TS$DATE),format="%Y%m%d")
#FIRST RUN
TS1=filter(TS,DATE>19990201)
TS1=filter(TS1,PRCP>=0)#get rid of -... may also get rid of zeros or count them... very important, decided to count zeros

plot(PRCP~A,data=TS1,type='l')
A_ts=ts(TS1$PRCP,frequency=365)
plot(decompose(A_ts))

#Second Run
#Do not need to limit date
library(tidyr)
TS2=filter(TS1,TMAX>=-2)
library(stringi)#sweet package
TS2$B=stri_extract_all_regex(TS2$A,'^[[:digit:]]{4}')
TS2$B=unlist(TS2$B)
Yoo=aggregate(TMAX~as.factor(B),FUN='mean',data=TS2)
colnames(Yoo)=c('Year','Max Temp')
plot.ts(Yoo$`Max Temp`)
