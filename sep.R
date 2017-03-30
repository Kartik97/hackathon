c<-read.csv("rainfall.csv",stringsAsFactors = FALSE)
library(dplyr)
library(tseries)
library(forecast)
library(TSA)
head(c)
rainfall<-data.frame(month=c(),rain=c())
c<-filter(c,SD_Name=="WEST UTTAR PRADESH")
head(c)
tail(c)
c<-filter(c,YEAR<2014)
library(reshape)
str(c)


####


d=c
d<-select(d,YEAR:JAN)
head(c)
meljan<-melt(d,c("YEAR"))
meljan<-arrange(meljan,YEAR)
head(meljan)
meljan<-ts(meljan$value,frequency=1)
#mel
adf.test(meljan)
model<-auto.arima(meljan)
modeljan<-arima(mel,order=c(2,2,1))
prejan<-forecast(modeljan,h=6)
plot(prejan)
prejan<-as.data.frame(prejan)
newd<-data.frame(month="JAN14",rain=prejan[1,1])
rainfall<-rbind(rainfall,newd)
rm(newd)


####


d=c
d<-select(d,YEAR:FEB)
d$JAN<-NULL
head(d)
melfeb<-melt(d,c("YEAR"))
melfeb<-arrange(melfeb,YEAR)
head(melfeb)
melfeb<-ts(melfeb$value,frequency=1)
#mel
adf.test(melfeb)
model<-auto.arima(melfeb)
acf(melfeb)
pacf(melfeb)
modelfeb<-arima(melfeb,order=c(0,1,1))
prefeb<-forecast(modelfeb,h=4)
plot(prefeb,type='l')
prefeb<-as.data.frame(prefeb)
newd<-data.frame(month="FEB14",rain=prefeb[1,1])
rainfall<-rbind(rainfall,newd)
rm(newd)


####

d=c
d<-select(d,YEAR,MAR)
head(d)
melmar<-melt(d,c("YEAR"))
melmar<-arrange(melmar,YEAR)
head(melmar)
melmar<-ts(melmar$value,frequency=1)
#mel
adf.test(melmar)
model<-auto.arima(melmar)
acf(melmar)
pacf(melmar)
modelmar<-arima(melmar,order=c(0,1,1))
premar<-forecast(modelmar,h=1)
plot(premar,type='l')
premar<-as.data.frame(premar)
newd<-data.frame(month="MAR14",rain=premar[1,1])
rainfall<-rbind(rainfall,newd)
rm(newd)


####



####




write.csv(rainfall,"prediction.csv")
