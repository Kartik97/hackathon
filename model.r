c<-read.csv("rainfall.csv")
library(dplyr)
library(tseries)
head(c)
d=c
c<-filter(c,SD_Name=="WEST UTTAR PRADESH")
c<-filter(c,YEAR<=2013)
c<-select(c,YEAR:DEC)
#d<-filter(d,YEAR>2010)
d<-filter(d,SD_Name=="WEST UTTAR PRADESH")
d<-select(d,YEAR:DEC)
d<-filter(d,YEAR<=2014)
head(c)
library(reshape)
mel<-melt(c,c("YEAR"))
meld<-melt(d,c("YEAR"))
head(mel)
mel<-arrange(mel,YEAR)
meld<-arrange(meld,YEAR)
head(mel)
mel<-ts(mel$value,frequency=12)
meld<-ts(meld$value,frequency=12)
class(mel)
#model<-arima(mel,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=24))
model<-Arima(mel,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=12))
#model1<-auto.arima(mel)
#summary(model)
#tail(model)
library(forecast)
pre<-forecast(model,h=12)
#plot(pre)
plot(pre,xlim=c(63,65))
lines(meld,col='red')
par(mfrow=c(1,1))
#write.csv(pre,"predictedindia.csv")

