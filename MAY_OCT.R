c<-read.csv("rainfall.csv",stringsAsFactors = FALSE)
library(dplyr)
library(tseries)
library(forecast)
head(c)
c<-filter(c,SD_Name=="WEST UTTAR PRADESH")
head(c)
tail(c)
d<-filter(c,YEAR<=2013)
c<-filter(c,YEAR<=2012)
library(reshape)
str(c)
c<-select(c,YEAR,MAY:OCT)
d<-select(d,YEAR,MAY:OCT)
mel<-melt(c,c("YEAR"))
mel1<-melt(d,c("YEAR"))
head(mel)
tail(mel1)
mel<-arrange(mel,YEAR)
mel1<-arrange(mel1,YEAR)
head(mel,12)
mel<-ts(mel$value,frequency=6)
mel1<-ts(mel1$value,frequency=6)
class(mel)
class(mel1)
plot(decompose(mel))
x<-decompose(mel)$trend
x1<-decompose(mel1)$trend
y<-decompose(mel)$seasonal
z<-decompose(mel)$random
x<-na.omit(x)
x1<-na.omit(x1)
z<-na.omit(z)
#t<-na.omit(x)
#t1<-na.omit(x1)
#x<-ifelse(is.na(x),median(t),x)
#x1<-ifelse(is.na(x1),median(t1),x)
#t<-na.omit(z)
#z<-ifelse(is.na(z),median(t),z)
x<-ts(x,frequency=6)
x1<-ts(x1,frequency = 6)
y<-ts(y,frequency=6)
z<-ts(z,frequency =6)
#auto.arima(x)
model<-Arima(x,order=c(2,1,2),seasonal=list(order=c(1,0,1),period=6))
model2=auto.arima(y)
model1<-Arima(y,order=c(1,0,1))
model3=auto.arima(z)
model2<-Arima(z,order=c(2,0,2))
library(forecast)
pre<-forecast(model,h=6)
pre1<-forecast(model2,h=6)
pre2<-forecast(model3,h=6)
pre<-as.data.frame(pre)
seas<-head(y,6)
seas<-as.data.frame(seas)
pre2<-as.data.frame(pre2)
s<-seas$x+pre$`Point Forecast`+pre2$`Point Forecast`
plot(s)
#lines(x1,col='red')
d<-filter(d,YEAR==2013)
d<-melt(d,c("YEAR"))
d<-ts(d$value,frequency=6)
s<-ifelse(s>0,s,0)
plot(d,col='red')
lines(s)
