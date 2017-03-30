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
meljan<-ts(meljan$value,frequency=1)
forecasts<-HoltWinters(meljan,beta=FALSE,gamma=FALSE)
forecasts$fitted
plot(forecasts)
forecasts$SSE
HoltWinters(meljan, beta=FALSE, gamma=FALSE, l.start=16.4)
forecasts2 <- forecast.HoltWinters(forecasts, h=5)
forecasts2
plot.forecast(forecasts2)
acf(forecasts2$residuals)
summary(forecasts2$residuals)
t<-na.omit(forecasts2$residuals)
forecasts2$residuals<-ifelse(is.na(forecasts2$residuals),median(t),forecasts2$residuals)
Box.test(forecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(forecasts2$residuals)
forecasts3<-HoltWinters(meljan,gamma=FALSE,l.start=16.4)
plot(forecasts3)
