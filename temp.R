temp<-read.csv("temp.csv",stringsAsFactors = TRUE)
temp<-filter(temp,YEAR<2014)
#temp<-filter(temp,YEAR>1950)
temp<-select(temp,YEAR:DEC)
meltem<-melt(temp,c("YEAR"))
meltem<-arrange(meltem,YEAR)
names(meltem)
#meltem<-ts(meltem$value,frequency=12)
meltem$variable=NULL
names(meltem)
model<-arimax(mel,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=12),xreg=meltem)
model1<-auto.arima(mel,xreg=meltem)
model2<-arimax(mel, order = c(1, 0, 1), seasonal = list(order = c(1, 1, 1),period = 12),xreg = meltem)
pre<-forecast(model,h=12,xreg=meltem)
plot(pre,xlim=c(110,120))
lines(meld,col='red')
