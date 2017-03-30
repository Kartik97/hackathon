data<-read.csv("rainfall.csv",stringsAsFactors = FALSE)
library(dplyr)
library(tseries)
library(forecast)
library(reshape)
head(data)
data<-filter(data,SD_Name=="ORISSA")

####   JAN-APR


trainjan<-filter(data,YEAR<=2012)
trainjan<-select(trainjan,YEAR,JAN:APR)

trainjan<-melt(trainjan,c("YEAR"))

head(trainjan)

trainjan<-arrange(trainjan,YEAR)

head(trainjan,12)
trainjan<-ts(trainjan$value,frequency=4)

class(trainjan)

plot(trainjan)
adf.test(trainjan,alternative = "stationary")
#p-value<0.05 (null rejected,alternative accepted)
#auto.arima(trainjan)
modjantrain<-Arima(trainjan,order=c(0,0,0))
summary(modjantrain)
#modjantrend<-Arima(jantrend,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
#modjanrandom<-Arima(janrandom,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))

janfinal<-forecast(modjantrain,h=4)


plot(janfinal,type="l")


plot(janfinal,xlim=c(50,70))



testjan<-filter(data,YEAR<=2013)
testjan<-select(testjan,YEAR,JAN:APR)
testjan<-melt(testjan,c("YEAR"))
testjan<-arrange(testjan,YEAR)
testjan<-ts(testjan$value,frequency=4)
class(testjan)
lines(testjan,col='red')


### 

#MAY-OCT



trainmay<-filter(data,YEAR<=2012)
head(trainmay)
trainmay<-select(trainmay,YEAR,MAY:OCT)
head(trainmay)
trainmay<-melt(trainmay,c("YEAR"))
head(trainmay)
trainmay<-arrange(trainmay,YEAR)
trainmay<-ts(trainmay$value,frequency=6)
class(trainmay)
auto.arima(trainmay)
modmay<-arima(trainmay,order=c(4,1,2),seasonal=list(order=c(0,0,2),period=6))
summary(modmay)
formay<-forecast(modmay,h=6)
plot(formay)
plot(formay,xlim=c(50,70))

testmay<-filter(data,YEAR<=2013)
testmay<-select(testmay,YEAR,MAY:OCT)
testmay<-melt(testmay,c("YEAR"))
testmay<-arrange(testmay,YEAR)
testmay<-ts(testmay$value,frequency=6)
class(testmay)
lines(testmay,col='red')


####






##NOV-DEC




trainnov<-filter(data,YEAR<=2012)
head(trainnov)
trainnov<-select(trainnov,YEAR,NOV:DEC)
head(trainnov)
trainnov<-melt(trainnov,c("YEAR"))
head(trainnov)
trainnov<-arrange(trainnov,YEAR)
trainnov<-ts(trainnov$value,frequency=2)
class(trainnov)
auto.arima(trainnov)
modnov<-arima(trainnov,order=c(1,0,0))
summary(modnov)
fornov<-forecast(modnov,h=6)
plot(fornov)
plot(fornov,xlim=c(50,70))

testnov<-filter(data,YEAR<=2014)
testnov<-select(testnov,YEAR,NOV:DEC)
testnov<-melt(testnov,c("YEAR"))
testnov<-arrange(testnov,YEAR)
testnov<-ts(testnov$value,frequency=2)
class(testnov)
lines(testnov,col='red')

######

janfinal<-as.data.frame(janfinal)
head(janfinal)
mayfinal<-as.data.frame(formay)
novfinal<-as.data.frame(fornov)

jan<-as.numeric(janfinal$`Point Forecast`)
head(jan)
jan<-as.data.frame(jan)
head(jan)
jan<-rename(jan,c(jan="rain"))

may<-as.numeric(mayfinal$`Point Forecast`)
may<-as.data.frame(may)
may<-rename(may,c(may="rain"))

nov<-as.numeric(novfinal$`Point Forecast`)
nov<-as.data.frame(nov)
nov<-rename(nov,c(nov="rain"))
head(nov)

final<-rbind(jan,may,nov)
head(final)
final<-ts(final$rain,frequency=12)
plot(final)

finaltest<-filter(data,YEAR==2013)
finaltest<-filter(finaltest,SD_Name=="ORISSA")
finaltest<-select(finaltest,YEAR,JAN:DEC)
finaltest<-melt(finaltest,c("YEAR"))
finaltest<-ts(finaltest$value,frequency=12)

plot(finaltest,col='red')
lines(final)
write.csv(final,"Final_Prediction.csv")
