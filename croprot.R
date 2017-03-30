#install.packages("sqldf")
library(sqldf)
crop<-read.csv("croprotation.csv")
month<-read.csv("rainmonth.csv")
pred<-read.csv("Prediction.csv",stringsAsFactors = FALSE)
attach(crop)
head(crop)
#names(crop)
predictedcrop<-data.frame(crop=c(),nextcrop=c(),sowing=c(),harvesting=c())
nextcrop<-function(prevcrop)
{
  q<-paste("SELECT * FROM crop where crop='",prevcrop,"'",sep="")
  res<-sqldf(q)
  res
  n<-nrow(res)
  for(i in 1:n){
    name<-res[i,2]
    q<-paste("SELECT * FROM month where crop='",name,"'",sep="")
    cropmonths<<-sqldf(q)
    newd<-data.frame(crop=res[i,1],nextcrop=res[i,2],sowing=cropmonths[1,4],harvesting=cropmonths[1,5])
    newd
    predictedcrop<<-rbind(predictedcrop,newd)
  }
}

rm(predictedcrop)
