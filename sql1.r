#install.packages("sqldf")
library(sqldf)
crop<-read.csv("rainmonth.csv")
pred<-read.csv("Prediction.csv",stringsAsFactors = FALSE)
attach(crop)
head(crop)
names(pred)
#irrigation<-res$rfrom-pred$forecast[2]
#irrigation
#summary(pred)
#f<-filter(pred,month=='jan')
#cropsel<-res$crop
#cropsel<-as.character(cropsel)
#f$crop<-res

#result<-cbind(cropsel,irrigation)
#result

############
#q<-"SELECT * FROM crop where mfrom='may'"
#res<-sqldf(q)
#res
#predicted<-data.frame(status=c(),reqiurement=c())
library(tcltk)
crops <- function(month)
{
  #month="feb"
 # q<-"SELECT * FROM crop where mfrom='",month,"'"
  q<-paste("SELECT * FROM crop where mfrom='",month,"'",sep="")
  res<<-sqldf(q)
  res
}
#}

##
rm(predicted)
irr<-function()
{
    res
    pred
    predicted<<-data.frame(status=c(),reqiurement=c(),crop=c())
    n <- nrow(res)
    n 
    for(i in 1:n){
      #  print(i)
      x <- which(pred$month==res[i,4])
      y <- which(pred$month==res[i,5])
      x
      y
      
      #sum = pred[which(pred$month==res[i,4]),2]
      sum =0
      for(j in x:y){
        sum = sum+ pred[j,2]
      }
      sum=sum/(y-x)
      sum
      res
      if(sum>res[i,2] & sum<res[i,3]){
        ar<-c("Appropraite")
        arr<-c(0)
      } else if(sum<res[i,2])
      {
        ar<-c("deficient")
        arr<-c(res[i,2]-sum)
      } else
      {
        ar<-c("extra")
        arr<-c(res[i,2]-sum)
      }
      newd<<-data.frame(status=ar,reqiurement=arr,crop=res[i,1])
      newd
      predicted<<-rbind(predicted,newd)
      predicted
    }
}