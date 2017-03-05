setwd("~/Desktop/sfei")
sfei<-read.csv("sfeiPlusDates.csv",stringsAsFactors=F)
names(sfei)

require(ggplot2)

sub=subset(sfei,Station%in%c("D10","D12","D4","D22","D26"))

## EDA to find out which station/variable combos have most complete records
ggplot(sub, aes(x=date_dec, y=din, colour=as.factor(Station)))+geom_line()+xlim(2000,2015)
## chl
## do
## pheo
## temp
## sal


percentMissingChl=percentMissingDo=percentMissingPheo=percentMissingTemp=percentMissingSal=c()
perStation=unique(sfei$Station)
for(i in 1:length(perStation)){
  
  data=subset(sfei,date_dec>2000 &Station==perStation[i])
  percentMissingChl<-c(percentMissingChl,length(which(is.na(data$chl)))/nrow(data))
  percentMissingDo<-c(percentMissingDo,length(which(is.na(data$do)))/nrow(data))
  percentMissingPheo<-c(percentMissingPheo,length(which(is.na(data$pheo)))/nrow(data))
  percentMissingTemp<-c(percentMissingTemp,length(which(is.na(data$temp)))/nrow(data))
  percentMissingSal<-c(percentMissingSal,length(which(is.na(data$sal)))/nrow(data))
  
  ## chl
  ## do
  ## pheo
  ## temp
  ## sal
  
  
}

percentMissingChl[which(perStation%in%c("D10","D12","D4","D22","D26"))] ## 0 0 0 0 0
percentMissingDo[which(perStation%in%c("D10","D12","D4","D22","D26"))]  ## 0.05232558 0.05202312 0.04093567 0.01149425 0.01149425
percentMissingPheo[which(perStation%in%c("D10","D12","D4","D22","D26"))]##  0 0 0 0 0
percentMissingTemp[which(perStation%in%c("D10","D12","D4","D22","D26"))] ## 0.03488372 0.03468208 0.01754386 0.01149425 0.01149425
percentMissingSal[which(perStation%in%c("D10","D12","D4","D22","D26"))]## 0.04069767 0.04046243 0.01754386 0.01149425 0.01149425


## counts
percentMissingChl=percentMissingDo=percentMissingPheo=percentMissingTemp=percentMissingSal=c()
perStation=c("D10","D12","D4","D22","D26")
for(i in 1:length(perStation)){
  
  data=subset(sfei,date_dec>2000 &Station==perStation[i])
  percentMissingChl<-c(percentMissingChl,length(which(is.na(data$chl))))
  percentMissingDo<-c(percentMissingDo,length(which(is.na(data$do))))
  percentMissingPheo<-c(percentMissingPheo,length(which(is.na(data$pheo))))
  percentMissingTemp<-c(percentMissingTemp,length(which(is.na(data$temp))))
  percentMissingSal<-c(percentMissingSal,length(which(is.na(data$sal))))
  
  ## chl
  ## do
  ## pheo
  ## temp
  ## sal
  
  
}
percentMissingDo ##  9 9 2 7 2
percentMissingTemp ## 6 6 2 3 2
percentMissingSal # 7 7 2 3 2

## now we can probably "Hot deck"

## any back to back missing values?
## avg value before and after missing values

stationUse=c("D10","D12","D4","D22","D26")
for(i in 1:length(stationUse)){
  data=subset(sfei,date_dec>2000 &Station==stationUse[i])
  
  print(sum(diff(which(is.na(data$do)))==1))
  print(sum(diff(which(is.na(data$temp)))==1))
  print(sum(diff(which(is.na(data$sal)))==1))
  
  
  
}

## missingness same for all variables within a station?
## often but not always
stationUse=c("D10","D12","D4","D22","D26")
for(i in 1:length(stationUse)){
  data=subset(sfei,date_dec>2000 &Station==stationUse[i])
  
  print(intersect(which(is.na(data$do)),which(is.na(data$temp))))
  print(intersect(which(is.na(data$temp)),which(is.na(data$sal))))
  #print(sum(diff(which(is.na(data$do)))==1))
  #print(sum(diff(which(is.na(data$temp)))==1))
  #print(sum(diff(which(is.na(data$sal)))==1))
  
  
  
}
stationUse=c("D10","D12","D4","D22","D26")
dataBeforeImputation=subset(sfei,date_dec>=2000 & Station%in%stationUse)
nrow(dataBeforeImputation) ## 864

length(which(is.na(dataBeforeImputation$chl))) ## 0 
length(which(is.na(dataBeforeImputation$pheo))) ## 0
length(which(is.na(dataBeforeImputation$do))) ## 29
length(which(is.na(dataBeforeImputation$sal))) ## 21
length(which(is.na(dataBeforeImputation$temp))) ## 19

dataAfterImputation=dataBeforeImputation

## if one of before and after is na, just take value of other
toImputeDo=which(is.na(dataBeforeImputation$do))
for(i in 1:length(which(is.na(dataBeforeImputation$do)))){
  dataAfterImputation$do[toImputeDo[i]]=mean(c(dataAfterImputation$do[toImputeDo[i]-1],dataAfterImputation$do[toImputeDo[i]+1]),na.rm=T)
print(i)
  }

toImputeSal=which(is.na(dataBeforeImputation$sal))
for(i in 1:length(which(is.na(dataBeforeImputation$sal)))){
  dataAfterImputation$sal[toImputeSal[i]]=mean(c(dataAfterImputation$sal[toImputeSal[i]-1],dataAfterImputation$sal[toImputeSal[i]+1]),na.rm=T)
}

toImputeTemp=which(is.na(dataBeforeImputation$temp))
for(i in 1:length(which(is.na(dataBeforeImputation$temp)))){
  dataAfterImputation$temp[toImputeTemp[i]]=mean(c(dataAfterImputation$temp[toImputeTemp[i]-1],dataAfterImputation$temp[toImputeTemp[i]+1]),na.rm=T)
}

length(which(is.na(dataAfterImputation$do))) ## 0
length(which(is.na(dataAfterImputation$sal))) ## 0
length(which(is.na(dataAfterImputation$temp))) ## 0

toUse=dataAfterImputation[,c("Date","Station","Longitude","Latitude","date_dec","doy","chl","do","pheo","sal","temp")]
dim(toUse)

setwd("~/UC_Berkeley/Semester_4/timeSeries")
write.csv(toUse,"sfeiDataForProject.csv",row.names=F)

