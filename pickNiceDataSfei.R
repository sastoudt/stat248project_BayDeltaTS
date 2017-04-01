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

# dataAfterImputation=dataBeforeImputation
# 
# ## this doesn't even make sense, not within station etc.
# 
# ## if one of before and after is na, just take value of other
# toImputeDo=which(is.na(dataBeforeImputation$do))
# for(i in 1:length(which(is.na(dataBeforeImputation$do)))){
#   dataAfterImputation$do[toImputeDo[i]]=mean(c(dataAfterImputation$do[toImputeDo[i]-1],dataAfterImputation$do[toImputeDo[i]+1]),na.rm=T)
# print(i)
#   }
# 
# toImputeSal=which(is.na(dataBeforeImputation$sal))
# for(i in 1:length(which(is.na(dataBeforeImputation$sal)))){
#   dataAfterImputation$sal[toImputeSal[i]]=mean(c(dataAfterImputation$sal[toImputeSal[i]-1],dataAfterImputation$sal[toImputeSal[i]+1]),na.rm=T)
# }
# 
# toImputeTemp=which(is.na(dataBeforeImputation$temp))
# for(i in 1:length(which(is.na(dataBeforeImputation$temp)))){
#   dataAfterImputation$temp[toImputeTemp[i]]=mean(c(dataAfterImputation$temp[toImputeTemp[i]-1],dataAfterImputation$temp[toImputeTemp[i]+1]),na.rm=T)
# }
# 
# length(which(is.na(dataAfterImputation$do))) ## 0
# length(which(is.na(dataAfterImputation$sal))) ## 0
# length(which(is.na(dataAfterImputation$temp))) ## 0
# 
# toUse=dataAfterImputation[,c("Date","Station","Longitude","Latitude","date_dec","doy","chl","do","pheo","sal","temp")]
# dim(toUse)
# 
# setwd("~/UC_Berkeley/Semester_4/timeSeries")
# write.csv(toUse,"sfeiDataForProject.csv",row.names=F)

#sfei<-read.csv("sfeiDataForProject.csv",stringsAsFactors=F)
## read original and reimpute
sfei=dataBeforeImputation
sfei$yr=as.numeric(as.character(format(as.Date(sfei$Date), "%Y")))
sfei$mon=as.numeric(as.character(format(as.Date(sfei$Date),"%m")))

sfei=sfei[,c("Date","Station","Longitude","Latitude","chl","do","pheo","sal","temp","date_dec","doy","yr","mon")]

yrSeq=seq(2000,2015,by=1)
monSeq=seq(1,12,by=1)

dataTimeFull=as.data.frame(expand.grid(yrSeq,monSeq))
names(dataTimeFull)=c("yr","mon")

D10<-subset(sfei,Station=="D10")
D10f=merge(dataTimeFull,D10,by.x=c("yr","mon"),by.y=c("yr","mon"),all.x=T)

D12<-subset(sfei,Station=="D12")
D12f=merge(dataTimeFull,D12,by.x=c("yr","mon"),by.y=c("yr","mon"),all.x=T)

D22<-subset(sfei,Station=="D22")
D22f=merge(dataTimeFull,D22,by.x=c("yr","mon"),by.y=c("yr","mon"),all.x=T)

D26<-subset(sfei,Station=="D26")
D26f=merge(dataTimeFull,D26,by.x=c("yr","mon"),by.y=c("yr","mon"),all.x=T)

D4<-subset(sfei,Station=="D4")
D4f=merge(dataTimeFull,D4,by.x=c("yr","mon"),by.y=c("yr","mon"),all.x=T)

## now need to fill in NAs

D10f[which(is.na(D10f$Date)),"Date"]=paste(D10f[which(is.na(D10f$Date)),"yr"],D10f[which(is.na(D10f$Date)),"mon"],
                                                  "01",sep="-" )

D10f$Date=as.Date(D10f$Date)
D10f[which(is.na(D10f$Station)),"Station"]="D10"
D10f[which(is.na(D10f$Longitude)),"Longitude"]=D10f$Longitude[1]
D10f[which(is.na(D10f$Latitude)),"Latitude"]=D10f$Latitude[1]
require(lubridate)
require(WRTDStidal)
D10f[which(is.na(D10f$date_dec)),"date_dec"]=as.numeric(dec_time(D10f[which(is.na(D10f$date_dec)),"Date"])$dec_time)
D10f[which(is.na(D10f$doy)),"doy"]=as.numeric(strftime(D10f[which(is.na(D10f$doy)),"Date"], format = "%j"))


D12f[which(is.na(D12f$Date)),"Date"]=paste(D12f[which(is.na(D12f$Date)),"yr"],D12f[which(is.na(D12f$Date)),"mon"],
                                           "01",sep="-" )

D12f$Date=as.Date(D12f$Date)
D12f[which(is.na(D12f$Station)),"Station"]="D12"
D12f[which(is.na(D12f$Longitude)),"Longitude"]=D12f$Longitude[1]
D12f[which(is.na(D12f$Latitude)),"Latitude"]=D12f$Latitude[1]
D12f[which(is.na(D12f$date_dec)),"date_dec"]=as.numeric(dec_time(D12f[which(is.na(D12f$date_dec)),"Date"])$dec_time)
D12f[which(is.na(D12f$doy)),"doy"]=as.numeric(strftime(D12f[which(is.na(D12f$doy)),"Date"], format = "%j"))


D22f[which(is.na(D22f$Date)),"Date"]=paste(D22f[which(is.na(D22f$Date)),"yr"],D22f[which(is.na(D22f$Date)),"mon"],
                                           "01",sep="-" )

D22f$Date=as.Date(D22f$Date)
D22f[which(is.na(D22f$Station)),"Station"]="D22"
D22f[which(is.na(D22f$Longitude)),"Longitude"]=D22f$Longitude[1]
D22f[which(is.na(D22f$Latitude)),"Latitude"]=D22f$Latitude[1]
D22f[which(is.na(D22f$date_dec)),"date_dec"]=as.numeric(dec_time(D22f[which(is.na(D22f$date_dec)),"Date"])$dec_time)
D22f[which(is.na(D22f$doy)),"doy"]=as.numeric(strftime(D22f[which(is.na(D22f$doy)),"Date"], format = "%j"))


D26f[which(is.na(D26f$Date)),"Date"]=paste(D26f[which(is.na(D26f$Date)),"yr"],D26f[which(is.na(D26f$Date)),"mon"],
                                           "01",sep="-" )

D26f$Date=as.Date(D26f$Date)
D26f[which(is.na(D26f$Station)),"Station"]="D26"
D26f[which(is.na(D26f$Longitude)),"Longitude"]=D26f$Longitude[1]
D26f[which(is.na(D26f$Latitude)),"Latitude"]=D26f$Latitude[1]
D26f[which(is.na(D26f$date_dec)),"date_dec"]=as.numeric(dec_time(D26f[which(is.na(D26f$date_dec)),"Date"])$dec_time)
D26f[which(is.na(D26f$doy)),"doy"]=as.numeric(strftime(D26f[which(is.na(D26f$doy)),"Date"], format = "%j"))


D4f[which(is.na(D4f$Date)),"Date"]=paste(D4f[which(is.na(D4f$Date)),"yr"],D4f[which(is.na(D4f$Date)),"mon"],
                                           "01",sep="-" )

D4f$Date=as.Date(D4f$Date)
D4f[which(is.na(D4f$Station)),"Station"]="D4"
D4f[which(is.na(D4f$Longitude)),"Longitude"]=D4f$Longitude[1]
D4f[which(is.na(D4f$Latitude)),"Latitude"]=D4f$Latitude[1]
D4f[which(is.na(D4f$date_dec)),"date_dec"]=as.numeric(dec_time(D4f[which(is.na(D4f$date_dec)),"Date"])$dec_time)
D4f[which(is.na(D4f$doy)),"doy"]=as.numeric(strftime(D4f[which(is.na(D4f$doy)),"Date"], format = "%j"))



### now impute actual variables


#D10f[which(is.na(D10f$chl)),"chl"]

toImputeChl=which(is.na(D10f$chl))
toImputeDo=which(is.na(D10f$do))
toImputePheo=which(is.na(D10f$pheo))
toImputeSal=which(is.na(D10f$sal))
toImputeTemp=which(is.na(D10f$temp))

for(i in 1:length(toImputeChl)){
  D10f$chl[toImputeChl[i]]=mean(c(D10f$chl[toImputeChl[i]-1],D10f$chl[toImputeChl[i]+1]),na.rm=T)
}
sum(is.na(D10f$chl))

for(i in 1:length(toImputeDo)){
  D10f$do[toImputeDo[i]]=mean(c(D10f$do[toImputeDo[i]-1],D10f$do[toImputeDo[i]+1]),na.rm=T)
}
sum(is.na(D10f$do))

for(i in 1:length(toImputePheo)){
  D10f$pheo[toImputePheo[i]]=mean(c(D10f$pheo[toImputePheo[i]-1],D10f$pheo[toImputePheo[i]+1]),na.rm=T)
}
sum(is.na(D10f$pheo))

for(i in 1:length(toImputeSal)){
  D10f$sal[toImputeSal[i]]=mean(c(D10f$sal[toImputeSal[i]-1],D10f$sal[toImputeSal[i]+1]),na.rm=T)
}
sum(is.na(D10f$sal))

for(i in 1:length(toImputeTemp)){
  D10f$temp[toImputeTemp[i]]=mean(c(D10f$temp[toImputeTemp[i]-1],D10f$temp[toImputeTemp[i]+1]),na.rm=T)
}
sum(is.na(D10f$temp))

###
toImputeChl=which(is.na(D12f$chl))
toImputeDo=which(is.na(D12f$do))
toImputePheo=which(is.na(D12f$pheo))
toImputeSal=which(is.na(D12f$sal))
toImputeTemp=which(is.na(D12f$temp))

for(i in 1:length(toImputeChl)){
  D12f$chl[toImputeChl[i]]=mean(c(D12f$chl[toImputeChl[i]-1],D12f$chl[toImputeChl[i]+1]),na.rm=T)
}
sum(is.na(D12f$chl))

for(i in 1:length(toImputeDo)){
  D12f$do[toImputeDo[i]]=mean(c(D12f$do[toImputeDo[i]-1],D12f$do[toImputeDo[i]+1]),na.rm=T)
}
sum(is.na(D12f$do))

for(i in 1:length(toImputePheo)){
  D12f$pheo[toImputePheo[i]]=mean(c(D12f$pheo[toImputePheo[i]-1],D12f$pheo[toImputePheo[i]+1]),na.rm=T)
}
sum(is.na(D12f$pheo))

for(i in 1:length(toImputeSal)){
  D12f$sal[toImputeSal[i]]=mean(c(D12f$sal[toImputeSal[i]-1],D12f$sal[toImputeSal[i]+1]),na.rm=T)
}
sum(is.na(D12f$sal))

for(i in 1:length(toImputeTemp)){
  D12f$temp[toImputeTemp[i]]=mean(c(D12f$temp[toImputeTemp[i]-1],D12f$temp[toImputeTemp[i]+1]),na.rm=T)
}
sum(is.na(D12f$temp))

###

toImputeChl=which(is.na(D22f$chl))
toImputeDo=which(is.na(D22f$do))
toImputePheo=which(is.na(D22f$pheo))
toImputeSal=which(is.na(D22f$sal))
toImputeTemp=which(is.na(D22f$temp))

for(i in 1:length(toImputeChl)){
  D22f$chl[toImputeChl[i]]=mean(c(D22f$chl[toImputeChl[i]-1],D22f$chl[toImputeChl[i]+1]),na.rm=T)
}
sum(is.na(D22f$chl))

for(i in 1:length(toImputeDo)){
  D22f$do[toImputeDo[i]]=mean(c(D22f$do[toImputeDo[i]-1],D22f$do[toImputeDo[i]+1]),na.rm=T)
}
sum(is.na(D22f$do))

for(i in 1:length(toImputePheo)){
  D22f$pheo[toImputePheo[i]]=mean(c(D22f$pheo[toImputePheo[i]-1],D22f$pheo[toImputePheo[i]+1]),na.rm=T)
}
sum(is.na(D22f$pheo))

for(i in 1:length(toImputeSal)){
  D22f$sal[toImputeSal[i]]=mean(c(D22f$sal[toImputeSal[i]-1],D22f$sal[toImputeSal[i]+1]),na.rm=T)
}
sum(is.na(D22f$sal))

for(i in 1:length(toImputeTemp)){
  D22f$temp[toImputeTemp[i]]=mean(c(D22f$temp[toImputeTemp[i]-1],D22f$temp[toImputeTemp[i]+1]),na.rm=T)
}
sum(is.na(D22f$temp))

###
toImputeChl=which(is.na(D26f$chl))
toImputeDo=which(is.na(D26f$do))
toImputePheo=which(is.na(D26f$pheo))
toImputeSal=which(is.na(D26f$sal))
toImputeTemp=which(is.na(D26f$temp))

for(i in 1:length(toImputeChl)){
  D26f$chl[toImputeChl[i]]=mean(c(D26f$chl[toImputeChl[i]-1],D26f$chl[toImputeChl[i]+1]),na.rm=T)
}
sum(is.na(D26f$chl))

for(i in 1:length(toImputeDo)){
  D26f$do[toImputeDo[i]]=mean(c(D26f$do[toImputeDo[i]-1],D26f$do[toImputeDo[i]+1]),na.rm=T)
}
sum(is.na(D26f$do))

for(i in 1:length(toImputePheo)){
  D26f$pheo[toImputePheo[i]]=mean(c(D26f$pheo[toImputePheo[i]-1],D26f$pheo[toImputePheo[i]+1]),na.rm=T)
}
sum(is.na(D26f$pheo))

for(i in 1:length(toImputeSal)){
  D26f$sal[toImputeSal[i]]=mean(c(D26f$sal[toImputeSal[i]-1],D26f$sal[toImputeSal[i]+1]),na.rm=T)
}
sum(is.na(D26f$sal))

for(i in 1:length(toImputeTemp)){
  D26f$temp[toImputeTemp[i]]=mean(c(D26f$temp[toImputeTemp[i]-1],D26f$temp[toImputeTemp[i]+1]),na.rm=T)
}
sum(is.na(D26f$temp))

###

toImputeChl=which(is.na(D4f$chl))
toImputeDo=which(is.na(D4f$do))
toImputePheo=which(is.na(D4f$pheo))
toImputeSal=which(is.na(D4f$sal))
toImputeTemp=which(is.na(D4f$temp))

for(i in 1:length(toImputeChl)){
  D4f$chl[toImputeChl[i]]=mean(c(D4f$chl[toImputeChl[i]-1],D4f$chl[toImputeChl[i]+1]),na.rm=T)
}
sum(is.na(D4f$chl))

for(i in 1:length(toImputeDo)){
  D4f$do[toImputeDo[i]]=mean(c(D4f$do[toImputeDo[i]-1],D4f$do[toImputeDo[i]+1]),na.rm=T)
}
sum(is.na(D4f$do))

for(i in 1:length(toImputePheo)){
  D4f$pheo[toImputePheo[i]]=mean(c(D4f$pheo[toImputePheo[i]-1],D4f$pheo[toImputePheo[i]+1]),na.rm=T)
}
sum(is.na(D4f$pheo))

for(i in 1:length(toImputeSal)){
  D4f$sal[toImputeSal[i]]=mean(c(D4f$sal[toImputeSal[i]-1],D4f$sal[toImputeSal[i]+1]),na.rm=T)
}
sum(is.na(D4f$sal))

for(i in 1:length(toImputeTemp)){
  D4f$temp[toImputeTemp[i]]=mean(c(D4f$temp[toImputeTemp[i]-1],D4f$temp[toImputeTemp[i]+1]),na.rm=T)
}
sum(is.na(D4f$temp))

###
tail(D10f,20)
## need to trim july on in 2015

D10f=D10f[1:which(D10f$yr=="2015" & D10f$mon=="6"),]
D12f=D12f[1:which(D12f$yr=="2015" & D12f$mon=="6"),]
D22f=D22f[1:which(D22f$yr=="2015" & D22f$mon=="6"),]
D26f=D26f[1:which(D26f$yr=="2015" & D26f$mon=="6"),]
D4f=D4f[1:which(D4f$yr=="2015" & D4f$mon=="6"),]

setwd("~/UC_Berkeley/Semester_4/timeSeries")
afterImputation=rbind(D10f,D12f,D22f,D26f,D4f)
write.csv(afterImputation,"sfeiDataForProject.csv",row.names=F)
