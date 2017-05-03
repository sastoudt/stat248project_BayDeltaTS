## find and look at significance

setwd("~/UC_Berkeley/Semester_4/timeSeries")
acrossStationSameVarResults<-read.csv("acrossStationSameVarResults.csv",stringsAsFactors=F)
acrossStationDiffVarResults<-read.csv("acrossStationDiffVarResults.csv",stringsAsFactors=F)
withinStationSameVarResults<-read.csv("withinStationSameVarResults.csv",stringsAsFactors=F)
withinStationDiffVarResults<-read.csv("withinStationDiffVarResults.csv",stringsAsFactors=F)

names(acrossStationSameVarResults)=names(acrossStationDiffVarResults)=names(withinStationSameVarResults)=
names(withinStationDiffVarResults)=c("maxLag","pVal","minPval")

## first check to make sure we have availability to get significant p-values

summary(acrossStationSameVarResults$minPval) ## 0.005376  for all is the max min pval, so we are good
summary(acrossStationDiffVarResults$minPval)
summary(withinStationSameVarResults$minPval)
summary(withinStationDiffVarResults$minPval)

length(which(acrossStationSameVarResults$pVal<0.05)) ## 18
nrow(acrossStationSameVarResults) ## 100
length(which(acrossStationDiffVarResults$pVal<0.05)) ## 83
nrow(acrossStationDiffVarResults) ## 400
length(which(withinStationSameVarResults$pVal<0.05)) ## 2
nrow(withinStationSameVarResults) ## 25
length(which(withinStationDiffVarResults$pVal<0.05)) ## 23
nrow(withinStationDiffVarResults) ## 100

sum(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 3

sigToPlot=acrossStationSameVar[which(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05),]
resToPlot=acrossStationSameVarResults[which(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05),]

lonSub=c(D4$Longitude[1],D10$Longitude[1],D12$Longitude[1],D22$Longitude[1],D26$Longitude[1])
latSub=c(D4$Latitude[1], D10$Latitude[1], D12$Latitude[1],D22$Latitude[1],D26$Latitude[1])
stationNameSub=c("D4","D10","D12","D22","D26")
names(lonSub)=names(latSub)=stationNameSub

longitude=c(-121.2647, -121.5205, -121.9183, -121.8063, -121.6148, -121.7391, -121.5669, -121.5730, -121.8205,
            -122.3729, -122.1177,-122.0397, -121.9900, -121.4199, -121.3823)
latitude=c(37.67934, 38.36771, 38.04631, 38.02161, 38.04376, 38.08453, 38.07664, 37.97048, 38.06248, 38.03022,
           38.04436, 38.11714, 38.05992, 38.04226, 37.97817)
## FIGURE
plot(longitude,latitude,cex=4)
points(lonSub,latSub,col="red",cex=4)
text(lonSub,latSub,stationNameSub)


## FIGURE
sigToPlot$station1=as.character(sigToPlot$station1)
sigToPlot$station2=as.character(sigToPlot$station2)

## none for chl
 plot(longitude,latitude,main="Across Stations, Same Nutrient")
 points(lonSub,latSub,col="red",pch=19)
 arrows(lonSub[sigToPlot$station1],latSub[sigToPlot$station1],
        lonSub[sigToPlot$station2],latSub[sigToPlot$station2],length=0)
 
 text(-121.8,38.1,paste(sigToPlot$var1[1],resToPlot$maxLag[1],sep=" "),col="dodgerblue")
 text(-121.65,38.1,paste(sigToPlot$var1[2],resToPlot$maxLag[2],sep=" "),col="dodgerblue")
 text(-121.7,38.05,paste(sigToPlot$var1[3],resToPlot$maxLag[3],sep=" "),col="dodgerblue")
 
 # for(i in 1:nrow(sigToPlot)){
 #   text(lonSub[sigToPlot$station1][i]+par("cxy")[2]/2,
 #        latSub[sigToPlot$station1][i]+par("cxy")[2]/2,paste(sigToPlot$var1[i],resToPlot$maxLag[i],sep=" "),col="dodgerblue")
 # }

#text(lonSub,latSub,stationNameSub)

plot(longitude,latitude,main="do across stations")
points(lonSub,latSub,col="red",pch=19)
arrows(lonSub[sigToPlot$station1[which(sigToPlot$var1=="do")]],latSub[sigToPlot$station1[which(sigToPlot$var1=="do")]],
       lonSub[sigToPlot$station2[which(sigToPlot$var1=="do")]],latSub[sigToPlot$station2[which(sigToPlot$var1=="do")]],length=0)
for(i in 1:length(which(sigToPlot$var1=="do"))){
  
  if(i==1){
    text(lonSub[sigToPlot$station2[which(sigToPlot$var1=="do")]][i]-par("cxy")[2]/2,
         latSub[sigToPlot$station2[which(sigToPlot$var1=="do")]][i]-par("cxy")[2]/2,resToPlot$maxLag[which(sigToPlot$var1=="do")][i],col="dodgerblue")
  }else{
  text(lonSub[sigToPlot$station1[which(sigToPlot$var1=="do")]][i]+par("cxy")[2]/2,
       latSub[sigToPlot$station1[which(sigToPlot$var1=="do")]][i]+par("cxy")[2]/2,resToPlot$maxLag[which(sigToPlot$var1=="do")][i],col="dodgerblue")
}
  }
text(lonSub,latSub,stationNameSub)

## No pheo
# plot(longitude,latitude,main="pheo across stations")
# points(lonSub,latSub,col="red",pch=19)
# arrows(lonSub[sigToPlot$station1[which(sigToPlot$var1=="pheo")]],latSub[sigToPlot$station1[which(sigToPlot$var1=="pheo")]],
#        lonSub[sigToPlot$station2[which(sigToPlot$var1=="pheo")]],latSub[sigToPlot$station2[which(sigToPlot$var1=="pheo")]],length=0)
# for(i in 1:length(which(sigToPlot$var1=="pheo"))){
#   
#   if(i==1){
#     text(lonSub[sigToPlot$station2[which(sigToPlot$var1=="pheo")]][i]-par("cxy")[2]/2,
#          latSub[sigToPlot$station2[which(sigToPlot$var1=="pheo")]][i]-par("cxy")[2]/2,resToPlot$maxLag[which(sigToPlot$var1=="pheo")][i],col="dodgerblue")
#   }else{
#     text(lonSub[sigToPlot$station1[which(sigToPlot$var1=="pheo")]][i]+par("cxy")[2]/2,
#          latSub[sigToPlot$station1[which(sigToPlot$var1=="pheo")]][i]+par("cxy")[2]/2,resToPlot$maxLag[which(sigToPlot$var1=="pheo")][i],col="dodgerblue")
#   }
# }
# text(lonSub,latSub,stationNameSub)

plot(longitude,latitude,main="sal across stations")
points(lonSub,latSub,col="red",pch=19)
arrows(lonSub[sigToPlot$station1[which(sigToPlot$var1=="sal")]],latSub[sigToPlot$station1[which(sigToPlot$var1=="sal")]],
       lonSub[sigToPlot$station2[which(sigToPlot$var1=="sal")]],latSub[sigToPlot$station2[which(sigToPlot$var1=="sal")]],length=0)
for(i in 1:length(which(sigToPlot$var1=="sal"))){
#for(i in 1:4) { 
  
    text(lonSub[sigToPlot$station1[which(sigToPlot$var1=="sal")]][i]+par("cxy")[2]/2,
         latSub[sigToPlot$station1[which(sigToPlot$var1=="sal")]][i]+par("cxy")[2]/2,resToPlot$maxLag[which(sigToPlot$var1=="sal")][i],col="dodgerblue")
  
}

text(lonSub,latSub,stationNameSub)

plot(longitude,latitude,main="temp across stations")
points(lonSub,latSub,col="red",pch=19)
arrows(lonSub[sigToPlot$station1[which(sigToPlot$var1=="temp")]],latSub[sigToPlot$station1[which(sigToPlot$var1=="temp")]],
       lonSub[sigToPlot$station2[which(sigToPlot$var1=="temp")]],latSub[sigToPlot$station2[which(sigToPlot$var1=="temp")]],length=0)
for(i in 1:length(which(sigToPlot$var1=="temp"))){
  #for(i in 1:4) { 
  
    text(lonSub[sigToPlot$station1[which(sigToPlot$var1=="temp")]][i]+par("cxy")[2]/2,
         latSub[sigToPlot$station1[which(sigToPlot$var1=="temp")]][i]+par("cxy")[2]/2,resToPlot$maxLag[which(sigToPlot$var1=="temp")][i],col="dodgerblue")
  #}
}

text(jitter(lonSub,10),jitter(latSub,10),stationNameSub)





sum(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 8

sigToPlot=acrossStationDiffVar[which(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05),]
resToPlot=acrossStationDiffVarResults[which(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05),]

#440       D4      D22 pheo  sal
#449      D26       D4 pheo  sal

test=ccf(D4[,"residGAMpheoTransform"],D22[,"residGAMsal"],lag.max=12,plot=T)
test=ccf(D26[,"residGAMpheoTransform"],D4[,"residGAMsal"],lag.max=12,plot=T)
## check for sign


sigToPlot

sigToPlot$station1=as.character(sigToPlot$station1)
sigToPlot$station2=as.character(sigToPlot$station2)

## FIGURE
plot(longitude,latitude,main="Across Stations, Different Nutrients",xlim=c(-122.2,-121.4),ylim=c(37.95,38.12))
points(lonSub,latSub,col="red",pch=19)
arrows(lonSub[sigToPlot$station1],latSub[sigToPlot$station1],
     lonSub[sigToPlot$station2],latSub[sigToPlot$station2],length=0)
text(jitter(lonSub,10),jitter(latSub,10),stationNameSub)
toPrint=cbind(sigToPlot,resToPlot$maxLag)
row.names(toPrint)=NULL

names(toPrint)=c("Station 1", "Station 2", "Nutrient 1", "Nutrient 2", "Lag Max")
toPrint

library(gridExtra)
pdf("acrossStationsDiffNutTab.pdf", height=6, width=6)
grid.table(toPrint)
dev.off()


## D4 D22 twice
text(lonSub,latSub,stationNameSub)
arrows(lonSub['D4'],latSub["D4"]+.01,lonSub["D22"],latSub["D22"]+.01,length=0)



## How to visualize this effectively
## col: nutrient 1
## text: nutrient 2 +lag
## but so much overlap

## maybe just do table with map for reference


sum(p.adjust(withinStationSameVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 0

sum(p.adjust(withinStationDiffVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 1

sigToPlot=withinStationDiffVar[which(p.adjust(withinStationDiffVarResults$pVal, method = "BY") <0.05),]
resToPlot=withinStationDiffVarResults[which(p.adjust(withinStationDiffVarResults$pVal, method = "BY") <0.05),]

sigToPlot
resToPlot

## table

plot(D12$residGAMdo,type="l")
lines(D12$residGAMtemp,col="red")

## FIGURE
par(mfrow=c(3,1))
plot(D12$residGAMdo,D12$residGAMtemp,xlab="resid do",ylab="resid temp",main="D12")
test=ccf(D12$residGAMdo,D12$residGAMtemp,lag.max=12,plot=T,main="")

station1Data=station2Data=D12
varName="do"
varName2="temp"
empP<-c()
for(i in 2:nrow(station2Data) ){
  test1=station2Data[c((i):nrow(station2Data),1:(i-1)),varName2]
  
  test=ccf(station1Data[,varName],test1,lag.max=12,plot=F)
  ccfOfInterest=test$acf[14:25]
  empP<-c( empP,ccfOfInterest[which.max(abs(ccfOfInterest))] )
  #  print(i)
}
test=ccf(station1Data[,varName],station2Data[,varName2],lag.max=12,plot=F)
ccfOfInterest=test$acf[14:25]
hist(empP,xlab="Max Absolute Cross Correlation",main="Empirical Distribution from Rotation Test")
abline(v=ccfOfInterest[which.max(ccfOfInterest)],col="red")
