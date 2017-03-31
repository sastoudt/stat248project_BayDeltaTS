## find and look at significance

setwd("~/UC_Berkeley/Semester_4/timeSeries")
acrossStationSameVarResults<-read.csv("acrossStationSameVarResults.csv",stringsAsFactors=F)
acrossStationDiffVarResults<-read.csv("acrossStationDiffVarResults.csv",stringsAsFactors=F)
withinStationSameVarResults<-read.csv("withinStationSameVarResults.csv",stringsAsFactors=F)
withinStationDiffVarResults<-read.csv("withinStationDiffVarResults.csv",stringsAsFactors=F)

names(acrossStationSameVarResults)=names(acrossStationDiffVarResults)=names(withinStationSameVarResults)=
names(withinStationDiffVarResults)=c("maxLag","pVal","minPval")

## first check to make sure we have availability to get significant p-values

summary(acrossStationSameVarResults$minPval) ## 0.005848 for all is the max min pval, so we are good
summary(acrossStationDiffVarResults$minPval)
summary(withinStationSameVarResults$minPval)
summary(withinStationDiffVarResults$minPval)

length(which(acrossStationSameVarResults$pVal<0.05)) ## 40
nrow(acrossStationSameVarResults) ## 100
length(which(acrossStationDiffVarResults$pVal<0.05)) ## 80
nrow(acrossStationDiffVarResults) ## 400
length(which(withinStationSameVarResults$pVal<0.05)) ## 7
nrow(withinStationSameVarResults) ## 25
length(which(withinStationDiffVarResults$pVal<0.05)) ## 10
nrow(withinStationDiffVarResults) ## 100

sum(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 17

sigToPlot=acrossStationSameVar[which(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05),]
resToPlot=acrossStationSameVarResults[which(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05),]

lonSub=c(D4$Longitude[1],D10$Longitude[1],D12$Longitude[1],D22$Longitude[1],D26$Longitude[1])
latSub=c(D4$Latitude[1], D10$Latitude[1], D12$Latitude[1],D22$Latitude[1],D26$Latitude[1])
names(lonSub)=names(latSub)=stationNameSub
stationNameSub=c("D4","D10","D12","D22","D26")

longitude=c(-121.2647, -121.5205, -121.9183, -121.8063, -121.6148, -121.7391, -121.5669, -121.5730, -121.8205,
            -122.3729, -122.1177,-122.0397, -121.9900, -121.4199, -121.3823)
latitude=c(37.67934, 38.36771, 38.04631, 38.02161, 38.04376, 38.08453, 38.07664, 37.97048, 38.06248, 38.03022,
           38.04436, 38.11714, 38.05992, 38.04226, 37.97817)
plot(longitude,latitude)
points(lonSub,latSub,col="red")
text(lonSub,latSub,stationNameSub)

sigToPlot$station1=as.character(sigToPlot$station1)
sigToPlot$station2=as.character(sigToPlot$station2)


plot(longitude,latitude,main="chl across stations")
points(lonSub,latSub,col="red",pch=19)
arrows(lonSub[sigToPlot$station1[which(sigToPlot$var1=="chl")]],latSub[sigToPlot$station1[which(sigToPlot$var1=="chl")]],
       lonSub[sigToPlot$station2[which(sigToPlot$var1=="chl")]],latSub[sigToPlot$station2[which(sigToPlot$var1=="chl")]],length=0)
for(i in 1:length(which(sigToPlot$var1=="chl"))){
  text(lonSub[sigToPlot$station1[which(sigToPlot$var1=="chl")]][i]+par("cxy")[2]/2,
       latSub[sigToPlot$station1[which(sigToPlot$var1=="chl")]][i]+par("cxy")[2]/2,resToPlot$maxLag[which(sigToPlot$var1=="chl")][i],col="dodgerblue")
}

text(lonSub,latSub,stationNameSub)

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


sum(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 9

sum(p.adjust(withinStationSameVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 0

sum(p.adjust(withinStationDiffVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 2


