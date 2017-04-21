## find and look at significance freq domain

setwd("~/UC_Berkeley/Semester_4/timeSeries")
acrossStationSameVarResults<-read.csv("acrossStationSameVarResultsFreq.csv",stringsAsFactors=F)
acrossStationDiffVarResults<-read.csv("acrossStationDiffVarResultsFreq.csv",stringsAsFactors=F)
#withinStationSameVarResults<-read.csv("withinStationSameVarResultsFreq.csv",stringsAsFactors=F)
withinStationDiffVarResults<-read.csv("withinStationDiffVarResultsFreq.csv",stringsAsFactors=F)

names(acrossStationSameVarResults)=names(acrossStationDiffVarResults)=names(withinStationDiffVarResults)=c("maxPhase","maxCoh","maxFreq","pVal")
#names(withinStationSameVarResults)=c("maxSpec","maxFreq","pVal","minPval")

## first check to make sure we have availability to get significant p-values

#summary(acrossStationSameVarResults$minPval) ##0.005376   for all is the max min pval, so we are good
#summary(acrossStationDiffVarResults$minPval)
#summary(withinStationSameVarResults$minPval)
#summary(withinStationDiffVarResults$minPval)

length(which(acrossStationSameVarResults$pVal<0.05)) ## 100 pw 100
nrow(acrossStationSameVarResults) ## 100
hist(acrossStationSameVarResults$pVal)
length(which(acrossStationDiffVarResults$pVal<0.05)) ## 266 pw 262
nrow(acrossStationDiffVarResults) ## 400
hist(acrossStationDiffVarResults$pVal)
#length(which(withinStationSameVarResults$pVal<0.05)) ## 1
#nrow(withinStationSameVarResults) ## 25
length(which(withinStationDiffVarResults$pVal<0.05)) ## 64 pw 64
nrow(withinStationDiffVarResults) ## 100
hist(withinStationDiffVarResults$pVal)

sum(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 90 pw 92

sigToPlot=acrossStationSameVar[which(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05),]
resToPlot=acrossStationSameVarResults[which(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05),]
sigToPlot
resToPlot

lonSub=c(D4$Longitude[1],D10$Longitude[1],D12$Longitude[1],D22$Longitude[1],D26$Longitude[1])
latSub=c(D4$Latitude[1], D10$Latitude[1], D12$Latitude[1],D22$Latitude[1],D26$Latitude[1])
stationNameSub=c("D4","D10","D12","D22","D26")
names(lonSub)=names(latSub)=stationNameSub

longitude=c(-121.2647, -121.5205, -121.9183, -121.8063, -121.6148, -121.7391, -121.5669, -121.5730, -121.8205,
            -122.3729, -122.1177,-122.0397, -121.9900, -121.4199, -121.3823)
latitude=c(37.67934, 38.36771, 38.04631, 38.02161, 38.04376, 38.08453, 38.07664, 37.97048, 38.06248, 38.03022,
           38.04436, 38.11714, 38.05992, 38.04226, 37.97817)

sum(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 72

sigToPlot=acrossStationDiffVar[which(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05),]
resToPlot=acrossStationDiffVarResults[which(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05),]
sigToPlot
resToPlot


#sum(p.adjust(withinStationSameVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 0
#sigToPlot=withinStationSameVar[which(p.adjust(withinStationSameVarResults$pVal, method = "BY") <0.05),]
#resToPlot=withinStationSameVarResults[which(p.adjust(withinStationSameVarResults$pVal, method = "BY") <0.05),]
#sigToPlot
#resToPlot



sum(p.adjust(withinStationDiffVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 26 pw 24

sigToPlot=withinStationDiffVar[which(p.adjust(withinStationDiffVarResults$pVal, method = "BY") <0.05),]
resToPlot=withinStationDiffVarResults[which(p.adjust(withinStationDiffVarResults$pVal, method = "BY") <0.05),]
sigToPlot
resToPlot


###
hist(withinStationDiffVarResults$pVal) ## more zeros than anything
hist(withinStationSameVarResults$pVal) ## more big values
hist(acrossStationDiffVarResults$pVal) ## more zeroes
hist(acrossStationSameVarResults$pVal) ## more zeros than anything


## maybe rotation test doesn't work in frequency domain
## rotate in time, then convert to frequency

names(withinStationDiffVarResults)

summary(withinStationDiffVarResults$maxFreq) ## median 5.35
summary(acrossStationSameVarResults$maxFreq) ## median 10.98
summary(acrossStationDiffVarResults$maxFreq) ## median 4.8 but larger mean and

par(mfrow=c(1,3))
boxplot(withinStationDiffVarResults$maxFreq,main="within station diff var",ylab="freq")
boxplot(acrossStationSameVarResults$maxFreq,main="across station same var",ylab="freq")
boxplot(acrossStationDiffVarResults$maxFreq,main="across station diff var",ylab="freq")
## across station diff var is surprising
## within station diff var makes sense
## why is same var across station so much more dispersed?

boxplot(withinStationDiffVarResults$maxPhase,main="within station diff var",ylab="phase")
boxplot(acrossStationSameVarResults$maxPhase,main="across station same var",ylab="phase")
boxplot(acrossStationDiffVarResults$maxPhase,main="across station diff var",ylab="phase")
## what are the units again?

## try prewhitening before doing this to see if it limits the significant pairs
## same general results for prewhitened versions
