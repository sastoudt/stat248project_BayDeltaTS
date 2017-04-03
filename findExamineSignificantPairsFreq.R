## find and look at significance freq domain

setwd("~/UC_Berkeley/Semester_4/timeSeries")
acrossStationSameVarResults<-read.csv("acrossStationSameVarResultsFreq.csv",stringsAsFactors=F)
acrossStationDiffVarResults<-read.csv("acrossStationDiffVarResultsFreq.csv",stringsAsFactors=F)
withinStationSameVarResults<-read.csv("withinStationSameVarResultsFreq.csv",stringsAsFactors=F)
withinStationDiffVarResults<-read.csv("withinStationDiffVarResultsFreq.csv",stringsAsFactors=F)

names(acrossStationSameVarResults)=names(acrossStationDiffVarResults)=names(withinStationDiffVarResults)=c("maxPhase","maxCoh","maxFreq","pVal","minPval")
names(withinStationSameVarResults)=c("maxSpec","maxFreq","pVal","minPval")

## first check to make sure we have availability to get significant p-values

summary(acrossStationSameVarResults$minPval) ##0.005376   for all is the max min pval, so we are good
summary(acrossStationDiffVarResults$minPval)
summary(withinStationSameVarResults$minPval)
summary(withinStationDiffVarResults$minPval)

length(which(acrossStationSameVarResults$pVal<0.05)) ## 85
nrow(acrossStationSameVarResults) ## 100
length(which(acrossStationDiffVarResults$pVal<0.05)) ## 81
nrow(acrossStationDiffVarResults) ## 400
length(which(withinStationSameVarResults$pVal<0.05)) ## 1
nrow(withinStationSameVarResults) ## 25
length(which(withinStationDiffVarResults$pVal<0.05)) ## 29
nrow(withinStationDiffVarResults) ## 100

sum(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 74

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
## 20

sigToPlot=acrossStationDiffVar[which(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05),]
resToPlot=acrossStationDiffVarResults[which(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05),]
sigToPlot
resToPlot


sum(p.adjust(withinStationSameVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 0
#sigToPlot=withinStationSameVar[which(p.adjust(withinStationSameVarResults$pVal, method = "BY") <0.05),]
#resToPlot=withinStationSameVarResults[which(p.adjust(withinStationSameVarResults$pVal, method = "BY") <0.05),]
#sigToPlot
#resToPlot



sum(p.adjust(withinStationDiffVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 9

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

