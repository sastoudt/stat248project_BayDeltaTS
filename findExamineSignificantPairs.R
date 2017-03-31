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

sum(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 9

sum(p.adjust(withinStationSameVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 0

sum(p.adjust(withinStationDiffVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 2


