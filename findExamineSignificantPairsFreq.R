## find and look at significance freq domain

setwd("~/UC_Berkeley/Semester_4/timeSeries")
acrossStationSameVarResults<-read.csv("acrossStationSameVarResultsFreq.csv",stringsAsFactors=F)
acrossStationDiffVarResults<-read.csv("acrossStationDiffVarResultsFreq.csv",stringsAsFactors=F)
withinStationSameVarResults<-read.csv("withinStationSameVarResultsFreq.csv",stringsAsFactors=F)
withinStationDiffVarResults<-read.csv("withinStationDiffVarResultsFreq.csv",stringsAsFactors=F)

names(acrossStationSameVarResults)=names(acrossStationDiffVarResults)=names(withinStationDiffVarResults)=c("maxPhase","maxCoh","maxFreq","pVal")
#names(withinStationSameVarResults)=c("maxSpec","maxFreq","pVal","minPval")
names(withinStationSameVarResults)=c("maxSpec","maxFreq","pVal")

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
length(which(withinStationSameVarResults$pVal<0.05)) ## 1
nrow(withinStationSameVarResults) ## 25
hist(withinStationSameVarResults$pVal)
length(which(withinStationDiffVarResults$pVal<0.05)) ## 64 pw 64
nrow(withinStationDiffVarResults) ## 100
hist(withinStationDiffVarResults$pVal)

sum(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05)## corrected p-val
## 90 pw 92
acrossStationSameVar[which(!p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05),]


acrossStationSameVarResults[which(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05),]
## FIGURE
par(mfrow=c(2,1))
hist(acrossStationSameVarResults[which(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05),"maxPhase"],
     main="Distribution of Phase for Significant Pairs",xlab="phase for maximum cross coherence")
hist(acrossStationSameVarResults[which(p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05),"maxFreq"],
     main="Distribution of Frequency for Significant Pairs",xlab="frequency for maximum cross coherence",breaks=seq(0,200,by=1))
abline(v=12,col="red")
toPrint=acrossStationSameVar[which(!p.adjust(acrossStationSameVarResults$pVal, method = "BY") <0.05),]
row.names(toPrint)=NULL

names(toPrint)=c("Station 1", "Station 2", "Nutrient 1", "Nutrient 2")

library(gridExtra)
pdf("acrossStationsSameNutFREQTab.pdf", height=6, width=6)
grid.table(toPrint)
dev.off()


toPrint=acrossStationSameVar[which(p.adjust(acrossStationSameVarResults$pVal, 
                                            method = "BY") <0.05),][which(acrossStationSameVarResults[which(p.adjust(acrossStationSameVarResults$pVal, 
                                                                                                                     method = "BY") <0.05),"maxFreq"]<12),]
row.names(toPrint)=NULL

names(toPrint)=c("Station 1", "Station 2", "Nutrient 1", "Nutrient 2")

## FIGURE for appendix
library(gridExtra)
pdf("acrossStationsSameNutFREQSmallFreqTab.pdf", height=13, width=6)
grid.table(toPrint)
dev.off()

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
## 60

par(mfrow=c(2,1))
hist(acrossStationDiffVarResults[which(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05),"maxPhase"],
     main="Distribution of Phase for Significant Pairs",xlab="phase for maximum cross coherence")
hist(acrossStationDiffVarResults[which(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05),"maxFreq"],
     main="Distribution of Frequency for Significant Pairs",xlab="frequency for maximum cross coherence",breaks=seq(0,200,by=1))
abline(v=12,col="red")
toPrint=acrossStationDiffVar[which(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05),]

toPrint[order(toPrint$station1),]

row.names(toPrint)=NULL

#names(toPrint)=c("Station 1", "Station 2", "Nutrient 1", "Nutrient 2")

toPrint$nutnut=paste(toPrint$var1,toPrint$var2,sep="_")
table(toPrint$nutnut)

tt=table(toPrint$nutnut)[order(table(toPrint$nutnut),decreasing=T)]
toPrint=as.data.frame(cbind(row.names(tt),unname(tt)))
names(toPrint)=c("Nutrient Combination","Number of Significant Pairs")
row.names(toPrint)=NULL

library(gridExtra)
pdf("acrossStationsDiffNutComboTab.pdf", height=8, width=6)
grid.table(toPrint)
dev.off()

toPrint=acrossStationDiffVar[which(p.adjust(acrossStationDiffVarResults$pVal, method = "BY") <0.05),]
toPrint[order(toPrint[,1],toPrint[,2]),]
#D4      D22 temp   do
#D10      D22 temp   do

toPrint$statstat=paste(toPrint$station1,toPrint$station2,sep="_")
tt=table(toPrint$statstat)[order(table(toPrint$statstat),decreasing=T)]
toPrint=as.data.frame(cbind(row.names(tt),unname(tt)))
names(toPrint)=c("Station Combination","Number of Significant Pairs")
row.names(toPrint)=NULL

pdf("acrossStationsDiffNutComboSTab.pdf", height=8, width=6)
grid.table(toPrint)
dev.off()




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
## 26 pw 24

## FIGURE
par(mfrow=c(4,1))
test=spectrum( cbind(D12[,"residGAMdo"],D12[,"residGAMtemp"]),taper=.2,log="no",spans=c(2,2),demean=T,detrend=F,plot=F) 
plot(test,plot.type = "coherency",sub=c("Window = 2"))
f=qf(0.95,2,test$df-2) ## make the significance level bonferroni style
C = f/(test$df/2-1+f) ## what about a double pass filter?
## test$df is 2L
abline(h=C,col="red") 

test=spectrum( cbind(D12[,"residGAMdo"],D12[,"residGAMtemp"]),taper=.2,log="no",spans=c(8,8),demean=T,detrend=F,plot=F) 
plot(test,plot.type = "coherency",sub=c("Window = 8"))
f=qf(0.95,2,test$df-2) ## make the significance level bonferroni style
C = f/(test$df/2-1+f) ## what about a double pass filter?
## test$df is 2L
abline(h=C,col="red") 

test=spectrum( cbind(D12[,"residGAMdo"],D12[,"residGAMtemp"]),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F) 
plot(test,plot.type = "coherency",sub=c("Window = 16"))
f=qf(0.95,2,test$df-2) ## make the significance level bonferroni style
C = f/(test$df/2-1+f) ## what about a double pass filter?
## test$df is 2L
abline(h=C,col="red") 

test=spectrum( cbind(D12[,"residGAMdo"],D12[,"residGAMtemp"]),taper=.2,log="no",spans=c(20,20),demean=T,detrend=F,plot=F) 
plot(test,plot.type = "coherency",sub=c("Window = 20"))
f=qf(0.95,2,test$df-2) ## make the significance level bonferroni style
C = f/(test$df/2-1+f) ## what about a double pass filter?
## test$df is 2L
abline(h=C,col="red") 

sigToPlot=withinStationDiffVar[which(p.adjust(withinStationDiffVarResults$pVal, method = "BY") <0.05),]
resToPlot=withinStationDiffVarResults[which(p.adjust(withinStationDiffVarResults$pVal, method = "BY") <0.05),]
sigToPlot
resToPlot
### make table for appendix ###


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
