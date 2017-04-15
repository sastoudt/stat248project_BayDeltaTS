setwd("~/UC_Berkeley/Semester_4/timeSeries")
D10<-read.csv("D10data.csv",stringsAsFactors=F)
D12<-read.csv("D12data.csv",stringsAsFactors=F)
D22<-read.csv("D22data.csv",stringsAsFactors=F)
D26<-read.csv("D26data.csv",stringsAsFactors=F)
D4<-read.csv("D4data.csv",stringsAsFactors=F)
stationNames<-c("D10","D12","D22","D26","D4")


storeData=vector("list",length(stationNames))
storeData[[1]]=D10
storeData[[2]]=D12
storeData[[3]]=D22
storeData[[4]]=D26
storeData[[5]]=D4
names(storeData)=stationNames
periodogram(D10$residGAMchl)

## two passes of modified Daniell kernel
test=spec.pgram(D10$residGAMchl,spans=c(16,16),taper=0.2,log="no")
names(test)
test$freq
test$spec
plot(test$freq,test$spec)
points(test$freq,test$freq/qchisq(.025,2),col="red")


test$spec[which(test$freq>0.1 & test$freq<0.15)]

1/test$freq[21] ## 9 months

1/0.2 ## 5 months

1/0.3 ## 3 months

1/0.4 ## 2.5 months

##http://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

1/test$freq[localMaxima(test$spec)]


spectrum(D10$residGAMchl)
test=spectrum( D4$residGAMchl,taper=.2,log="no",spans=c(16,16),demean=T,detrend=F) 
test=spec( D4$residGAMchl,taper=.2,log="no",spans=c(16,16),demean=T,detrend=F)
plot(test,type="spec")
pchisq(test$spec[which.max(test$spec)],test$df)
qchisq(0.95,test$df)
names(test)

dchisq(1/test$spec*test$df,2)
dchisq(test$spec,test$df)

Upper = qchisq(.025, test$df)
Lower = qchisq(.975, test$df)
value = test$spec
CI = cbind(test$df*value/Lower, value, test$df*value/Upper)
points(test$freq,CI[,1])
points(test$freq,CI[,3])
pchisq(test$spec,test$df)


plot(D10$residGAMchl,type="l")

test=spectrum( cbind(D10$residGAMchl,D4$residGAMdo),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F) 
#test$coh
#test$phase

gg<-2/test$df
se<-sqrt(gg/2)
z1<- -qnorm(0.025)
z2<- -qnorm(0.975)
coh<-sqrt(test$coh)
#tanh(atanh(coh)+z*se)^2
#https://stat.ethz.ch/pipermail/r-help/2014-October/422946.html
f=qf(0.95,2,test$df-2) ## make the significance level bonferroni style
C = f/(test$df/2-1+f) ## what about a double pass filter?
## test$df is 2L
plot(test, plot.type = "coherency")
abline(h=C,col="red") 
points(test$freq,tanh(atanh(coh)+z1*se)^2,col="red")
points(test$freq,tanh(atanh(coh)+z2*se)^2)


probCoh=df(test$coh,2,test$df-2)


f=df(test$coh,2,test$df-2)# make the significance level bonferroni style
tryThis = f/(test$df/2-1+f)
tryThis[which.max(test$coh)]
which.min(tryThis)
plot(test$coh,tryThis)

df(test$coh[which.max(test$coh)],2,test$df-2)
df(C,2,test$df-2)
qf(1-0.05,2,test$df-2)

plot(test$coh,1-probCoh)


probCoh[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]
1/test$freq[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]
test$phase[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]





plot(test, plot.type = "coherency")
abline(h=C,col="red") ## go back to this, now see if lower bound is above this
## but now don't have p-value for multiple testing
plot(test, plot.type = "phase")

##
test2=spectrum( cbind(D10$residGAMchl,D4$residGAMpheo),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F) 
#test$coh
#test$phase

probCoh=df(test2$coh,2,2*test2$df-2)

f=qf(0.95,2,2*test2$df-2)
C = f/(16-1+f) ## what about a double pass filter?

probCoh[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]
1/test$freq[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]
test$phase[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]

station1Data=D10
station2Data=D4
station1Nutrient="chl"
station2Nutrient="pheo"

ccfTestFreq=function(station1Data,station2Data,station1Nutrient,station2Nutrient){
  
  if(sum(grepl(paste(station1Nutrient,"Transform",sep=""),names(station1Data)))>0){
    varName=paste("residGAM",station1Nutrient,"Transform",sep="")
  }else{
    varName=paste("residGAM",station1Nutrient,sep="")
  }
  
  if(sum(grepl(paste(station2Nutrient,"Transform",sep=""),names(station2Data)))>0){
    varName2=paste("residGAM",station2Nutrient,"Transform",sep="")
  }else{
    varName2=paste("residGAM",station2Nutrient,sep="")
  }
  
  empP<-c()
  for(i in 2:nrow(station2Data) ){
    test1=station2Data[c((i):nrow(station2Data),1:(i-1)),varName2]
    test=spectrum( cbind(station1Data[,varName],test1),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F) 
    #test=ccf(station1Data[,varName],test1,lag.max=12,plot=F)
    
    probCoh=df(test$coh,2,2*test$df-2)
    f=qf(0.95,2,2*test$df-2)
    C = f/(16-1+f) ## what about a double pass filter?
    #empP<-c( empP,probCoh[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])] )
    
    
    
    empP<-c(empP,test$coh[which(probCoh>C)][which.max(test$coh[which(probCoh>C)])])
    #empP<-c(empP,test$coh[which.max(test$coh)])
    #  print(i)
  }
  test=spectrum( cbind(station1Data[,varName],station2Data[,varName2]),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F) 
  #probCoh=df(test$coh,2,2*test$df-2)
  
  #f=qf(0.95,2,2*test$df-2)
  #C = f/(16-1+f)
  #maxPhase=test$phase[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]
  #maxCoh=test$coh[which.max(probCoh[which(probCoh>C)])]
  #maxFreq=1/test$freq[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]
  

  pVal=length(which(empP>test$coh[which.max(test$coh)]))/nrow(station1Data)
  
  maxPhase=test$phase[which.max(test$coh)]
  maxCoh=test$coh[which.max(test$coh)]
  maxFreq=1/test$freq[which.max(test$coh)]
  minPval=1/nrow(station2Data)
  
  return(list(maxPhase=maxPhase,maxCoh=maxCoh,maxFreq=maxFreq,pVal=pVal,minPval=minPval))
}

ccfTestFreq(D10,D12,"chl","pheo")


acfTestFreq=function(stationData,nutrient){
  if(sum(grepl(paste(nutrient,"Transform",sep=""),names(stationData)))>0){
    varName=paste("residGAM",nutrient,"Transform",sep="")
  }else{
    varName=paste("residGAM",nutrient,sep="")
  }
  
  empP<-c()
  for(i in 2:nrow(stationData) ){
    test1=stationData[c((i):nrow(stationData),1:(i-1)),varName]

    test=spectrum(test1,taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F)
    #U = qchisq(.025, 2) #upper value
    #L = qchisq(.975, 2) #lower value
    #CI = c(2*test$spec[which.max(test$spec)]/L, test$spec[which.max(test$spec)], 2*test$spec[which.max(test$spec)]/U)
    #if(CI[1]<0)
   
    #empP<-c( empP, pchisq(test$spec[which.max(test$spec)],2))
    empP<-c(empP,test$spec[which.max(test$spec)])
    # print(i)
  }
  
  test=spectrum(stationData[,varName],taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F)
maxSpec=test$spec[which.max(test$spec)]
maxFreq=1/test$freq[which.max(test$spec)]
pVal=length(which(empP>maxSpec))/nrow(stationData)
  
  minPval=1/nrow(stationData)
  
  return(list(maxSpec=maxSpec,maxFreq=maxFreq,pVal=pVal,minPval=minPval))
  
}


acfTestFreq(D10,"chl")
######

stationNames<-c("D10","D12","D22","D26","D4")
varNames<-c("chl","do","pheo","sal","temp")

allCombo=expand.grid(stationNames,stationNames,varNames,varNames)
dim(allCombo)
head(allCombo)
names(allCombo)=c("station1","station2","var1","var2")

withinStation=allCombo[which(allCombo$station1==allCombo$station2),]
dim(withinStation) ## 125

withinStationSameVar=withinStation[which(withinStation$var1==withinStation$var2),]
dim(withinStationSameVar) ## 25

withinStationDiffVar=withinStation[which(withinStation$var1!=withinStation$var2),]
dim(withinStationDiffVar) ## 100

leftOver=allCombo[-which(allCombo$station1==allCombo$station2),]
acrossStationSameVar=leftOver[which(leftOver$var1==leftOver$var2),]
dim(acrossStationSameVar) ## 100

leftOver2=leftOver[-which(leftOver$var1==leftOver$var2),]
dim(leftOver2)

acrossStationDiffVar=leftOver2[which(leftOver2$var1!=leftOver2$var2),]
dim(acrossStationDiffVar) ## 400 same as leftOver2 as expected 



setwd("~/UC_Berkeley/Semester_4/timeSeries")
acrossStationSameVarResults<-c()
for(i in 1:nrow(acrossStationSameVar)){
  
  res=ccfTestFreq(storeData[[acrossStationSameVar$station1[i]]],storeData[[acrossStationSameVar$station2[i]]],
               as.character(acrossStationSameVar$var1[i]),as.character(acrossStationSameVar$var2[i]))
  

  acrossStationSameVarResults<-rbind(acrossStationSameVarResults,c(res$maxPhase,res$maxCoh,res$maxFreq,res$pVal,res$minPval))
  print(i)
}

write.csv(acrossStationSameVarResults,"acrossStationSameVarResultsFreq.csv",row.names=F)


acrossStationDiffVarResults<-c()
for(i in 1:nrow(acrossStationDiffVar)){
  res= ccfTestFreq(storeData[[acrossStationDiffVar$station1[i]]],storeData[[acrossStationDiffVar$station2[i]]],
               as.character(acrossStationDiffVar$var1[i]),as.character(acrossStationDiffVar$var2[i]))
  
  
  acrossStationDiffVarResults<-rbind(acrossStationDiffVarResults,c(res$maxPhase,res$maxCoh,res$maxFreq,res$pVal,res$minPval))
  print(i)
}

write.csv(acrossStationDiffVarResults,"acrossStationDiffVarResultsFreq.csv",row.names=F)


withinStationDiffVarResults<-c()
for(i in 1:nrow(withinStationDiffVar)){
  res=ccfTestFreq(storeData[[withinStationDiffVar$station1[i]]],storeData[[withinStationDiffVar$station2[i]]],
              as.character(withinStationDiffVar$var1[i]),as.character(withinStationDiffVar$var2[i]))
  withinStationDiffVarResults<-rbind(withinStationDiffVarResults,c(res$maxPhase,res$maxCoh,res$maxFreq,res$pVal,res$minPval))
  print(i)
}
write.csv(withinStationDiffVarResults,"withinStationDiffVarResultsFreq.csv",row.names=F)


## need to make acfFreq
withinStationSameVarResults<-c()
for(i in 1:nrow(withinStationSameVar)){
  res=acfTestFreq(storeData[[withinStationSameVar$station1[i]]],as.character(withinStationSameVar$var1[i]))
  withinStationSameVarResults<-rbind(withinStationSameVarResults,c(res$maxSpec,res$maxFreq,res$pVal,res$minPval))
  print(i)
}

write.csv(withinStationSameVarResults,"withinStationSameVarResultsFreq.csv",row.names=F)


