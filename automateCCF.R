
acfTest=function(stationData,nutrient){
  if(sum(grepl(paste(nutrient,"Transform",sep=""),names(stationData)))>0){
    varName=paste("residGAM",nutrient,"Transform",sep="")
  }else{
    varName=paste("residGAM",nutrient)
  }
  
  empP<-c()
  for(i in 2:nrow(stationData) ){
    test1=stationData[c((i):nrow(stationData),1:(i-1)),varName]
    
    test=acf(test1,lag.max=12,plot=F)
    acfOfInterest=test$acf[2:13]
    empP<-c( empP,acfOfInterest[which.max(abs(acfOfInterest))] )
    print(i)
  }
  
  
  test=acf(stationData[,varName],lag.max=12,plot=F)
  acfOfInterest=test$acf[2:13]
  lagOfInterest=test$lag[2:13]
  maxLag=lagOfInterest[which.max(abs(acfOfInterest))]
  if(acfOfInterest[which.max(abs(acfOfInterest))]<0){
    pVal=length(which(empP<acfOfInterest[which.max(abs(acfOfInterest))]))/nrow(stationData)
    
  }else{
    pVal=length(which(empP>acfOfInterest[which.max(abs(acfOfInterest))]))/nrow(stationData)
  }
  minPval=1/nrow(stationData)
  
  return(list(maxLag=maxLag,pVal=pVal,minPval=minPval))
  
}

acfTest(D10,"chl")
#stationData=D10
#nutrient="chl"


ccfTest=function(station1Data,station2Data,station1Nutrient,station2Nutrient){
  
  if(sum(grepl(paste(station1Nutrient,"Transform",sep=""),names(station1Data)))>0){
    varName=paste("residGAM",station1Nutrient,"Transform",sep="")
  }else{
    varName=paste("residGAM",station1Nutrient)
  }
  
  if(sum(grepl(paste(station2Nutrient,"Transform",sep=""),names(station2Data)))>0){
    varName2=paste("residGAM",station2Nutrient,"Transform",sep="")
  }else{
    varName2=paste("residGAM",station2Nutrient)
  }
  
  empP<-c()
  for(i in 2:nrow(station2Data) ){
    test1=station2Data[c((i):nrow(station2Data),1:(i-1)),varName2]
    
    test=ccf(station1Data[,varName],test1,lag.max=12,plot=F)
    ccfOfInterest=test$acf[14:25]
    empP<-c( empP,ccfOfInterest[which.max(abs(ccfOfInterest))] )
    print(i)
  }
  test=ccf(station1Data[,varName],station2Data[,varName2],lag.max=12,plot=F)
  ccfOfInterest=test$acf[14:25]
  lagOfInterest=test$lag[14:25]
  maxLag=lagOfInterest[which.max(abs(ccfOfInterest))]
  if(ccfOfInterest[which.max(abs(ccfOfInterest))]<0){
    pVal=length(which(empP<ccfOfInterest[which.max(abs(ccfOfInterest))]))/nrow(station2Data)
    
  }else{
  pVal=length(which(empP>ccfOfInterest[which.max(abs(ccfOfInterest))]))/nrow(station2Data)
  }
  minPval=1/nrow(station2Data)
  
  return(list(maxLag=maxLag,pVal=pVal,minPval=minPval))
}

tryThis=ccfTest(D10,D12,"chl","chl")
#station1Data=D10
#station2Data=D12
#station1Nutrient="chl"
#station2Nutrient="chl"


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
D10<-read.csv("D10data.csv",stringsAsFactors=F)
D12<-read.csv("D12data.csv",stringsAsFactors=F)
D22<-read.csv("D22data.csv",stringsAsFactors=F)
D26<-read.csv("D26data.csv",stringsAsFactors=F)
D4<-read.csv("D4data.csv",stringsAsFactors=F)

storeData=vector("list",length(stationNames))
storeData[[1]]=D10
storeData[[2]]=D12
storeData[[3]]=D22
storeData[[4]]=D26
storeData[[5]]=D4
names(storeData)=stationNames

setwd("~/UC_Berkeley/Semester_4/timeSeries")
acrossStationSameVarResults<-c()
for(i in 1:nrow(acrossStationSameVar)){
  
 res= ccfTest(storeData[acrossStationSameVar$station1[i]],storeData[acrossStationSameVar$station2[i]],
          as.character(acrossStationSameVar$var1[i]),as.character(acrossStationSameVar$var2[i]))

 
 acrossStationSameVarResults<-rbind(acrossStationSameVarResults,c(res$maxLag,res$pVal,res$minPval))
print(i)
}

write.csv(acrossStationSameVarResults,"acrossStationSameVarResults.csv",row.names=F)


acrossStationDiffVarResults<-c()
for(i in 1:nrow(acrossStationDiffVar)){
  res= ccfTest(storeData[acrossStationDiffVar$station1[i]],storeData[acrossStationDiffVar$station2[i]],
               as.character(acrossStationDiffVar$var1[i]),as.character(acrossStationDiffVar$var2[i]))
  
  
  acrossStationDiffVarResults<-rbind(acrossStationDiffVarResults,c(res$maxLag,res$pVal,res$minPval))
  print(i)
}

write.csv(acrossStationDiffVarResults,"acrossStationDiffVarResults.csv",row.names=F)


withinStationDiffVarResults<-c()
for(i in 1:nrow(withinStationDiffVar)){
  res=ccfTest(storeData[withinStationDiffVar$station1[i]],storeData[withinStationDiffVar$station2[i]],
              as.character(withinStationDiffVar$var1[i]),as.character(withinStationDiffVar$var2[i]))
  withinStationDiffVarResults<-rbind(withinStationDiffVarResults,c(res$maxLag,res$pVal,res$minPval))
  print(i)
}
write.csv(withinStationDiffVarResults,"withinStationDiffVarResults.csv",row.names=F)



withinStationSameVarResults<-c()
for(i in 1:nrow(withinStationSameVar)){
  res=acfTest(storeData[withinStationSameVar$station1[i]],as.character(withinStationSameVar$var1[i]))
  withinStationSameVarResults<-rbind(withinStationSameVarResults,c(res$maxLag,res$pVal,res$minPval))
  print(i)
}

write.csv(withinStationSameVarResults,"withinStationSameVarResults.csv",row.names=F)

