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
  station1Data=D10
  station2Data=D4
  varName="residGAMdo"
  varName2="residGAMtemp"
  test=spectrum( cbind(station1Data[,varName],station2Data[,varName2]),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F) 
  
  #plot(test$coh,df(test$coh/(1-test$coh)*(test$df/2-1),2,test$df-2))
  
  ## under a null hypothesis of no coherence, test statistic
  ## squared coherence/(1-squared coherence)*(L-1) follows an F distribution 2, 2L-2
  ## lab 5 pg 6
  
  pVal=df(test$coh/(1-test$coh)*(test$df/2-1),2,test$df-2)[which.max(test$coh)]
## need degrees of freedom to be smaller for the p-value to get bigger
  
 # gg<-2/test$df
  #se<-sqrt(gg/2)
  #z1<- -qnorm(0.025)
  #z2<- -qnorm(0.975)
  #coh<-sqrt(test$coh)
  
  
  #f=df(test$coh,2,test$df-2)# make the significance level bonferroni style
  #tryThis = f/(test$df/2-1+f)
 #pVal=tryThis[which.max(test$coh)]
  maxPhase=test$phase[which.max(test$coh)]
  maxCoh=test$coh[which.max(test$coh)]
  maxFreq=1/test$freq[which.max(test$coh)]
  
  
  return(list(maxPhase=maxPhase,maxCoh=maxCoh,maxFreq=maxFreq,pVal=pVal))
}

ccfTestFreq(D10,D12,"chl","pheo")

## need pval still
acfTestFreq=function(stationData,nutrient){
  if(sum(grepl(paste(nutrient,"Transform",sep=""),names(stationData)))>0){
    varName=paste("residGAM",nutrient,"Transform",sep="")
  }else{
    varName=paste("residGAM",nutrient,sep="")
  }
  #stationData=D10
 # varName="residGAMdo"
  test=spectrum(stationData[,varName],taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F)
  dchisq(test$spec,2)
  dchisq(test$spec,test$df)
 # qchisq(0.975,2) ## withoutsmoothing
#  qchisq(0.975,test$df) ## with smoothing
  plot(test)
  
  ## need to ask about p-val dist
  ## pg 366 timeSeriesAppR
  # https://books.google.com/books?id=Ks9PBjkjyckC&pg=PA364&lpg=PA364&dq=null+distribution+for+periodogram&source=bl&ots=mT_QE4LXLY&sig=y8URHyu9Vq1zzNZf4I3kCY2PQ90&hl=en&sa=X&ved=0ahUKEwiD_MLkxrTTAhXBx1QKHT_MDpwQ6AEIRjAH#v=onepage&q=null%20distribution%20for%20periodogram&f=false
  
  # empP<-c()
  # for(i in 2:nrow(stationData) ){
  #   test1=stationData[c((i):nrow(stationData),1:(i-1)),varName]
  #   
  #   test=spectrum(test1,taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F)
  #   
  #   
  #   #U = qchisq(.025, 2) #upper value
  #   #L = qchisq(.975, 2) #lower value
  #   #CI = c(2*test$spec[which.max(test$spec)]/L, test$spec[which.max(test$spec)], 2*test$spec[which.max(test$spec)]/U)
  #   #if(CI[1]<0)
  #   
  #   #empP<-c( empP, pchisq(test$spec[which.max(test$spec)],2))
  #   empP<-c(empP,test$spec[which.max(test$spec)])
  #   # print(i)
  # }
  # 
  # test=spectrum(stationData[,varName],taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F)
  maxSpec=test$spec[which.max(test$spec)]
  maxFreq=1/test$freq[which.max(test$spec)]
  pVal=length(which(empP>maxSpec))/nrow(stationData)
  
  #minPval=1/nrow(stationData)
  
  return(list(maxSpec=maxSpec,maxFreq=maxFreq,pVal=pVal))
  
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
  
  
  acrossStationSameVarResults<-rbind(acrossStationSameVarResults,c(res$maxPhase,res$maxCoh,res$maxFreq,res$pVal))
  print(i)
}

write.csv(acrossStationSameVarResults,"acrossStationSameVarResultsFreq.csv",row.names=F)


acrossStationDiffVarResults<-c()
for(i in 1:nrow(acrossStationDiffVar)){
  res= ccfTestFreq(storeData[[acrossStationDiffVar$station1[i]]],storeData[[acrossStationDiffVar$station2[i]]],
                   as.character(acrossStationDiffVar$var1[i]),as.character(acrossStationDiffVar$var2[i]))
  
  
  acrossStationDiffVarResults<-rbind(acrossStationDiffVarResults,c(res$maxPhase,res$maxCoh,res$maxFreq,res$pVal))
  print(i)
}

write.csv(acrossStationDiffVarResults,"acrossStationDiffVarResultsFreq.csv",row.names=F)


withinStationDiffVarResults<-c()
for(i in 1:nrow(withinStationDiffVar)){
  res=ccfTestFreq(storeData[[withinStationDiffVar$station1[i]]],storeData[[withinStationDiffVar$station2[i]]],
                  as.character(withinStationDiffVar$var1[i]),as.character(withinStationDiffVar$var2[i]))
  withinStationDiffVarResults<-rbind(withinStationDiffVarResults,c(res$maxPhase,res$maxCoh,res$maxFreq,res$pVal))
  print(i)
}
write.csv(withinStationDiffVarResults,"withinStationDiffVarResultsFreq.csv",row.names=F)


## need to fix acf version first
withinStationSameVarResults<-c()
for(i in 1:nrow(withinStationSameVar)){
  res=acfTestFreq(storeData[[withinStationSameVar$station1[i]]],as.character(withinStationSameVar$var1[i]))
  withinStationSameVarResults<-rbind(withinStationSameVarResults,c(res$maxSpec,res$maxFreq,res$pVal))
  print(i)
}

write.csv(withinStationSameVarResults,"withinStationSameVarResultsFreq.csv",row.names=F)


