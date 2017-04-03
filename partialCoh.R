## partial coherence

## only significant coherence within a station (for now) but set up framework just in case

## same examples for now

#D22 D10 do
#but not D4
#lag 6
test1=lm(D22$residGAMdo~D4$residGAMdo)
partial=resid(test1)

station1Data=D10
station2Data=D4
varName="residGAMdo"
varName2="residGAMdo"
empP<-c()
for(i in 2:nrow(D22) ){
  test1=partial[c((i):length(partial),1:(i-1))]
  test=spectrum( cbind(station1Data[,varName],test1),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F) 
  probCoh=df(test$coh,2,2*test$df-2)
  
  f=qf(0.95,2,2*test$df-2)
  C = f/(16-1+f) ## what about a double pass filter?
  
  #test=ccf(station1Data[,varName],test1,lag.max=12,plot=F)
  #ccfOfInterest=test$acf[14:25]
  empP<-c( empP,probCoh[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])])
  #  print(i)
}
test=spectrum( cbind(station1Data[,varName],partial),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F) 
probCoh=df(test$coh,2,2*test$df-2)

f=qf(0.95,2,2*test$df-2)
C = f/(16-1+f)
maxPhase=test$phase[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]
maxCoh=test$coh[which.max(probCoh[which(probCoh>C)])]
maxFreq=1/test$freq[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]

pVal=length(which(empP>probCoh[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])] ))/nrow(station1Data)


minPval=1/nrow(station2Data)

maxPhase ## 1.96693
maxCoh ## 0.0006343933
maxFreq ##  24
pVal ## 0.4677419
minPval ## 0.005376344


#D26 pheo
#D4 sal
#but not D22
#lag 3

test1=lm(D26$residGAMpheoTransform~D22$residGAMsal)
test2=lm(D26$residGAMpheoTransform~D22$residGAMpheo)
partial=resid(test1)
partial2=resid(test2)

station1Data=D4
varName="residGAMsal"

empP<-c()
for(i in 2:nrow(D26) ){
  test1=partial[c((i):length(partial),1:(i-1))]
  test=spectrum( cbind(station1Data[,varName],test1),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F) 
  #test=ccf(station1Data[,varName],test1,lag.max=12,plot=F)
  #ccfOfInterest=test$acf[14:25]
  probCoh=df(test$coh,2,2*test$df-2)
  
  f=qf(0.95,2,2*test$df-2)
  C = f/(16-1+f)
  
  empP<-c( empP,probCoh[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])] )
  #  print(i)
}
test=spectrum( cbind(station1Data[,varName],partial),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F) 
probCoh=df(test$coh,2,2*test$df-2)

f=qf(0.95,2,2*test$df-2)
C = f/(16-1+f)
maxPhase=test$phase[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]
maxCoh=test$coh[which.max(probCoh[which(probCoh>C)])]
maxFreq=1/test$freq[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]

pVal=length(which(empP>probCoh[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])] ))/nrow(station1Data)


minPval=1/nrow(station2Data)

maxPhase ## 9.625096e-16
maxCoh ## 0.01996144
maxFreq ##   2
pVal ## 0.9731183
minPval ## 0.005376344




empP<-c()
for(i in 2:nrow(D26) ){
  test1=partial2[c((i):length(partial2),1:(i-1))]
  test=spectrum( cbind(station1Data[,varName],test1),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F) 
  probCoh=df(test$coh,2,2*test$df-2)
  
  f=qf(0.95,2,2*test$df-2)
  C = f/(16-1+f) ## what about a double pass filter?
  

  empP<-c( empP,probCoh[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]  )
  #  print(i)
}
test=spectrum( cbind(station1Data[,varName],partial2),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F,plot=F) 
probCoh=df(test$coh,2,2*test$df-2)

f=qf(0.95,2,2*test$df-2)
C = f/(16-1+f)
maxPhase=test$phase[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]
maxCoh=test$coh[which.max(probCoh[which(probCoh>C)])]
maxFreq=1/test$freq[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])]

pVal=length(which(empP>probCoh[which(probCoh>C)][which.max(probCoh[which(probCoh>C)])] ))/nrow(station1Data)


minPval=1/nrow(station2Data)

maxPhase ##-1.183674
maxCoh ##  0.0008370846
maxFreq ##   8.347826
pVal ## 0.4623656
minPval ## 0.005376344