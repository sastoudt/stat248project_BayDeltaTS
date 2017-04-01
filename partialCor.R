## partial correlation

## no paths of three to test removing the middle

## but two examples of bypassing an in between station
## we would expect the partial correlation to still find significance

#D22 D10 do
#but not D4
#lag 6
test1=lm(D22$residGAMdo~D4$residGAMdo)
partial=resid(test1)

station1Data=D10
varName="residGAMdo"
empP<-c()
for(i in 2:nrow(D22) ){
  test1=partial[c((i):length(partial),1:(i-1))]
  
  test=ccf(station1Data[,varName],test1,lag.max=12,plot=F)
  ccfOfInterest=test$acf[14:25]
  empP<-c( empP,ccfOfInterest[which.max(abs(ccfOfInterest))] )
  #  print(i)
}
test=ccf(station1Data[,varName],partial,lag.max=12,plot=F)
ccfOfInterest=test$acf[14:25]
lagOfInterest=test$lag[14:25]
maxLag=lagOfInterest[which.max(abs(ccfOfInterest))]
if(ccfOfInterest[which.max(abs(ccfOfInterest))]<0){
  pVal=length(which(empP<ccfOfInterest[which.max(abs(ccfOfInterest))]))/nrow(station1Data)
  
}else{
  pVal=length(which(empP>ccfOfInterest[which.max(abs(ccfOfInterest))]))/nrow(station1Data)
}
minPval=1/nrow(station1Data)

maxLag ## 9
pVal ## 0.04301075
minPval ## 0.005376344

## still significant, max lag changed

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
  
  test=ccf(station1Data[,varName],test1,lag.max=12,plot=F)
  ccfOfInterest=test$acf[14:25]
  empP<-c( empP,ccfOfInterest[which.max(abs(ccfOfInterest))] )
  #  print(i)
}
test=ccf(station1Data[,varName],partial,lag.max=12,plot=F)
ccfOfInterest=test$acf[14:25]
lagOfInterest=test$lag[14:25]
maxLag=lagOfInterest[which.max(abs(ccfOfInterest))]
if(ccfOfInterest[which.max(abs(ccfOfInterest))]<0){
  pVal=length(which(empP<ccfOfInterest[which.max(abs(ccfOfInterest))]))/nrow(station1Data)
  
}else{
  pVal=length(which(empP>ccfOfInterest[which.max(abs(ccfOfInterest))]))/nrow(station1Data)
}
minPval=1/nrow(station1Data)

maxLag ## 6
pVal ## 0.06989247
minPval ## 0.005376344

empP<-c()
for(i in 2:nrow(D26) ){
  test1=partial2[c((i):length(partial2),1:(i-1))]
  
  test=ccf(station1Data[,varName],test1,lag.max=12,plot=F)
  ccfOfInterest=test$acf[14:25]
  empP<-c( empP,ccfOfInterest[which.max(abs(ccfOfInterest))] )
  #  print(i)
}
test=ccf(station1Data[,varName],partial2,lag.max=12,plot=F)
ccfOfInterest=test$acf[14:25]
lagOfInterest=test$lag[14:25]
maxLag=lagOfInterest[which.max(abs(ccfOfInterest))]
if(ccfOfInterest[which.max(abs(ccfOfInterest))]<0){
  pVal=length(which(empP<ccfOfInterest[which.max(abs(ccfOfInterest))]))/nrow(station1Data)
  
}else{
  pVal=length(which(empP>ccfOfInterest[which.max(abs(ccfOfInterest))]))/nrow(station1Data)
}
minPval=1/nrow(station1Data)

maxLag ## 6
pVal ##0.07526882
minPval ## 0.005376344

## close

## change in lag
