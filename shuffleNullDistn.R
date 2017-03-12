## shuffle for null distribution

test=sfei[which(sfei$Station=="D10"),"chl"]

length(test) ## 172


origACF=acf(test)
plot(origACF$acf,type="l")
### first split into blocks

for(j in 1:10){

shuf=sample(seq(1,length(test),by=8))

orderToUse=c()
for(i in 1:length(shuf)){
  if(shuf[i]==169){
    orderToUse=c(orderToUse,seq(169,172,by=1))
  }else{
  orderToUse=c(orderToUse,seq(shuf[i],shuf[i]+7,by=1))
  }
}

test[orderToUse]



newACF=acf(test[orderToUse],plot=F)


lines(newACF$acf,col="red")

}



####
test2=sfei[which(sfei$Station=="D12"),"chl"]
length(test2)
test2=test2[-173]


origCCF=ccf(test,test2)
plot(origCCF$acf,type="l")

for(j in 1:10){
  
  shuf=sample(seq(1,length(test),by=8))
  
  orderToUse=c()
  for(i in 1:length(shuf)){
    if(shuf[i]==169){
      orderToUse=c(orderToUse,seq(169,172,by=1))
    }else{
      orderToUse=c(orderToUse,seq(shuf[i],shuf[i]+7,by=1))
    }
  }
  
  test[orderToUse]
  
  
  
  newCCF=ccf(test[orderToUse],test2,plot=F)
  
  
  lines(newCCF$acf,col="red")
  
}


