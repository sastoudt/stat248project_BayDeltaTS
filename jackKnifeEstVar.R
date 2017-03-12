### jack knife estimate of variance

test=sfei[which(sfei$Station=="D10"),"chl"]

splitSeg=seq(1,length(test),by=15) ## want to capture whole season?

testA=acf(test[-c(1:14)])
jkResults=matrix(NA,nrow=length(splitSeg),ncol=length(testA$acf))
for(i in 1:(length(splitSeg)-1)){
  
  
testA=acf(test[-c(splitSeg[i]:(splitSeg[i]+14))])  
  jkResults[i,]=as.vector(testA$acf)
  print(i)
}

testA=acf(test[-c(splitSeg[12]:172)])  
jkResults[12,]=as.vector(testA$acf)[1:ncol(jkResults)]

varPerLag=apply(jkResults,2,var,na.rm=T)
meanPerLag=apply(jkResults,2,mean,na.rm=T)
varPerLag
meanPerLag

plot(meanPerLag)
points(meanPerLag+2*varPerLag)
points(meanPerLag-2*varPerLag)
## pick correct length of segments



