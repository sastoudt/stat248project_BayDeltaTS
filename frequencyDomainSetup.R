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
test=spectrum( cbind(D10$residGAMchl,D4$residGAMchl),taper=.2,log="no",spans=c(16,16),demean=T,detrend=F) 
test$coh
test$phase

f=qf(0.95,2,2*test$df-2)
C = f/(16+f) ## what about a double pass filter?

plot(test, plot.type = "coherency")
abline(h=C,col="red")
plot(test, plot.type = "phase")


plot(test$phase)
pf(test$coh,2,2*L-2)