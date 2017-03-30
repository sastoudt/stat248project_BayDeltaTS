#### pick out stationary part of each series, subtract loess
#### variance stabilize (log)

setwd("~/UC_Berkeley/Semester_4/timeSeries")

sfeiData<-read.csv("sfeiDataForProject.csv",stringsAsFactors=FALSE)
names(sfeiData)
## "chl"       "do"        "pheo"      "sal"       "temp"   

length(unique(sfeiData$Station)) ## 5
unique(sfeiData$Station) ## "D10" "D12" "D22" "D26" "D4" 

D10<-subset(sfeiData,Station=="D10")
D12<-subset(sfeiData,Station=="D12")
D22<-subset(sfeiData,Station=="D22")
D26<-subset(sfeiData,Station=="D26")
D4<-subset(sfeiData,Station=="D4")

plot(D10$chl,type="l")

require(mgcv)

require(parallel)  
nc <- 4   ## have up to 8
if (detectCores()>1) { ## no point otherwise
  cl <- makeCluster(nc) 
} else cl <- NULL

sfeiData$Station=as.factor(sfeiData$Station)
ptm <- proc.time()
chlSeasonalTimeTrend=bam(chl~Station+s(date_dec,bs="cs",by=Station,k=25)+s(doy,bs="cs",by=Station,k=25),data=sfeiData,cluster=cl)
proc.time() - ptm
summary(chlSeasonalTimeTrend)
gam.check(chlSeasonalTimeTrend)
plot(chlSeasonalTimeTrend)

sfeiData$predGAMchl=chlSeasonalTimeTrend$fitted
sfeiData$residGAMchl=chlSeasonalTimeTrend$resid

D10<-subset(sfeiData,Station=="D10")
plot(D10$chl,type="l")
lines(D10$residGAMchl,col="red")
lines(D10$predGAMchl,col="blue")
#head(D10$chl-D10$predGAM)
#head(D10$residGAM)
plot(D10$residGAMchl) ## check for stationarity

ptm <- proc.time()
doSeasonalTimeTrend=bam(do~Station+s(date_dec,bs="cs",by=Station,k=25)+s(doy,bs="cs",by=Station,k=25),data=sfeiData,cluster=cl)
proc.time() - ptm
summary(doSeasonalTimeTrend)
gam.check(doSeasonalTimeTrend)
plot(doSeasonalTimeTrend)

sfeiData$predGAMdo=doSeasonalTimeTrend$fitted
sfeiData$residGAMdo=doSeasonalTimeTrend$resid


D10<-subset(sfeiData,Station=="D10")
plot(D10$do,type="l")
lines(D10$residGAMdo,col="red")
lines(D10$predGAMdo,col="blue")

plot(D10$residGAMdo,type="l") ## check for stationarity
