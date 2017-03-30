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

#### chl ####
sfeiData$Station=as.factor(sfeiData$Station)
ptm <- proc.time()
chlSeasonalTimeTrend=bam(chl~Station+s(date_dec,bs="cs",by=Station,k=25)+s(doy,bs="cs",by=Station,k=25),data=sfeiData,cluster=cl)
proc.time() - ptm
summary(chlSeasonalTimeTrend)
gam.check(chlSeasonalTimeTrend)
plot(chlSeasonalTimeTrend)
## datedec flat for D10, D12, D4
## increasing for D22
## curved updward for D26
## doy peak in 100s for D10, D12, D4
## plateau 100 to 250 for D22, D26

sfeiData$predGAMchl=chlSeasonalTimeTrend$fitted
sfeiData$residGAMchl=chlSeasonalTimeTrend$resid

D10<-subset(sfeiData,Station=="D10")
D12<-subset(sfeiData,Station=="D12")
D22<-subset(sfeiData,Station=="D22")
D26<-subset(sfeiData,Station=="D26")
D4<-subset(sfeiData,Station=="D4")

plot(D10$chl,type="l")
lines(D10$residGAMchl,col="red")
lines(D10$predGAMchl,col="blue")
#head(D10$chl-D10$predGAM)
#head(D10$residGAM)

par(mfrow=c(2,1))
plot(D10$residGAMchl,type="l") ## check for stationarity
plot(log(D10$residGAMchl+abs(min(D10$residGAMchl))),type="l")

plot(D12$residGAMchl,type="l")
plot(log(D12$residGAMchl+abs(min(D12$residGAMchl))),type="l")

plot(D22$residGAMchl,type="l")
plot(log(D22$residGAMchl+abs(min(D22$residGAMchl))),type="l")

plot(D26$residGAMchl,type="l")
plot(log(D26$residGAMchl+abs(min(D26$residGAMchl))),type="l")

plot(D4$residGAMchl,type="l")
plot(log(D4$residGAMchl+abs(min(D4$residGAMchl))),type="l")

D10$residGAMchlTransform=log(D10$residGAMchl+abs(min(D10$residGAMchl)))
D12$residGAMchlTransform=log(D12$residGAMchl+abs(min(D12$residGAMchl)))
D22$residGAMchlTransform=log(D22$residGAMchl+abs(min(D22$residGAMchl)))
D26$residGAMchlTransform=log(D26$residGAMchl+abs(min(D26$residGAMchl)))
D4$residGAMchlTransform=log(D4$residGAMchl+abs(min(D4$residGAMchl)))
                             

## needs variance stabilizing, plus outliers at end?

#### do ####
ptm <- proc.time()
doSeasonalTimeTrend=bam(do~Station+s(date_dec,bs="cs",by=Station,k=25)+s(doy,bs="cs",by=Station,k=25),data=sfeiData,cluster=cl)
proc.time() - ptm
summary(doSeasonalTimeTrend)
gam.check(doSeasonalTimeTrend)
plot(doSeasonalTimeTrend)
## datedec slight concave D10, D12, D22 D4
## flatter D26
## doy peak 50, min peak 200 all

sfeiData$predGAMdo=doSeasonalTimeTrend$fitted
sfeiData$residGAMdo=doSeasonalTimeTrend$resid


D10<-subset(sfeiData,Station=="D10")
D12<-subset(sfeiData,Station=="D12")
D22<-subset(sfeiData,Station=="D22")
D26<-subset(sfeiData,Station=="D26")
D4<-subset(sfeiData,Station=="D4")

plot(D10$do,type="l")
lines(D10$residGAMdo,col="red")
lines(D10$predGAMdo,col="blue")

## these don't seem to help stick with original
par(mfrow=c(2,1))
plot(D10$residGAMdo,type="l") ## check for stationarity
plot(log(D10$residGAMdo+abs(min(D10$residGAMdo))),type="l")

plot(D12$residGAMdo,type="l")
plot(log(D12$residGAMdo+abs(min(D12$residGAMdo))),type="l")

plot(D22$residGAMdo,type="l")
plot(log(D22$residGAMdo+abs(min(D22$residGAMdo))),type="l")

plot(D26$residGAMdo,type="l")
plot(log(D26$residGAMdo+abs(min(D26$residGAMdo))),type="l")

plot(D4$residGAMdo,type="l")
plot(log(D4$residGAMdo+abs(min(D4$residGAMdo))),type="l")

## could benefit from variance reduction, outliers early and late

#### pheo ####
ptm <- proc.time()
pheoSeasonalTimeTrend=bam(pheo~Station+s(date_dec,bs="cs",by=Station,k=25)+s(doy,bs="cs",by=Station,k=25),data=sfeiData,cluster=cl)
proc.time() - ptm
summary(pheoSeasonalTimeTrend)
gam.check(pheoSeasonalTimeTrend)
plot(pheoSeasonalTimeTrend)
## date dec flat D10, D12, D4
## downward trend D22
## down and up D26
## doy, peak 125, low point 275 D10 
## concave slight peak around 150 D12, D26, D4 (even slighter for D22)


sfeiData$predGAMpheo=pheoSeasonalTimeTrend$fitted
sfeiData$residGAMpheo=pheoSeasonalTimeTrend$resid


D10<-subset(sfeiData,Station=="D10")
D12<-subset(sfeiData,Station=="D12")
D22<-subset(sfeiData,Station=="D22")
D26<-subset(sfeiData,Station=="D26")
D4<-subset(sfeiData,Station=="D4")

plot(D10$pheo,type="l")
lines(D10$residGAMpheo,col="red")
lines(D10$predGAMpheo,col="blue")

par(mfrow=c(2,1))
plot(D10$residGAMpheo,type="l") ## check for stationarity
plot(log(D10$residGAMpheo+abs(min(D10$residGAMpheo))),type="l")

plot(D12$residGAMpheo,type="l")
plot(log(D12$residGAMpheo+abs(min(D12$residGAMpheo))),type="l")

plot(D22$residGAMpheo,type="l")
plot(log(D22$residGAMpheo+abs(min(D22$residGAMpheo))),type="l")

plot(D26$residGAMpheo,type="l")
plot(log(D26$residGAMpheo+abs(min(D26$residGAMpheo))),type="l")

plot(D4$residGAMpheo,type="l")
plot(log(D4$residGAMpheo+abs(min(D4$residGAMpheo))),type="l")

## could benefit from variance reduction, outliers at end of series

D10$residGAMpheoTransform=log(D10$residGAMpheo+abs(min(D10$residGAMpheo)))
D12$residGAMpheoTransform=log(D12$residGAMpheo+abs(min(D12$residGAMpheo)))
D22$residGAMpheoTransform=log(D22$residGAMpheo+abs(min(D22$residGAMpheo)))
D26$residGAMpheoTransform=log(D26$residGAMpheo+abs(min(D26$residGAMpheo)))
D4$residGAMpheoTransform=log(D4$residGAMpheo+abs(min(D4$residGAMpheo)))

#### sal ####

ptm <- proc.time()
salSeasonalTimeTrend=bam(sal~Station+s(date_dec,bs="cs",by=Station,k=25)+s(doy,bs="cs",by=Station,k=25),data=sfeiData,cluster=cl)
proc.time() - ptm
summary(salSeasonalTimeTrend)
gam.check(salSeasonalTimeTrend)
plot(salSeasonalTimeTrend)
## date dec D10 wiggly peak 2008, same wiggles but dampened D12, D4
## concave plateau D22
## flat D26
## doy min peak 75, peak 325 D10, D4 same but dampened D12, D22
## flat D26

sfeiData$predGAMsal=salSeasonalTimeTrend$fitted
sfeiData$residGAMsal=salSeasonalTimeTrend$resid


D10<-subset(sfeiData,Station=="D10")
D12<-subset(sfeiData,Station=="D12")
D22<-subset(sfeiData,Station=="D22")
D26<-subset(sfeiData,Station=="D26")
D4<-subset(sfeiData,Station=="D4")

plot(D10$sal,type="l") 
lines(D10$residGAMsal,col="red")
lines(D10$predGAMsal,col="blue")

## these don't seem to help, stick to original
par(mfrow=c(2,1))
plot(D10$residGAMsal,type="l") ## check for stationarity
plot(log(D10$residGAMsal+abs(min(D10$residGAMsal))),type="l")

plot(D12$residGAMsal,type="l")
plot(log(D12$residGAMsal+abs(min(D12$residGAMsal))),type="l")

plot(D22$residGAMsal,type="l")
plot(log(D22$residGAMsal+abs(min(D22$residGAMsal))),type="l")

plot(D26$residGAMsal,type="l")
plot(log(D26$residGAMsal+abs(min(D26$residGAMsal))),type="l")

plot(D4$residGAMsal,type="l")
plot(log(D4$residGAMsal+abs(min(D4$residGAMsal))),type="l")

##could benefit from variance stabilization some big peaks early and end

#### temp ####

ptm <- proc.time()
tempSeasonalTimeTrend=bam(temp~Station+s(date_dec,bs="cs",by=Station,k=25)+s(doy,bs="cs",by=Station,k=25),data=sfeiData,cluster=cl)
proc.time() - ptm
summary(tempSeasonalTimeTrend)
gam.check(tempSeasonalTimeTrend)
plot(tempSeasonalTimeTrend)
## datedec nearly flat D10, D12, D26, D4
## flat D22
## doy peak 200 all

sfeiData$predGAMtemp=tempSeasonalTimeTrend$fitted
sfeiData$residGAMtemp=tempSeasonalTimeTrend$resid


D10<-subset(sfeiData,Station=="D10")
D12<-subset(sfeiData,Station=="D12")
D22<-subset(sfeiData,Station=="D22")
D26<-subset(sfeiData,Station=="D26")
D4<-subset(sfeiData,Station=="D4")

plot(D10$temp,type="l")
lines(D10$residGAMtemp,col="red")
lines(D10$predGAMtemp,col="blue")

## doesn't really help, stick with original
par(mfrow=c(2,1))
plot(D10$residGAMtemp,type="l") ## check for stationarity
plot(log(D10$residGAMtemp+abs(min(D10$residGAMtemp))),type="l")

plot(D12$residGAMtemp,type="l")
plot(log(D12$residGAMtemp+abs(min(D12$residGAMtemp))),type="l")

plot(D22$residGAMtemp,type="l")
plot(log(D22$residGAMtemp+abs(min(D22$residGAMtemp))),type="l")

plot(D26$residGAMtemp,type="l")
plot(log(D26$residGAMtemp+abs(min(D26$residGAMtemp))),type="l")

plot(D4$residGAMtemp,type="l")
plot(log(D4$residGAMtemp+abs(min(D4$residGAMtemp))),type="l")

## could benefit from variance stabilization

## make plots x axis date_dec for report
## clean up axis labels and titles

## going to do log transforms but this changes the correlations, I think this is still ok
## Kendall rank order correlation for time series?

## do I need to demean?