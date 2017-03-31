## eda of data
setwd("~/UC_Berkeley/Semester_4/timeSeries")
sfei<-read.csv("sfeiDataForProject.csv",stringsAsFactors=F)

longitude=c(-121.2647, -121.5205, -121.9183, -121.8063, -121.6148, -121.7391, -121.5669, -121.5730, -121.8205,
            -122.3729, -122.1177,-122.0397, -121.9900, -121.4199, -121.3823)
latitude=c(37.67934, 38.36771, 38.04631, 38.02161, 38.04376, 38.08453, 38.07664, 37.97048, 38.06248, 38.03022,
           38.04436, 38.11714, 38.05992, 38.04226, 37.97817)

stationNames= c("C10", "C3", "C7" , "C9","D10","D11","D12","D14A","D15","D16","D19",    
                "D2","D22", "D24","D26","D28A", "D4","D41","D41A","D42", "D6","D7", "D8","D9","EZ2","EZ2-SJR", "EZ6","EZ6-SJR",
                "MD10","MD6", "MD7","NZ002","NZ004","NZ032","NZ325","NZS42","P10" ,"P12","P2", "P8","S42")

#plot(longitude,latitude)
#points(sfei$Longitude,sfei$Latitude,col="red")
#text(longitude[which(stationNames %in% unique(sfei$Station))],latitude[which(stationNames %in% unique(sfei$Station))],col="red",stationNames[which(stationNames %in% unique(sfei$Station))])

load(file="~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data/delt_map.RData")
plot(delt_map)
points(longitude,latitude,pch=19)
points(sfei$Longitude,sfei$Latitude,col="red",cex=2)

require(ggplot2)
ggplot(sfei, aes(x=date_dec, y=chl, colour=as.factor(Station)))+geom_line()+xlim(2000,2015)
ggplot(sfei, aes(x=date_dec, y=pheo, colour=as.factor(Station)))+geom_line()+xlim(2000,2015)
ggplot(sfei, aes(x=date_dec, y=temp, colour=as.factor(Station)))+geom_line()+xlim(2000,2015)
ggplot(sfei, aes(x=date_dec, y=sal, colour=as.factor(Station)))+geom_line()+xlim(2000,2015)


## 5 stations 5 nutrients

chlPlot <- ggplot(sfei, aes(x=date_dec, y=chl))+geom_line()+xlim(2000,2015)
chlPlot<- chlPlot + facet_grid(Station ~ .,
                                             labeller=label_parsed,
                                             scales = "free_y")
chlPlot

pheoPlot <- ggplot(sfei, aes(x=date_dec, y=pheo))+geom_line()+xlim(2000,2015)
pheoPlot<- pheoPlot + facet_grid(Station ~ .,
                               labeller=label_parsed,
                               scales = "free_y")
pheoPlot

doPlot <- ggplot(sfei, aes(x=date_dec, y=do))+geom_line()+xlim(2000,2015)
doPlot<- doPlot + facet_grid(Station ~ .,
                                 labeller=label_parsed,
                                 scales = "free_y")
doPlot

salPlot <- ggplot(sfei, aes(x=date_dec, y=sal))+geom_line()+xlim(2000,2015)
salPlot<- pheoPlot + facet_grid(Station ~ .,
                                 labeller=label_parsed,
                                 scales = "free_y")
salPlot

tempPlot <- ggplot(sfei, aes(x=date_dec, y=temp))+geom_line()+xlim(2000,2015)
tempPlot<- tempPlot + facet_grid(Station ~ .,
                                 labeller=label_parsed,
                                 scales = "free_y")
tempPlot

## seasonality

chlPlot <- ggplot(sfei, aes(x=doy, y=chl))+geom_point()
chlPlot<- chlPlot + facet_grid(Station ~ .,
                               labeller=label_parsed,
                               scales = "free_y")
chlPlot

pheoPlot <- ggplot(sfei, aes(x=doy, y=pheo))+geom_line()
pheoPlot<- pheoPlot + facet_grid(Station ~ .,
                                 labeller=label_parsed,
                                 scales = "free_y")
pheoPlot

doPlot <- ggplot(sfei, aes(x=doy, y=do))+geom_line()
doPlot<- doPlot + facet_grid(Station ~ .,
                             labeller=label_parsed,
                             scales = "free_y")
doPlot

salPlot <- ggplot(sfei, aes(x=doy, y=sal))+geom_line()
salPlot<- pheoPlot + facet_grid(Station ~ .,
                                labeller=label_parsed,
                                scales = "free_y")
salPlot

tempPlot <- ggplot(sfei, aes(x=doy, y=temp))+geom_line()
tempPlot<- tempPlot + facet_grid(Station ~ .,
                                 labeller=label_parsed,
                                 scales = "free_y")
tempPlot



## stationarity

## differencing
## detrending (fit OLS over time)
## take out seasonality (remove the periodic effect: fit a linear model with indicator variables or fit with periodic function)
## http://www.wright.edu/~thaddeus.tarpey/ES714timeseries.pdf 4.3

acf(sfei[which(sfei$Station=="D10"),"chl"])
pacf(sfei[which(sfei$Station=="D10"),"chl"])


###
spectrum(sfei[which(sfei$Station=="D10"),"chl"]) ## periodogram
ccf(sfei[which(sfei$Station=="D10"),"chl"],sfei[which(sfei$Station=="D12"),"chl"]) ## cross correlation plot
x = ts(cbind(t.one, t.ten))

test=spectrum( cbind(sfei[which(sfei$Station=="D10"),"chl"], sfei[which(sfei$Station=="D12"),"chl"][1:172] ) ) 
test$coh

plot(sfei[which(sfei$Station=="D10"),"chl"][40:70],type="l")
lines(sfei[which(sfei$Station=="D12"),"chl"][40:70],type="l",col="red")


## all are ones, doesn't make any sense
test=spectrum( cbind(sfei[which(sfei$Station=="D10"),"chl"], sfei[which(sfei$Station=="D12"),"pheo"][1:172] ) ) 
test$coh


test=spectrum( cbind(sfei[which(sfei$Station=="D10"),"chl"], rnorm(172))) 
test$coh

test=spectrum( cbind(sfei[which(sfei$Station=="D10"),"chl"], sfei[which(sfei$Station=="D10"),"pheo"])) 
test$coh

test=spectrum( cbind(sfei[which(sfei$Station=="D10"),"chl"][40:70], rnorm(30))) 
test$coh

test=spectrum( cbind(rnorm(30), rnorm(30))) 
test$coh


x.spec = spec.pgram(cbind(sfei[which(sfei$Station=="D10"),"chl"],sfei[which(sfei$Station=="D12"),"chl"][1:172]),taper=.2) 
plot(x.spec, plot.type="coherency")
plot(x.spec, plot.type="phase")

plot(sfei[which(sfei$Station=="D12"),"chl"],type="l")
lines(sfei[which(sfei$Station=="D10"),"chl"],col="red")

par(mfrow=c(2,1))
spec.pgram(sfei[which(sfei$Station=="D12"),"chl"])
spec.pgram(sfei[which(sfei$Station=="D10"),"chl"])


###spectrogram
require(astsa)
data=sfei[which(sfei$Station=="D12"),"chl"]
#EXP6
nobs = length(data) # number of observations 
wsize = 30# window size  ## 8 15
overlap = 5 # overlap ## 2 4
ovr = wsize-overlap 
nseg = floor(nobs/ovr)-1; # number of segments 
krnl = kernel("daniell", c(1,1)) # kernel
ex.spec = matrix(0, wsize/2, nseg) 
for (k in 1:nseg){ 
  a = ovr*(k-1)+1 
b = wsize+ovr*(k-1) 
ex.spec[,k] = mvspec(data[a:b], krnl, taper=.5, plot=FALSE)$spec 
} 
ex.spec

x = seq(0, 10, len = floor(nrow(ex.spec)/2)) 
y = seq(0, ovr*nseg, len = ncol(ex.spec)) 
z = ex.spec[1:(nrow(ex.spec)/2),] # below is text version
filled.contour(x,y,log(z),ylab="time",xlab="frequency (Hz)",nlevels=12 ,col=gray(11:0/11),main="Explosion") 
persp(x,y,z,zlab="Power",xlab="frequency(Hz)",ylab="time",ticktype="detailed",theta=25,d=2,main="Explosion")


specgram(data) ## I think my data is too coarse for this

