## eda of data
setwd("~/UC_Berkeley/Semester_4/timeSeries")
sfei<-read.csv("sfeiDataForProject.csv",stringsAsFactors=F)

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


x.spec = spec.pgram(cbind(sfei[which(sfei$Station=="D10"),"chl"],sfei[which(sfei$Station=="D12"),"chl"][1:172])) 
plot(x.spec, plot.type="coherency")
plot(x.spec, plot.type="phase")
