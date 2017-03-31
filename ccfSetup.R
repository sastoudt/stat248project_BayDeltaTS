names(D10)

## lag.max 12
test=ccf(D10$residGAMdo,D10$residGAMchl,lag.max=12)
names(test)
test$acf
test$type
test$n.used
test$lag
test$series
test$snames
## only care about positive lags, because I will do ccf(x,y) and ccf(y,x) seperately
ccfOfInterest=test$acf[14:25]
lagOfInterest=test$lag[14:25]
## max absolute correlation per comparison
lagOfInterest[which.max(abs(ccfOfInterest))]
ccfOfInterest[which.max(abs(ccfOfInterest))]

## significance
##http://stackoverflow.com/questions/38173544/how-to-calculate-p-values-from-cross-correlation-function-in-r
## but this is under white noise assumptions right?

nrow(D10)

test1=D10$chl[c(5:172,1:4)]
test2=D10$chl[c(10:172,1:9)]
test3=D10$chl[c(15:172,1:14)]

test=ccf(D10$residGAMdo,D10$residGAMchl,lag.max=12)
ccfOfInterest[which.max(abs(ccfOfInterest))] ##  0.210351
test=ccf(D10$residGAMdo,test1,lag.max=12)
ccfOfInterest=test$acf[14:25]
ccfOfInterest[which.max(abs(ccfOfInterest))] ##  0.1836556

empP<-c()
for(i in 2:nrow(D10) ){
  test1=D10$chl[c((i):172,1:(i-1))]
  
  test=ccf(D10$residGAMdo,test1,lag.max=12)
  ccfOfInterest=test$acf[14:25]
 empP<-c( empP,ccfOfInterest[which.max(abs(ccfOfInterest))] )
  print(i)
}

summary(empP)
hist(empP) ##  0.005847953 smallest p-value
quantile(empP,0.975)
length(which(empP>ccfOfInterest[which.max(abs(ccfOfInterest))]))/172

## recommended by Aaditya Ramdas
#Dørum, Guro; Snipen, Lars; Solheim, Margrete; and Sæbø, Solve (2009) "Rotation Testing in
#Gene Set Enrichment Analysis for Small Direct Comparison Experiments," Statistical
#Applications in Genetics and Molecular Biology: Vol. 8: Iss. 1, Article 34.
#DOI: 10.2202/1544-6115.1418

#Dørum, Guro, et al. "Rotation gene set testing for longitudinal expression data." 
#Biometrical Journal 56.6 (2014): 1055-1075.

## multiple testing

## BY under recommendation from Aaditya Ramdas
p.adjust(p, method = "BY")


## do this at every level
## ccf between all stations/ diff nutrients/waterquality
## acf within all stations diff nutrients/water quality
## partial correlation
## etc.

## but how to incorporate multiple testing across lags in each
## does picking the most extreme one as I am sort of get around this?


## other ideas to look into
## https://www.researchgate.net/post/How_to_calculate_p-value_for_Cross-Correlation_for_two_time_series_with_delay
## http://www.recurrence-plot.tk/programmes.php
## http://bmcstructbiol.biomedcentral.com/articles/10.1186/1472-6807-9-48