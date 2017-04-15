load(file="~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data/delt_map.RData")
plot(delt_map)
points(longitude,latitude,pch=19)
points(sfei$Longitude,sfei$Latitude,col="red",cex=2)

setwd("~/UC_Berkeley/Semester_4/timeSeries")
D10<-read.csv("D10data.csv",stringsAsFactors=F)
D12<-read.csv("D12data.csv",stringsAsFactors=F)
D22<-read.csv("D22data.csv",stringsAsFactors=F)
D26<-read.csv("D26data.csv",stringsAsFactors=F)
D4<-read.csv("D4data.csv",stringsAsFactors=F)
stationNames<-c("D10","D12","D22","D26","D4")

D10=cbind(D10,rep("D10",nrow(D10)))
D12=cbind(D12,rep("D12",nrow(D11)))
D22=cbind(D22,rep("D22",nrow(D22)))
D26=cbind(D26,rep("D26",nrow(D26)))
D4=cbind(D4,rep("D4",nrow(D4)))

D10n=D10[,c(1:13,ncol(D10))]
D12n=D12[,c(1:13,ncol(D12))]
D22n=D22[,c(1:13,ncol(D22))]
D26n=D26[,c(1:13,ncol(D26))]
D4n=D4[,c(1:13,ncol(D4))]

names(D10n)[14]=names(D12n)[14]=names(D22n)[14]=names(D26n)[14]=names(D4n)[14]="station"

D10[,14]=as.character(D10[,14])
D12[,14]=as.character(D12[,14])
D22[,14]=as.character(D22[,14])
D26[,14]=as.character(D26[,14])
D4[,14]=as.character(D4[,14])

sfei=rbind(D10n, D12n, D22n, D26n, D4n)

require(ggplot2)
g1=ggplot(sfei, aes(x=date_dec, y=chl, colour=as.factor(Station)))+geom_line()+xlim(2000,2015)+ labs(colour = "Station")+xlab("Date")
g2=ggplot(sfei, aes(x=date_dec, y=pheo, colour=as.factor(Station)))+geom_line()+xlim(2000,2015)+ labs(colour = "Station") +xlab("Date")
g3=ggplot(sfei, aes(x=date_dec, y=temp, colour=as.factor(Station)))+geom_line()+xlim(2000,2015)+ labs(colour = "Station") +xlab("Date")
g4=ggplot(sfei, aes(x=date_dec, y=sal, colour=as.factor(Station)))+geom_line()+xlim(2000,2015)+ labs(colour = "Station") +xlab("Date")
g5=ggplot(sfei, aes(x=date_dec, y=do, colour=as.factor(Station)))+geom_line()+xlim(2000,2015)+ labs(colour = "Station") +xlab("Date")


library(gridExtra)
grid.arrange(g1,g2,g3,g4,g5, ncol=1, nrow =5)







