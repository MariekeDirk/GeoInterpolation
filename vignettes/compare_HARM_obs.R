library(adehabitat)
library(lubridate)
library(data.table)
library(raster)
library(rgdal)
library(SDMTools)

pro=CRS("+init=epsg:28992")

HARM38.full.list<-list.files("./Data/HARMONIE/",
                             pattern=".grd",
                             full.names = T)

HARM38.list<-list.files("./Data/HARMONIE/",pattern=".grd")

HARM38.list.time<-gsub("file","",HARM38.list)
HARM38.list.time<-gsub("T123000Z.grd","",HARM38.list.time)
HARM38.list.time<-as.POSIXct(HARM38.list.time,format="%Y%m%d")

KNMI.stations<-fread("./Data/KNMIstations/TemperatuurDag.txt",
                     sep=",",
                     na.strings ="/////")
KNMI.stations$Datum<-as.character(KNMI.stations$Datum)
KNMI.stations$Datum<-as.POSIXct(KNMI.stations$Datum,format="%Y%m%d")
KNMI.stations<-subset(KNMI.stations,select=c(Datum,Stn,Locatie,RDN_X,RDN_Y,Tg))
KNMI.stations<-KNMI.stations[complete.cases(KNMI.stations),]
coordinates(KNMI.stations)<-~RDN_X+RDN_Y
proj4string(KNMI.stations)<-pro

for (i in 2:length(HARM38.list.time)){
  t<-HARM38.list.time[i]
  obs.subset<-KNMI.stations[which(KNMI.stations$Datum==t),]
  
  r.HARM38<-raster(HARM38.full.list[i]) 
  r.HARM38<-r.HARM38-273.15
  rASC<-asc.from.raster(r.HARM38)
  spdf<-asc2spixdf(rASC)
  proj4string(spdf)<-pro
      
  var.Tg<-subset(obs.subset,select=Tg)
  sat.var<-over(var.Tg,spdf)
      
  n<-names(sat.var)
  diff<-sat.var[n]-var.Tg$Tg
      
      HA38_over_obs<-cbind(as.data.frame(obs.subset),sat.var,diff)
      if (i==2) {
        print(HA38_over_obs)
        names(HA38_over_obs)<-c("Datum","Stn","Locatie","RDN_X","RDN_Y","Tg","HA38","Diff")
      } else {names(HA38_over_obs)<-NULL}
      
      write.table(HA38_over_obs,file="./Rdata/HA38_over_obs.csv",append=TRUE,sep=",",row.names = FALSE)
      print(t)
    }

###
#The temperature difference is stored as Temp_HA38 - Temp_obs = Diff
out<-fread("./Rdata/HA38_over_obs.csv",sep=",")
names(out)<-c("Datum","Stn","Locatie","RDN_X","RDN_Y","Tg","HA38","Diff")
out$Datum<-as.POSIXct(out$Datum)

#data analysis
library(mgcv)
out<-fread("./Rdata/HA38_over_obs.csv")
names(out)<-c("Datum","Stn","Locatie","RDN_X","RDN_Y","Tg","HA38","Diff")
a.df<-data.frame(out$Diff,out$Datum,year(out$Datum),yday(out$Datum),rep(1,length(out$Datum)))
names(a.df)<-c("value","time","year","yday","dummy")
fit1 <- gam(value ~ s(year)+ s(yday, bs = "cc"), data = a.df)
fit2 <- gam(value ~ s(year,bs="ts"), data = a.df)

#plotting routine
plot(a.df$time,a.df$value,
     xlab="time",ylab="Temperature",
     pch=".",
     main=paste0("diff= ",round(mean(a.df$value),2),"\n sd= ",round(sd(a.df$value),2)))
lines(a.df$time,fit1$fitted.values,col="red",lwd=2)
# lines(a.df$time,fit2$fitted.values,col="blue",lwd=2)
