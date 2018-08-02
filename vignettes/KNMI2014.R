library(raster)
library(data.table)
files.dir <- "/nobackup/users/dirksen/KNMI2014/KNMI14_schetsboeken/"

#knmi2014.files <- list.files(path=files.dir,pattern="tg",full.names=TRUE)
klimaatKNMI <- function(filetxt,varname){
  filetxt<-t(filetxt)
  filetxt<-as.data.frame(filetxt)
  filetxt<-drop(filetxt[2:26,])
  names(filetxt)<-c("RDx","RDy","lat","lon",varname)
  return(filetxt)
}

tg<-fread(paste0(files.dir,"klimaat_KNMI14____ref__tg.av____annual_19810101-20101231_v1.0.txt"),header=TRUE,sep=" ",
          colClasses=rep("character",26))
tg.varname <- "Tg"

Nwarm<-fread(paste0(files.dir,"klimaat_KNMI14____ref__N_warm___annual_19810101-20101231_v1.0.txt"),header=TRUE,sep=" ",
             colClasses=rep("character",26))
Nwarm.varname <- "Nwarm"

Tg <- klimaatKNMI(tg,tg.varname)

Warm_days <- klimaatKNMI(Nwarm,Nwarm.varname)

out<-merge(Tg,Warm_days,all=TRUE)
