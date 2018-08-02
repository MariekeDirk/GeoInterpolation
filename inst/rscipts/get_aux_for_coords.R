####################################################################################
#create auxiliary dataset for the stations
####################################################################################
data("coords_aws")
coordinates(coords_aws)<-~RDN_X+RDN_Y

start<-as.Date("1990-01-01")
stop<-as.Date("2017-12-31")
all.datums<-seq(start,stop,by="day")
save_output<-"/nobackup/users/dirksen/data/auxcillary_NED/station_overlay/auxiliary_AWS_loc_datum.csv"
# cls<-c("character","numeric","character",rep("numeric",4),"Date",rep("numeric",48))

library(doParallel)
library(doSNOW)
cl<-makeCluster(6)
registerDoParallel(cl)
# clusterExport(cl, list("load_auxiliarydata", "points_over_raster"))
# out<-sapply(all.datums[1:2],FUN=points_over_raster,spdf=coords_aws)

df.out<-foreach(i=1:length(all.datums),.packages = c("GeoInterpolation","raster","sp"),.combine = "rbind") %dopar% {
  print(i)
  df.out<-points_over_raster(datum = all.datums[i],spdf = coords_aws)
  
  # df.out[]<-Map('class<-',df.out,cls)
  fname<-gsub("datum",all.datums[i],save_output)
  
  write.table(df.out,
              sep=",",
              file = fname,
              row.names = FALSE,
              col.names =TRUE)
  
  
  return(TRUE)
  
  
}



stopCluster(cl)

#merge all the results
aux.csv<-list.files("/nobackup/users/dirksen/data/auxcillary_NED/station_overlay/",pattern=".csv",full.names = TRUE)
aux.df.list<-lapply(aux.csv,fread)
aux.df<-Reduce(function(...) rbind(...,fill=TRUE),aux.df.list)


write.table(aux.df,
            sep=",",
            file = "/nobackup/users/dirksen/data/auxcillary_NED/aux_coords_aws.txt",
            row.names = FALSE,
            col.names =TRUE)
####################################################################################
####################################################################################
####################################################################################