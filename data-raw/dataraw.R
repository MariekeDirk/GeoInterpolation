library(data.table)
library(dplyr)
library(raster)
library(config)

cfg <- config::get(file = "/nobackup/users/dirksen/Temperature/GeoInterpolation/config/config.yml")

#solar radiation
solar_radiation <- fread(cfg$nlradiation)
names(solar_radiation)<-c("Datum","DS_CODE","Q","RDN_X","RDN_Y","SAT","DIFF") 
solar_radiation<-as.data.frame(solar_radiation)
devtools::use_data(solar_radiation,overwrite=TRUE)

#temperature climate
temperature_climate<-fread("/nobackup/users/dirksen/data/Temperature/KNMIstations/KNMI_20170911.txt",na.strings = "",skip = 61)
names(temperature_climate)<-c("STN","Datum","Tg")
temperature_climate<-temperature_climate[complete.cases(temperature_climate$Tg),]
temperature_climate$Tg<-temperature_climate$Tg/10
temperature_climate$Datum<-as.Date(as.character(temperature_climate$Datum),format="%Y%m%d")
temperature_climate$STN<-as.character(temperature_climate$STN)
temperature_climate$Tg<-as.numeric(temperature_climate$Tg)
devtools::use_data(temperature_climate,overwrite=TRUE)

#Temperature minimum and maximum
# /nobackup/users/dirksen/data/Temperature/KNMIstations/TN_TX_1990_2017.csv
temperature_min_max<-fread("/nobackup/users/dirksen/data/Temperature/KNMIstations/temperature_min_max.csv")
temperature_min_max$Datum<-as.Date(as.character(temperature_min_max$IT_DATETIME),format = "%Y%m%d_240000_000000")
temperature_min_max<-subset(temperature_min_max,select=c("Datum","DS_CODE","REH1.TN","REH1.TX"))
names(temperature_min_max)<-c("Datum","DS_CODE","TN","TX")
temperature_min_max$STN<-gsub("_H","",temperature_min_max$DS_CODE)
temperature_min_max$TX<-as.numeric(temperature_min_max$TX)
temperature_min_max$TN<-as.numeric(temperature_min_max$TN)
temperature_min_max<-temperature_min_max[complete.cases(temperature_min_max),]
temperature_min_max$DTR<-temperature_min_max$TX-temperature_min_max$TN
devtools::use_data(temperature_min_max,overwrite = TRUE)

#coords aws
coords_aws<-fread(paste0(cfg$datapath,cfg$nlstations))
coords_aws$STN<-gsub(":","",coords_aws$STN)
coords_aws$LAT<-as.numeric(coords_aws$LAT)
coords_aws$LON<-as.numeric(coords_aws$LON)
coordinates(coords_aws)<-~LON+LAT
projection(coords_aws)<-cfg$WGS84
rd<-spTransform(coords_aws,cfg$pro)
rdcoords<-coordinates(rd)
coords_aws<-data.frame(rd,rdcoords)
names(coords_aws)<-c("STN" , "DS_ALT",  "DS_NAME"  ,"DS_LON"  , "DS_LAT","optional"  , "RDN_X"  , "RDN_Y")
devtools::use_data(coords_aws,overwrite=TRUE)

#DISTSEA grid
distsea.grid<-read.asciigrid("/nobackup/users/dirksen/data/auxcillary_NED/distsea/wn_distshore_001.asc")
names(distsea.grid)<-"distsea"
devtools::use_data(distsea.grid,overwrite = TRUE)

#HARMONIE files masked
r.harm.files<-list.files(paste0(cfg$datapath,cfg$HARMONIEdata),pattern=".grd",full.names = TRUE)
st.all<-stack(r.harm.files)

mask_buffer<-read.asciigrid("/nobackup/users/dirksen/data/Temperature/KNMIstations/wn_distshore_001.asc")


gridded(mask_buffer)<-TRUE
proj4string(mask_buffer)<-proj4string(st.all)
mask_buffer<-raster(mask_buffer)
extent(mask_buffer)<-extent(st.all)
mask_buffer<-resample(mask_buffer,st.all)

for(i in 1:length(r.harm.files)){
  st.mask<-mask(st.all[[i]],mask_buffer)
  writeRaster(st.mask,filename = paste0("/nobackup/users/dirksen/data/Temperature/HARMONIE_mask/",st.mask@data@names,".grd"),overwrite=TRUE)
}

#Wunderground metadata
wunderground_meta<-fread("/nobackup/users/dirksen/data/wunderground/meta_data_start_stop.txt")
wunderground_meta<-data.frame(wunderground_meta)
wunderground_meta$start<-as.Date(wunderground_meta$start)
wunderground_meta$stop<-as.Date(wunderground_meta$stop)
devtools::use_data(wunderground_meta, overwrite = TRUE)

#spatial data from which we want to extract patterns
library(raster)
rad_eu<-list.files(path = "/nobackup/users/dirksen/data/CERES_SARAH_mosaic/", 
                   pattern = ".grd",
                   full.names = TRUE)

# t.rad_eu<-as.Date(list.files(path = "/nobackup/users/dirksen/data/CERES_SARAH_mosaic/", 
                             # pattern = ".grd"),format = "%Y-%m-%d.grd")

europe.mask<-stack("/nobackup/users/dirksen/data/radiation_europe/DEM/gmted2010_gis_1km.grd")

#masking the raster images
st<-stack(rad_eu) 

europe.mask<-resample(europe.mask[[1]],st[[1]])
st<-crop(st,europe.mask)
st.mask<-mask(st,europe.mask)

devtools::use_data(st.mask,overwrite = TRUE)

