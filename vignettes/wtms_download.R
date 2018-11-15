library(rgdal)
library(gdalUtils)

bgt_wfs <- "WFS:https://geodata.nationaalgeoregister.nl/beta/bgt/wfs?request=getcapabilities"

#ogrinfo(bgt_wfs, so=TRUE)

#load pand layer and make shapefile
ogr2ogr(bgt_wfs, "bgt.shp", "bgt:pand")

#plot BGT
bgt <- readOGR("bgt.shp", "bgt", stringsAsFactors=FALSE)

extent(bgt)<-c(-285401.92,595401.92,22598.08,903401.92)

#bbox around the coordinates from the Bilt
library(raster)
library(rgeos)
debilt<-data.frame("lat"=140802.698, "lon"=456881.8)
coordinates(debilt)<-~lat+lon

bbox<-buffer(debilt,width=200)
crs(bbox)<-crs(bgt)
####################

bgt.bilt<-crop(bgt,extent(bbox))
