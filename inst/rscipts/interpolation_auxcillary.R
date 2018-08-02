library(raster)
library(rgdal)
library(maptools)

cfg <- config::get(file = "/nobackup/users/dirksen/Temperature/GeoInterpolation/config/config.yml")

correctQGIS_population_grid<-function(population){
  population.new<-flip(population,direction='y')
  extent(population.new)<-extent(r.distsea)
  population.new<-projectRaster(population.new,r.distsea)
  population.new<-mask(population.new,r.distsea)
  return(population.new)
}

correctQGIS_height_grid<-function(height){
  height.new<-flip(height,direction='y')
  extent(height.new)<-extent(r.distsea)
  height.new<-projectRaster(height.new,r.distsea)
  height.new<-mask(height.new,r.distsea)
  names(height.new)<-"Height"
  return(height.new)
}

# population<-stack("/nobackup/users/dirksen/data/auxcillary_NED/population/grid/INW_2001_2014.grd")
distsea<-read.asciigrid("/nobackup/users/dirksen/data/auxcillary_NED/distsea/wn_distshore_001.asc")
r.distsea<-raster(distsea)
proj4string(r.distsea)<-cfg$pro
r.distsea.wgs<-projectRaster(r.distsea,crs=cfg$WGS84)
names(r.distsea)<-"distsea"

pop.names<-list.files("/nobackup/users/dirksen/data/auxcillary_NED/population/grid/QGIS_layers/",pattern=".asc$")
pop.names<-gsub(".asc","",pop.names)
pop.grids<-list.files("/nobackup/users/dirksen/data/auxcillary_NED/population/grid/QGIS_layers/",pattern=".asc$",full.names = TRUE)
pop.grids<-lapply(pop.grids,stack)

crs(pop.grids[[1]])<-crs(pop.grids[[2]])

pop.grids.new<-lapply(pop.grids,correctQGIS_population_grid)
st.pop<-stack(pop.grids.new)
names(st.pop)<-pop.names

height<-raster("/nobackup/users/dirksen/data/auxcillary_NED/height/QGIS_layers/height.grd")
height.new<-correctQGIS_height_grid(height)

st<-stack(r.distsea,st.pop,height.new,r.no.snow.mean)
st.preprocess<-scale(st)
# writeRaster(st,"/nobackup/users/dirksen/data/auxcillary_NED/auxcillary_stacks/raw.grd",overwrite=TRUE)
# writeRaster(st.preprocess,"/nobackup/users/dirksen/data/auxcillary_NED/auxcillary_stacks/center_scale.grd",overwrite=TRUE)


#Insulation SICCS
#net_loc<-"/net/pc150398/nobackup_1/users/meirink/wouter_greuell/SICCS/daymean/harmonie_proj/"
grids_siccs<-stack(list.files("/nobackup/users/dirksen/data/radiationSICCSgroundobs/Satellite_data/temp",pattern=".grd",full.names = TRUE))
dates_siccs<-as.Date(gsub(".grd","",
                          list.files("/nobackup/users/dirksen/data/radiationSICCSgroundobs/Satellite_data/temp",pattern=".grd")),
                     format="%Y%m%d")
#last date is now 2016-01-19
for(i in 1:length(dates_siccs)){
print(dates_siccs[i])
grids_siccs.crop<-crop(grids_siccs[[i]],extent(r.distsea))
grids_siccs.crop<-resample(grids_siccs.crop,r.distsea,"bilinear")
grids_siccs.mask<-mask(grids_siccs.crop,r.distsea)
names(grids_siccs.mask)<-dates_siccs[i]
writeRaster(grids_siccs.mask,
            filename = paste0("/nobackup/users/dirksen/data/auxcillary_NED/insulation_SICCS/insulation_SICCS_",dates_siccs[i],".grd"),
            overwrite=TRUE)
}
# New year data SICCS
library(h5)
library(rhdf5)

#h5
siccs.h5<-list.files("/nobackup/users/dirksen/data/SICCS/2016/",full.names = TRUE)
siccs.dates.h5<-as.Date(list.files("/nobackup/users/dirksen/data/SICCS/2016/",full.names = FALSE),format = "daymean_reproj_%Y%m%d.h5")
save_file_names<-paste0("/nobackup/users/dirksen/data/auxcillary_NED/insulation_SICCS/insulation_SICCS_",siccs.dates.h5,".grd")

h5ls(siccs.h5[1])

out<-mapply(SICCS_preprocess,file_loc=siccs.h5,write_raster=TRUE,save_dir=save_file_names,file.format="h5")

#hdf5
y<-"2016"
m<-c("10","11","12")

m<-m[3]
siccs.hdf5<-list.files(paste0("/nobackup/users/dirksen/data/SICCS/hdf5/",y,"/",m),full.names = TRUE)
siccs.days.hdf5<-list.files(paste0("/nobackup/users/dirksen/data/SICCS/hdf5/",y,"/",m,"/"),full.names = FALSE)
siccs.days.hdf5<-gsub("daymean","",siccs.days.hdf5)
siccs.days.hdf5<-gsub("EURO4M.hdf5","",siccs.days.hdf5)
siccs.dates.hdf5<-as.Date(paste(y,m,siccs.days.hdf5,sep="-"))

save_file_names<-paste0("/nobackup/users/dirksen/data/auxcillary_NED/insulation_SICCS/insulation_SICCS_",siccs.dates.hdf5,".grd")

out<-mapply(SICCS_preprocess,file_loc=siccs.hdf5,write_raster=TRUE,save_dir=save_file_names,file.format="hdf5")

#r<-flip(r,direction = "y")

plot(r)
#Insulation SARAH
grids_sarah<-list.files("/nobackup/users/dirksen/data/SARAH_raster/",pattern=".grd",full.names = TRUE)
grids_dates_sarah<-as.Date(list.files("/nobackup/users/dirksen/data/SARAH_raster/",pattern=".grd",full.names = FALSE),
                           format = "raster_%Y-%m-%d.grd")
I.1990_1995<-which(grids_dates_sarah>="1990-01-01" & grids_dates_sarah<"1995-01-01")

for(i in 1:length(I.1990_1995)){
  st<-stack(grids_sarah[I.1990_1995[i]])
  st<-crop(st,extent(r.distsea.wgs))
  st.ned<-projectRaster(st,crs=cfg$pro)
  st.ned<-resample(st.ned,r.distsea,"bilinear")
  st.ned.mask<-mask(st.ned,r.distsea)
  writeRaster(st.ned.mask,filename = paste0("/nobackup/users/dirksen/data/auxcillary_NED/insolation/sat_",
                                            grids_dates_sarah[I.1990_1995[i]],".grd"))
}

#Monthly climatology to fill the missing data
sarah_dates<-as.Date(list.files("/nobackup/users/dirksen/data/auxcillary_NED/insolation/",pattern=".grd"),format="sat_%Y-%m-%d.grd")
sarah_grids<-list.files("/nobackup/users/dirksen/data/auxcillary_NED/insolation/",pattern=".grd",full.names = TRUE)
s.m<-unique(month(sarah_dates))
s.d<-unique(mday(sarah_dates))

I<-which(month(sarah_dates)==12 & mday(sarah_dates)==22 | mday(sarah_dates)==21)
dec21<-stack(sarah_grids[I])
dec21<-stackApply(dec21,1,fun=mean)

library(rasterVis)
spplot(dec21,col.regions=terrain.colors(n=50))


for(i in 1:length(s.m)){
I<-which(month(sarah_dates)==s.m[i])
st<-stack(sarah_grids[I])
st<-stackApply(st,1,fun=mean)
writeRaster(st,paste0("/nobackup/users/dirksen/data/auxcillary_NED/insulation_monthly_climatology/",s.m[i],"_month.grd"),overwrite=TRUE)
}

#Roughness CORINE
summer<-raster("/nobackup/users/dirksen/data/auxcillary_NED/roughness/roughness_summer.xyz",crs=cfg$WGS84)
winter<-raster("/nobackup/users/dirksen/data/auxcillary_NED/roughness/roughness_winter.xyz",crs=cfg$WGS84)

values(summer)[values(summer)==-1]<-NA
values(winter)[values(winter)==-1]<-NA

roughness<-stack(summer,winter)
roughness<-crop(roughness,extent(r.distsea.wgs))
roughness<-projectRaster(roughness,crs=cfg$pro)
roughness<-resample(roughness,r.distsea,"bilinear")
roughness<-mask(roughness,r.distsea)
spplot(roughness)

writeRaster(roughness,"/nobackup/users/dirksen/data/auxcillary_NED/roughness/roughness_summer_winter.grd")
####################################################################
#MODIS ALBEDO
####################################################################
# Zoek eerst het goede dagnummer 'jdxxx' (jd001 geldt voor dag 1 t/m 16
#                                         enz.). Lees dan 'srefl_vis' en 'srefl_nir' in voor het gebied dat je
# nodig hebt. N.B. het grid staat niet in de file maar loopt van 90N naar
# 90S en van 180W naar 180E met gelijke lat-lon afstanden van 0.05 graden.
# 
# De weging van deze twee moet gebeuren op basis van inkomende straling
# aan het oppervlak in UV/VIS ten opzichte van nabij-infrarood. Deze
# varieert maar is ongeveer gelijk verdeeld. Gebruik dus:
#   
#   srefl = 0.5*srefl_vis + 0.5*srefl_nir
####################################################################
# library(h5) #CRAN package

#package from bioconductor
# source("http://bioconductor.org/biocLite.R")
# biocLite("rhdf5")
library(rhdf5)
file_loc<-"/net/pc150398/nobackup/users/meirink/msg_data/lut/cpp_anc_clim.v20120229.h5"
file_albedo_hdf<-h5ls(file_loc)

albedo_data<-h5read(file_loc,"/Surface_Reflectance/MODIS_Filled")

Njd<-names(albedo_data)

# for(i in 1:length(Njd)){
# print(Njd[i])
# srefl = 0.5*albedo_data[[Njd[i]]]$srefl_vis + 0.5*albedo_data[[Njd[i]]]$srefl_nir
# 
# r.modis<-raster(x=t(srefl),xmn=-180, xmx=180, ymn=-90, ymx=90,crs=cfg$WGS84)
# 
# writeRaster(r.modis,filename = paste0("/nobackup/users/dirksen/data/MODIS/MODIS_albedo_",
#                                                Njd[i],".grd"))
# }

st.modis<-stack(list.files("/nobackup/users/dirksen/data/albedo_modis/MODIS_with_snow/",pattern = ".grd",full.names = TRUE))
st.modis.crop<-crop(st.modis,extent(r.distsea.wgs))

r.modis.ned<-projectRaster(st.modis.crop,crs=cfg$pro)
r.modis.ned<-resample(r.modis.ned,r.distsea,"bilinear")
r.modis.ned.mask<-mask(r.modis.ned,r.distsea)
names(r.modis.ned.mask)<-Njd
# writeRaster(r.modis.ned.mask,filename = paste0("/nobackup/users/dirksen/data/auxcillary_NED/albedo/MODIS_albedo_jd.grd"))

#MODIS without snow
Njd<-seq(1,365,by=16)

modis_vis<-list.files("/nobackup/users/dirksen/data/albedo_modis/Moody_clim/0.3_0.7.um.00-04.WS.c004.v2.0/",pattern = "*.h5",full.names = TRUE)
modis_nir<-list.files("/nobackup/users/dirksen/data/albedo_modis/Moody_clim/0.7_5.0.um.00-04.WS.c004.v2.0/",pattern = "*.h5",full.names = TRUE)

for(i in 1:length(Njd)){
print(Njd[i])
VIS<-h5read(modis_vis[i],"Albedo_Map_0.3_0.7")
NIR<-h5read(modis_nir[i],"Albedo_Map_0.7_5.0")

VIS[VIS==32767]<-NA #replace NA values
NIR[NIR==32767]<-NA

VIS<-VIS*0.001 #scaling factor
NIR<-NIR*0.001
srefl<-0.5*VIS+0.5*NIR

r.mod<-raster(x=t(srefl),xmn=-180, xmx=180, ymn=-90, ymx=90,crs=cfg$WGS84)
r.mod<-crop(r.mod,extent(r.distsea.wgs))
r.mod.ned<-projectRaster(r.mod,crs=cfg$pro)
r.mod.ned<-resample(r.mod.ned,r.distsea,"bilinear")
r.mod.ned<-mask(r.mod.ned,r.distsea)
names(r.mod.ned)<-Njd[i]
writeRaster(r.mod.ned,filename = paste0("/nobackup/users/dirksen/data/auxcillary_NED/albedo/modis_no_snow/MODIS_",Njd[i],".grd"), 
            overwrite=TRUE)
}
#see https://modis-images.gsfc.nasa.gov/ALBEDO/AlbMap.WS.CDL.fs for details (scale factor==0.001)
####################################################################
####################################################################
####################################################################

library(rasterVis)
I<-c(1,16,17,18)
levelplot(st.preprocess[[I]],
          layout=c(2,2),
          scales=list(draw=FALSE ))
#################################FROM HEIGHT contours to DISTSEA grid
# height<-readOGR("/nobackup/users/dirksen/data/auxcillary_NED/height")
# crs(distsea)<-crs(height)
# H<-grep("hoogte",names(height)) #tweede is alleen hoogte
# hoogte<-H[2]
# r<-rasterize(height,r.distsea,"hoogte",fun=mean)

#https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
install.packages("elevatr")
library(elevatr)
cat("mapzen_key=mapzen-sjucJkP\n", file = file.path(normalizePath("~/"), ".Renviron"), 
    append = TRUE)

height<-get_elev_raster(r.distsea,src = "aws",z=9)
writeRaster(r,filename="/nobackup/users/dirksen/data/auxcillary_NED/height/grid/height.grd",overwrite=TRUE)

#################################FROM SVF grid to DISTSEA grid

#this file is missing parts of zeeland!
# r<-raster("/nobackup/users/dirksen/data/auxcillary_NED/svf/svf_100m_0.5.grd")
# r<-aggregate(r,fact=10)

#################################FROM POPULATION SPDF TO RASTER GRID LIKE DISTSEA
population<-readOGR("/nobackup/users/dirksen/data/auxcillary_NED/population/",
                    layer="CBSvierkant100m201410")
# crs(distsea)<-crs(population)

I<-grep("INW",names(population))
layernames<-names(population)[I]

for(i in 1:length(I)){
layername<-layernames[i]
print(layername)
population@data[population@data[layername] == "-99998",layername]<-0
}

writeOGR(obj = population,
         dsn = "/nobackup/users/dirksen/data/auxcillary_NED/population/modified_rstudio/",
         layer= "INW2000",
         driver="ESRI Shapefile")
# 
# for(i in 1:length(I)){
# layername<-layernames[i]
# r<-rasterize(population,r.distsea,layername,fun=mean)
# writeRaster(r,filename=paste0("/nobackup/users/dirksen/data/auxcillary_NED/population/grid/",layername,".grd"),overwrite=TRUE)
# }
# 
# rls<-list.files("/nobackup/users/dirksen/data/auxcillary_NED/population/grid/",pattern=".grd",full.names = TRUE)
# st<-stack(rls[2:15])
# st_mask<-mask(st,r.distsea)
# writeRaster(st_mask,filename = "/nobackup/users/dirksen/data/auxcillary_NED/population/grid/INW_2001_2014.grd",overwrite=TRUE )
################################################OLD CODE########################################################
# r.pop2000<-rasterize(population,r.distsea,"INW2000") #does not work!

# spdf.distsea<-as(distsea,"SpatialPointsDataFrame")
# df.pop2000<-over(spdf.distsea,population["INW2000"]) # returns a data frame with corresponding id
# distsea.new<-spCbind(spdf.distsea,df.pop2000)
# 
# r.distsea.new<-raster(distsea.new)
# r <- rasterFromXYZ(as.data.frame(distsea.new)[, c("s1", "s2", "INW2000")])
# r.pop2000<-aggregate(population["INW2000"],distsea,FUN="mean") # in addition to over, returning a sp-object
# r.pop2000<-extract(r.distsea,population["INW2000"],FUN="mean")
#################################################################################################################

#################PET AND ARIDITY################################
library(rgdal)

m<-seq(1,12,by=1)


dir_to_write<-paste0("/nobackup/users/dirksen/data/auxcillary_NED/PET/pet_",m,".grd")
file_loc<-paste0("/nobackup/users/dirksen/data/PET/PET_he_monthly/pet_he_",m)

aridity<-"/nobackup/users/dirksen/data/Aridity/AI_annual/ai_yr"
aridity_new<-"/nobackup/users/dirksen/data/auxcillary_NED/aridity/aridity.grd"

adf_to_croped_raster<-function(file_loc,dir_to_write){
x<-new("GDALReadOnlyDataset",file_loc)
xx<-asSGDF_GROD(x)
r<-raster(xx)
st<-crop(r,extent(r.distsea.wgs))
st.ned<-projectRaster(st,crs=cfg$pro)
st.ned<-resample(st.ned,r.distsea,"bilinear")
st.ned.mask<-mask(st.ned,r.distsea)
writeRaster(st.ned.mask,filename = dir_to_write,overwrite=TRUE)
print("writen raster")
return(st.ned.mask)
}

st<-mapply(adf_to_croped_raster,file_loc=file_loc,dir_to_write=dir_to_write)
#The aridity can be calculated as the mean PET/mean Precipitation 

#ai<-adf_to_croped_raster(file_loc = aridity,dir_to_write = aridity_new) # This file is missing some islands and the top of noord-holland
####################################################
library(lubridate)
library(ncdf4)
#Precipitation data
ftp<-"ftp://data.knmi.nl/download/Rd1/5/0005/"

path.all<-"/nobackup/users/dirksen/data/auxcillary_NED/precipitation/data.knmi.nl/download/Rd1/5/0005"
path.sub<-"/nobackup/users/dirksen/data/auxcillary_NED/precipitation/test/data.knmi.nl/download/Rd1/5/0005"
precip<-list.files(path=path.sub,
                   recursive = TRUE,
                   full.names = TRUE)
precip.names<-list.files(path=path.sub,
                   recursive = TRUE,
                   full.names = FALSE)
precip.dates<-as.Date(gsub("/INTER.*","",precip.names),format = "%Y/%m/%d")
Ip<-which(precip.dates>"1990-01-01")

precip<-precip[Ip]
summary(precip.dates[Ip])

nc4_to_raster<-function(nc4file,lat,lon,var,name){
  ncin<-nc_open(nc4file)
  lat<-ncvar_get(ncin,lat)
  lon<-ncvar_get(ncin,lon)
  var<-ncvar_get(ncin,var)
  
  r<-raster(t(var))
  extent(r)<-c(min(lat),max(lat),min(lon),max(lon))
  r<-flip(r,direction='y')
  crs(r)<-cfg$pro
  writeRaster(r,filename = name ,overwrite=TRUE)
  nc_close(ncin)
  return(r)
}

name<-paste0("/nobackup/users/dirksen/data/auxcillary_NED/precipitation/grid/precip_",precip.dates[Ip],".grd")
lat<-rep("x",length(name))
lon<-rep("y",length(name))
var<-rep("prediction",length(name))
ncfile<-precip

for(i in 1:length(name)){
  nc4_to_raster(nc4file = ncfile[i],
                lat = lat[i],
                lon = lon[i],
                var = var[i],
                name = name[i])
  print(name[i])
  print(i)
}

# ncin<-nc_open("/nobackup/users/dirksen/data/auxcillary_NED/precipitation/data.knmi.nl/download/Rd1/5/0005/2003/01/05/INTER_OPER_R___RD1_____L3__20030105T080000_20030106T080000_0005.nc")

#error files
#2384
#/nobackup/users/dirksen/data/auxcillary_NED/precipitation/data.knmi.nl/download/Rd1/5/0005/1996/07/12/INTER_OPER_R___RD1_____L3__19960712T080000_19960713T080000_0005.nc
#4752
#/nobackup/users/dirksen/data/auxcillary_NED/precipitation/data.knmi.nl/download/Rd1/5/0005/2003/01/05/INTER_OPER_R___RD1_____L3__20030105T080000_20030106T080000_0005.nc
#7902
#/nobackup/users/dirksen/data/auxcillary_NED/precipitation/data.knmi.nl/download/Rd1/5/0005/2011/08/21/INTER_OPER_R___RD1_____L3__20110821T080000_20110822T080000_0005.nc
#8094
#/nobackup/users/dirksen/data/auxcillary_NED/precipitation/data.knmi.nl/download/Rd1/5/0005/2012/02/29/INTER_OPER_R___RD1_____L3__20120229T080000_20120301T080000_0005.nc
#8466
#/nobackup/users/dirksen/data/auxcillary_NED/precipitation/data.knmi.nl/download/Rd1/5/0005/2013/03/07/INTER_OPER_R___RD1_____L3__20130307T080000_20130308T080000_0005.nc
#9616
#/nobackup/users/dirksen/data/auxcillary_NED/precipitation/data.knmi.nl/download/Rd1/5/0005/2016/04/30/INTER_OPER_R___RD1_____L3__20160430T080000_20160501T080000_0005.nc
#9827: kills rsession
#/nobackup/users/dirksen/data/auxcillary_NED/precipitation/data.knmi.nl/download/Rd1/5/0005/2016/11/27/INTER_OPER_R___RD1_____L3__20161127T080000_20161128T080000_0005.nc

# mapply(nc4_to_raster,nc4file=ncfile,var=var,lat=lat,lon=lon,name=name)
# 
grids<-list.files("/nobackup/users/dirksen/data/auxcillary_NED/precipitation/grid",pattern=".grd",full.names = TRUE)
grids.dates<-as.Date(list.files("/nobackup/users/dirksen/data/auxcillary_NED/precipitation/grid",
                        pattern = ".grd"),format="precip_%Y-%m-%d.grd")

m<-unique(month(grids.dates))
for(i in 1:12){
  mon<-m[i]
  print(mon)
  I<-which(month(grids.dates)==mon)
  st<-stack(grids[I])
  st<-stackApply(st,1,fun="mean")
  crs(st)<-cfg$pro
  st<-crop(st,extent(r.distsea))

  st<-resample(st,r.distsea,"bilinear")
  st.mask<-mask(st,r.distsea)
  # writeRaster(st.mask,
  #             filename = paste0("/nobackup/users/dirksen/data/auxcillary_NED/precipitation/monthly_clim/precip_",m[i],".grd"),
  #             overwrite=TRUE)
  print("writen raster")

}

st.clim<-stackApply(stack(grids),1,fun="mean")
crs(st.clim)<-cfg$pro
st.clim<-crop(st.clim,extent(r.distsea))

st.clim<-resample(st.clim,r.distsea,"bilinear")
st.clim<-mask(st.clim,r.distsea)

# writeRaster(st.clim,
#             filename = paste0("/nobackup/users/dirksen/data/auxcillary_NED/precipitation/clim_precip.grd"),
#             overwrite=TRUE)

##############MODIS NDVI monthly mean
#https://cran.r-project.org/web/packages/MODIS/MODIS.pdf
library(MODIS)
library(raster)
library(RCurl)

path<-"/nobackup/users/dirksen/data/NDVI/e4ftl01.cr.usgs.gov/"
vi<-preStack(path = path)

#https://gis.stackexchange.com/questions/237272/mean-by-month-on-r-stacked-raster
## download and extract required layers
runGdal("MOD13A3", collection = getCollection("MOD13A3", forceCheck = TRUE),
        begin = "2000001", end = "2016366", extent = "Netherlands",
        job = "temporalComposite", SDSstring = "100000000010")

# ## import ndvi
ndvi <- list.files(paste0(getOption("MODIS_outDirPath"), "/temporalComposite"),
                   pattern = "NDVI.tif", full.names = TRUE)
# 
# ## import corresponding composite day of the year
# cdoy <- list.files(paste0(getOption("MODIS_outDirPath"), "/temporalComposite"),
#                    pattern = "reliability.tif", full.names = TRUE)
# 
# ## create monthly mean value composites
# mmvc <- temporalComposite(ndvi, 
#                           cdoy,
#                           fun = "mean")
# 
# plot(mmvc[[1:3]] / 10000, zlim = c(-.1, .95))

#https://conservationecology.wordpress.com/2014/08/11/bulk-downloading-and-analysing-modis-data-in-r/
vi<-preStack(path="/nobackup/users/dirksen/data/MODIS/PROCESSED//temporalComposite/",pattern="NDVI.tif")
st<-stack(vi)

st.clim<-stackApply(st,1,fun=mean)
st.clim<-crop(st.clim,extent(r.distsea.wgs))
st.ned<-projectRaster(st.clim,crs=cfg$pro)
st.ned<-resample(st.ned,r.distsea,"bilinear")
st.ned.mask<-mask(st.ned,r.distsea)
st.ned.mask<-scale(st.ned.mask)
writeRaster(st.ned.mask,"/nobackup/users/dirksen/data/auxcillary_NED/NDVI/modis_ndvi_clim.grd",overwrite=TRUE)

library(lubridate)
dates.ndvi<-gsub("MOD13A3.A","",names(st))
dates.ndvi<-gsub(".1_km_monthly_NDVI","",dates.ndvi)
dates.ndvi<-strptime(dates.ndvi,format="%Y%j")

m<-month(dates.ndvi)
m.u<-unique(month(dates.ndvi))

for(i in 1:length(m.u)){
  print(m.u[i])
  I<-which(m==m.u[i])
  st.month<-st[[I]]
  st.mean<-stackApply(st.month,1,fun=mean)
  st.mean<-crop(st.mean,extent(r.distsea.wgs))
  st.ned<-projectRaster(st.mean,crs=cfg$pro)
  st.ned<-resample(st.ned,r.distsea,"bilinear")
  st.ned.mask<-mask(st.ned,r.distsea)
  
  st.ned.mask<-scale(st.ned.mask)
  
  
  writeRaster(st.ned.mask,
              paste0("/nobackup/users/dirksen/data/auxcillary_NED/NDVI/MODIS_clim_ndvi_",m.u[i],".grd"),
              overwrite=TRUE)
}
# st<-st/10000

#Snow depth
loc<-"/nobackup/users/dirksen/data/auxcillary_NED/snow_cover/station_data/ECA_blend_sd/"
library(data.table)
library(stringr)
library(plyr)
stations<-fread("/nobackup/users/dirksen/data/auxcillary_NED/snow_cover/station_data/ECA_blend_sd/stations.txt",
                skip=17)
stations[["LAT"]]<-gsub(":"," ",stations[["LAT"]])
stations[["LON"]]<-gsub(":"," ",stations[["LON"]])
stations$LON<-measurements::conv_unit(stations[["LON"]],from='deg_min_sec',to='dec_deg')
stations$LAT<-measurements::conv_unit(stations[["LAT"]],from='deg_min_sec',to='dec_deg')

nl.stations<-stations$STAID[which(stations$CN=="NL")]
padded.nr<-str_pad(nl.stations, 6, pad = "0")
nl.names<-paste0("SD_STAID",padded.nr,".txt")

ls.names<-list.files(loc,pattern="SD_STAID*")

nl.available<-intersect(nl.names,ls.names)


nl.data<-mapply(fread,paste0(loc,nl.available),na.strings = "-9999",SIMPLIFY = FALSE)

all.data<-Reduce(function(...) rbind(...), nl.data)
all.data$DATE<-as.Date(as.character(all.data$DATE),format="%Y%m%d")
all.data<-data.frame(all.data)
all.data<-join(all.data,stations,type="inner")

saveRDS(all.data,"/nobackup/users/dirksen/data/auxcillary_NED/snow_cover/station_data/rds/nlstations.rds")


snow1990_2017<-all.data[which(all.data$DATE >= "1990-01-01"),]
snow1990_2017<-snow1990_2017[complete.cases(snow1990_2017),]
snow1990_2017$LAT<-as.numeric(snow1990_2017$LAT)
snow1990_2017$LON<-as.numeric(snow1990_2017$LON)

int.days<-unique(snow1990_2017$DATE)

for(i in 1:length(int.days)){
  
input.snow<-snow1990_2017[which(snow1990_2017$DATE==int.days[i]),]

if(sum(input.snow$SD!=0)){
print(paste0("snow on ",int.days[i]))  
coordinates(input.snow)<-~LON+LAT
proj4string(input.snow)<-cfg$WGS84
input.snow<-spTransform(input.snow,CRSobj = cfg$pro)

#write a tps interpolation for all days (1990-2017)
crs(distsea)<-cfg$pro
int.snow<-interpolation_tps(input.snow,variable = "SD",distshore.grd = distsea)

r<-raster(int.snow$spatial,layer=2)
values(r)[values(r)<0]=0
r<-mask(r,r.distsea)
writeRaster(r,paste0("/nobackup/users/dirksen/data/auxcillary_NED/snow_cover/gridded_product/tps/",
                                    int.days[i],
                                    ".grd"),
                                    overwrite=TRUE)
write.table(data.frame(int.snow$cv),paste0("/nobackup/users/dirksen/data/auxcillary_NED/snow_cover/gridded_product/cv/",
                                           int.days[i],".txt"))
write.table(int.snow$statistical_summary_cv,"/nobackup/users/dirksen/data/auxcillary_NED/snow_cover/gridded_product/tps_summary.txt",
            row.names=FALSE,col.names = !file.exists("/nobackup/users/dirksen/data/auxcillary_NED/snow_cover/gridded_product/tps_summary.txt"),
            append = TRUE,sep=",")
rm(input.snow)
} else print(paste0("no snow this on ",int.days[i]))

}

#from snow height to snow cover [fraction 0,1]
file_dir<-"/nobackup/users/dirksen/data/auxcillary_NED/snow_cover/gridded_product/tps"
save_dir<-"/nobackup/users/dirksen/data/auxcillary_NED/snow_cover/gridded_product/snow_cover/"

Dsn<-stack(list.files(paste0(file_dir),pattern=".grd",full.names = TRUE))
Dsn.datum<-gsub(".grd","",list.files(paste0(file_dir),pattern=".grd",full.names = FALSE))
names(Dsn)<-Dsn.datum

Csn<-Dsn/0.1
values(Csn)[values(Csn)>1]=1
writeRaster(Csn,paste0(save_dir,"snow_cover.grd"),overwrite=TRUE)

#Average albedo for snow conditions per ecosystem according to Moody(2007)
systems<-c("Evergreen needle forest","Evergreen broad forest","Deciduous needle forest","Deciduous broad forest",
           "Mixed forest","Closed shrubs","Open shrubs","Woody savanna","Savanna","Grassland","Wetland","Cropland",
           "Urban","Crop mosaic","Permanent snow","BarrenDesert")
VIS<-c(0.31,0.44,0.39,0.35,0.32,0.42,0.68,0.44,0.57,0.77,0.66,0.69,0.50,0.59,0.89,0.78)
NIR<-c(0.24,0.33,0.27,0.27,0.25,0.30,0.44,0.30,0.39,0.48,0.44,0.47,0.34,0.41,0.57,0.51)
TOT<-(VIS+NIR)/2
value<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)

albedo.ecosystem<-data.frame(systems,VIS,NIR,TOT,value)

#MODIS MOD12Q1 product with ecosystems
file<-"/usr/people/dirksen/pCloudDrive/data/MCD12Q1/MCD12Q1.A2001001.h18v03.051.2014287163303.hdf"

library(rgdal)
library(gdalUtils)
library(MODIS)

hdf_modis<-list.files("/usr/people/dirksen/pCloudDrive/data/MCD12Q1/",full.names = TRUE)
mod<-mapply(getSds,HdfName=hdf_modis,SIMPLIFY = FALSE)

yr<-seq(2001,2013,by=1)
for(i in 1:length(hdf_modis)){
y<-yr[i]
r<-raster(mod[[i]]$SDS4gdal[1])

r.ned<-projectRaster(r,r.distsea)
r.ned<-mask(r.ned,r.distsea)

writeRaster(r.ned,file=paste0("/usr/people/dirksen/pCloudDrive/data/auxcillary_NED/IGBP/igbp_",y,".grd"),overwrite=TRUE)
}

#Assigning albedo values to the classes
st.albedo.ecos<-stack(list.files("/usr/people/dirksen/pCloudDrive/data/auxcillary_NED/IGBP/",
                                 full.names = TRUE,
                                 pattern=".grd"))
st.albedo.ecos<-round(st.albedo.ecos)

val<-unique(albedo.ecosystem$value)
for(i in 1:length(val)){
st.albedo.ecos[values(st.albedo.ecos==val[i])]<-albedo.ecosystem$TOT[which(albedo.ecosystem$value==val[i])]
}

names(st.albedo.ecos)<-yr
writeRaster(st.albedo.ecos,"/usr/people/dirksen/pCloudDrive/data/auxcillary_NED/snow_cover/Snow_albedo_average_moody2007/average_snow_albedo.grd")

# class<-c("water","Evergreen needle forest","Evergreen broad forest","Deciduous needle forest","Deciduous broad forest",
#          "Mixed forest","Closed shrubs","Open shrubs","Woody savanna","Savanna","Grassland","Wetland","Cropland",
#          "Urban","Crop mosaic","Permanent snow","BarrenDesert","Unclassified","Fill Value")

################################################################
################################Diurnal Temperature Range
################################################################
#Background temperature field estimated form Tmax-Tmin
library(dplyr)
library(GeoInterpolation)
library(raster)
data("temperature_min_max")
data("coords_aws")
data("distsea.grid")
save_dtr<-"/nobackup/users/dirksen/data/auxcillary_NED/DTR/"

datums<-unique(temperature_min_max$Datum)

for(i in 1:length(datums)){
  d.in<-datums[i]
  print(d.in)
  temp.in<-temperature_min_max[which(temperature_min_max$Datum==d.in),]
  temp.in<-inner_join(temp.in,coords_aws,by="STN")
  coordinates(temp.in)<-~RDN_X+RDN_Y
  
  grid.ok<-interpolation_ordinarykriging(temp.in,
                                         variable = "DTR",
                                         grid_drift = distsea.grid)
  r.pred<-raster(grid.ok$spatial)
  writeRaster(r.pred,filename = paste0(save_dtr,"DTR_",d.in,".grd"),overwrite=TRUE)
}

