#'
#'@title load auxiliary data
#'@param datum datum in the format Ymd
#'@description find and loads all the auxiliary data used for the interpolation and returns a raster stack. 
#'@details sarah insolation, siccs data, albedo from modis, snow coverage, snow albedo modis, distance to the sea,
#'ndvi, precipitation and roughness are all loaded. 
#'@return returns a raster stack with all the layers
#'@author Marieke Dirksen
#'@export
load_auxiliarydata<-function(datum){
  requireNamespace("sp", quietly = TRUE)
  requireNamespace("raster", quietly = TRUE)
  requireNamespace("rhdf5", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  requireNamespace("spatialEco", quietly = TRUE)
  
 
#File locations
sarah_dates<-as.Date(list.files("/nobackup/users/dirksen/data/auxcillary_NED/insolation/",pattern=".grd"),format="sat_%Y-%m-%d.grd")
sarah_grids<-list.files("/nobackup/users/dirksen/data/auxcillary_NED/insolation/",pattern=".grd",full.names = TRUE)

siccs_dates<-as.Date(list.files("/nobackup/users/dirksen/data/auxcillary_NED/insulation_SICCS/",pattern=".grd"),format = "insulation_SICCS_%Y-%m-%d.grd")
siccs_grids<-list.files("/nobackup/users/dirksen/data/auxcillary_NED/insulation_SICCS/",pattern = ".grd", full.names = TRUE)

albedo_with_snow<-stack("/nobackup/users/dirksen/data/auxcillary_NED/albedo/MODIS_albedo_jd.grd")
albedo_no_snow<-"/nobackup/users/dirksen/data/auxcillary_NED/albedo/modis_no_snow/"

average_snow_albedo<-stack("/nobackup/users/dirksen/data/auxcillary_NED/snow_cover/Snow_albedo_average_moody2007/average_snow_albedo.grd")
snow_cover<-stack("/nobackup/users/dirksen/data/auxcillary_NED/snow_cover/gridded_product/snow_cover/snow_cover.grd")
days.snow<-as.Date(names(snow_cover),format="X%Y.%m.%d")

geo_buffer<-stack("/nobackup/users/dirksen/data/auxcillary_NED/grid_buffer/grid_buffer.grd")

modis_lst_loc<-"/nobackup/users/dirksen/data/auxcillary_NED/MODIS_LST/MOD11A2_LST_2000__2017_NL1km/data/NL1km/"
modis_lst_day<-list.files("/nobackup/users/dirksen/data/auxcillary_NED/MODIS_LST/MOD11A2_LST_2000__2017_NL1km/data/NL1km",pattern = "_Day_")
modis_lst_night<-list.files("/nobackup/users/dirksen/data/auxcillary_NED/MODIS_LST/MOD11A2_LST_2000__2017_NL1km/data/NL1km",pattern = "_Night_")

dtr_loc<-"/nobackup/users/dirksen/data/auxcillary_NED/DTR"
dtr_full_names<-list.files(dtr_loc,pattern=".grd",full.names = TRUE)
dtr_dates<-as.Date(list.files(dtr_loc,pattern=".grd"),format="DTR_%Y-%m-%d")

#Initial variables
astronomical_summer_begin<-yday("2000-03-20")
astronomical_summer_end<-yday("2000-09-23")
time.year<-lubridate::year(datum)
time.day<-lubridate::yday(datum)
dnum<-seq(1,365,by=16)
igbp.yr<-seq(2001,2013,by=1)
j<-which(sarah_dates==datum)
sc<-which(siccs_dates==datum)
dtr<-which(dtr_dates==datum)

#Auxcillary data
dummy<-stack("/nobackup/users/dirksen/data/auxcillary_NED/auxcillary_stacks/center_scale_rough.grd")

# albedo<-dummy[[18]]
height<-dummy[[17]]
# distsea<-dummy[[1]]

# st.aux<-stack(height,distsea)
# names(st.aux)<-c("height","distsea")
st.aux<-stack(height)
names(st.aux)<-c("height")

st.pop<-dummy[[2:16]]
year.pop<-year(as.Date(gsub("INW","",names(st.pop)),format="%Y"))

st.auxiliary<-st.aux

#POPULATION
#Get the correct population density map
if(time.year<year.pop[1]){
  print("using first population year")
  names(st.pop[[1]])<-"INW"
  st.auxiliary<-addLayer(st.auxiliary,st.pop[[1]])
} else if(time.year>="2015"){
  print("using last population year")
  names(st.pop[[15]])<-"INW"
  st.auxiliary<-addLayer(st.auxiliary,st.pop[[15]])
} else {
  pop.sub<-which(names(st.pop)==paste0("INW",time.year))
  names(st.pop[[pop.sub]])<-"INW"
  st.auxiliary<-addLayer(st.auxiliary,st.pop[[pop.sub]])
}

#ALBEDO
#this albedo is a 16day averaged 8 year climatology with snow, consider also the no-snow version


if(time.day>353){
  I<-which(dnum==dnum[23])
  print(paste0("albedo 16 day climatology, using day ",dnum[23]))
  albedo<-stack(paste0(albedo_no_snow,"MODIS_353.grd"))
} else {
  I<-max(which(time.day>=dnum))
  print(paste0("albedo 16 day climatology, using day ",dnum[I]))
  albedo<-stack(paste0(albedo_no_snow,"MODIS_",dnum[I],".grd"))
} 

I<-which(days.snow==datum)


if(length(I)!=0){
  csn<-snow_cover[[I]]
  
  if(time.year<igbp.yr[1]){
    aasn<-average_snow_albedo[[1]]
  } else if(time.year>tail(igbp.yr,n=1)){
    aasn<-average_snow_albedo[[13]]
  } else {
    aasn.sub<-which(names(average_snow_albedo)==paste0("X",time.year))
    aasn<-average_snow_albedo[[aasn.sub]]
  }
  
  #add the two layers!   
  print("snow albedo added to albedo climatology")  
  albedo.total<-(1-csn)*albedo+csn*aasn
  
  
} else{albedo.total<-albedo}

names(albedo.total)<-"albedo"
st.auxiliary<-addLayer(st.auxiliary,albedo.total)

#ROUGHNESS
#Get the seasonal roughness map
roughness<-stack("/nobackup/users/dirksen/data/auxcillary_NED/roughness/roughness_summer_winter.grd")
roughness<-raster::scale(roughness)


if(yday(datum)<astronomical_summer_begin | yday(datum)>astronomical_summer_end){
  print("roughness summer")
  names(roughness[[2]])<-"roughness"
  st.auxiliary<-addLayer(st.auxiliary,roughness[[2]])
} else{
  print("roughness winter")
  names(roughness[[1]])<-"roughness"
  st.auxiliary<-addLayer(st.auxiliary,roughness[[1]])
}

#PRECIPITATION
precip_aux<-stack(paste0("/nobackup/users/dirksen/data/auxcillary_NED/precipitation/monthly_clim/precip_",month(datum),".grd"))
precip_aux<-raster::scale(precip_aux)
names(precip_aux)<-"P"
st.auxiliary<-addLayer(st.auxiliary,precip_aux)

#VEGETATION INDEX
nvdi_aux<-stack(paste0("/nobackup/users/dirksen/data/auxcillary_NED/NDVI/monthly_clim/MODIS_clim_ndvi_",month(datum),".grd"))
nvdi_aux<-raster::scale(nvdi_aux)
names(nvdi_aux)<-"NDVI"
st.auxiliary<-addLayer(st.auxiliary,nvdi_aux)

#INSULAION
if (length(sc)!=0) {
  
  print("using SICCS daily insulation")
  siccs_aux<-stack(siccs_grids[sc])
  siccs_aux<-raster::scale(siccs_aux)
  names(siccs_aux)<-"irr"
  st.auxiliary<-addLayer(st.auxiliary,siccs_aux)
  
} else if (length(j)!=0){
  print("sarah daily insulation available")
  sarah_aux<-stack(sarah_grids[j])
  r<-raster::as.matrix(sarah_aux)
  
  if(length(na.omit(r))/length(r)>0.5){
    print("using sarah monthly climatology because there are NA values in sarah grid")
    sarah_aux<-stack(paste0("/nobackup/users/dirksen/data/auxcillary_NED/insulation_monthly_climatology/",month(datum),"_month.grd"))
    sarah_aux<-raster::scale(sarah_aux)
    names(sarah_aux)<-"irr"
    st.auxiliary<-addLayer(st.auxiliary,sarah_aux)
  }else{
    sarah_aux<-raster::scale(sarah_aux)
    names(sarah_aux)<-"irr"
    st.auxiliary<-addLayer(st.auxiliary,sarah_aux)
  }
  
} else {
  print("using sarah monthly climatology sarah and SICCS not there")
  sarah_aux<-stack(paste0("/nobackup/users/dirksen/data/auxcillary_NED/insulation_monthly_climatology/",month(datum),"_month.grd"))
  sarah_aux<-raster::scale(sarah_aux)
  names(sarah_aux)<-"irr"
  st.auxiliary<-addLayer(st.auxiliary,sarah_aux)
}

#Diurnal Temperature range
r.dtr<-stack(dtr_full_names[dtr])
names(r.dtr)<-"dtr"
st.auxiliary<-addLayer(st.auxiliary,r.dtr)

#Yearday and ORIGIN day
yearday<-yday(datum)
originday<-as.numeric(as.POSIXct(datum))/(3600*24)

r.yearday<-height
raster::values(r.yearday)<-yearday
names(r.yearday)<-"yearday"
st.auxiliary<-addLayer(st.auxiliary,r.yearday)

r.originday<-height
raster::values(r.originday)<-originday
names(r.originday)<-"originday"
st.auxiliary<-addLayer(st.auxiliary,r.originday)

#GEO BUFFER
st.auxiliary<-addLayer(st.auxiliary,geo_buffer)



print("added all auxiliary data")
spdf_aux<-as(st.auxiliary,"SpatialGridDataFrame")
# names(spdf_aux)<-c("Height","Distsea",
#                    "Population","Albedo",
#                    "Roughness","Precipitation_monthly",
#                    "NDVI","Radiation","yday",
#                    "originday")
# spdf_aux<-as(spdf_aux,"SpatialPointsDataFrame")
# 
# spdf_aux<-sp.na.omit(spdf_aux)            #remove NA values with spatialEco library

return(spdf_aux)
}

