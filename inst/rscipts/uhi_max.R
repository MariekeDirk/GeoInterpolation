#' Maximum Urban heat island intensity
#' @title Maximum urban heat island intensity
#' @description Maximum urban heat island intensity calculated according to Theeuwes(2016). The UHImax is defined as the maximum 
#' temperature difference between the urban and rural site for a time period starting on Day0 08:00 up to Day1 07:00. The formula 
#' requires input from one rural reference station and the sky view factor and vegetation fraction inside the city.
#' Temperature data from several stations inside the city is recommended to validate the model.  
#' @param SVF the sky view factor at the locations
#' @param fveg vegetation fraction in a 500 meter radius
#' @param S hourly maximum solar irradiance at the rural site summed over 24 hours devided by 24. Start=day0 01:00, Stop=day1 00:00
#' @param DTR diurnal temperature range calculated from Tmax and Tmin in the rural area. Start=day0 08:00, Stop=day1 07:00
#' @param U mean 10 meter wind speed from hourly data from the rural site. Start=day0 08:00 Stop=day1 07:00
UHImax<-function(SVF,fveg,S,DTR,U){
  UHImax<-(2-SVF-fveg)*((S*DTR^3)/U)^(1/4)
  return(UHImax)
}

#' Calculate the mean solar irradiance 
#' @title Mean solar irradiance
#' @description Mean solar irradiance according to Theeuwes(2016). Calculate for the
#' rural site for a time period starting on Day0 01:00 up to Day1 00:00. 
#' @param time time vector in POSIXct with at least an hourly resolution
#' @param solar_irr solar irradiance measurements at the times of the time vector
calc_S<-function(time,solar_irr){
  df<-data.frame(time,"solar_irr"=as.numeric(solar_irr))
  df$hour<-cut(df$time,breaks="hour")
  df$day<-as.Date(df$time)
  df.h<-df %>% group_by(day,hour) %>% summarize(S=mean(solar_irr,na.rm=TRUE))
  df.h<-data.frame(df.h)
  df.h$hour<-as.POSIXct(df.h$hour)
  start=which(hour(df.h$hour)==1)
  stop=which(hour(df.h$hour)==0)
  
  if(start[1]>stop[1]){
    stop<-stop[2:length(stop)]
  }
  
  if(length(start) != length(stop)){
    start<-start[1:length(stop)]
  }
  
  S_out<-mapply(function(y,z) mean(df.h$S[y:z],na.rm=TRUE),
                y=start,
                z=stop)
  ss<-data.frame("start"=df.h$hour[start],"stop"=df.h$hour[stop],"S"=S_out)
  return(ss)
}

#' Calculate the Diurnal temperature range
#' @title Diurnal temperature range
#' @description Diurnal temperature range calculated according to Theeuwes(2016). The DTR is defined as the maximum 
#' temperature difference of the rural site for a time period starting on Day0 08:00 up to Day1 07:00. 
#' @param time time vector in POSIXct with at least an hourly resolution
#' @param temperature temperature measurmenents at the times of the time vector
calc_DTR<-function(time,temperature){
  df<-data.frame(time,"temperature"=as.numeric(temperature))
  df$hour<-cut(df$time,breaks="hour")
  df$day<-as.Date(df$time)
  df.h<-df %>% group_by(day,hour) %>% summarize(T=mean(temperature,na.rm=TRUE))
  df.h<-data.frame(df.h)
  df.h$hour<-as.POSIXct(df.h$hour)
  start=which(hour(df.h$hour)==8)
  stop=which(hour(df.h$hour)==7)
  
  if(start[1]>stop[1]){
    stop<-stop[2:length(stop)]
  }
  
  if(length(start) != length(stop)){
    start<-start[1:length(stop)]
  }
  
  Tmin<-mapply(function(y,z) min(df.h$T[y:z],na.rm=TRUE),
               y=start,
               z=stop)
  Tmax<-mapply(function(y,z) max(df.h$T[y:z],na.rm=TRUE),
               y=start,
               z=stop)
  DTR<-Tmax-Tmin
  ss<-data.frame("start"=df.h$hour[start],"stop"=df.h$hour[stop],"DTR"=DTR)
  return(ss)
}

#' Calculating mean wind speeds
#' @title Mean wind speed
#' @description Mean wind speed calculated according to Theeuwes(2016). The mean wind speed is defined as the mean
#' wind at the rural site for a time period starting on Day0 08:00 up to Day1 07:00. 
#' @param time time vector in POSIXct with at least an hourly resolution
#' @param wind wind speed at the times of the time vector
calc_U<-function(time,wind){
  df<-data.frame(time,"wind"=as.numeric(wind))
  df$hour<-cut(df$time,breaks="hour")
  df$day<-as.Date(df$time)
  df.h<-df %>% group_by(day,hour) %>% summarize(W=mean(wind,na.rm=TRUE))
  df.h<-data.frame(df.h)
  df.h$hour<-as.POSIXct(df.h$hour)
  start=which(hour(df.h$hour)==8)
  stop=which(hour(df.h$hour)==7)
  
  if(start[1]>stop[1]){
    stop<-stop[2:length(stop)]
  }
  
  if(length(start) != length(stop)){
    start<-start[1:length(stop)]
  }
  
  W_out<-mapply(function(y,z) mean(df.h$W[y:z],na.rm=TRUE),
                y=start,
                z=stop)
  ss<-data.frame("start"=df.h$hour[start],"stop"=df.h$hour[stop],"W"=W_out)
  return(ss)
}

library(raster)
library(rgdal)
library(mapview)
# neighbourhood<-readOGR(dsn="/nobackup/users/dirksen/data/auxcillary_NED/Buurtenkaart2018/",layer="buurt2018")
# district<-readOGR(dsn="/nobackup/users/dirksen/data/auxcillary_NED/Buurtenkaart2018/",layer="wijk_2018")
############################Layer information SVF and Greenness within district Utrecht
municipality<-readOGR(dsn="/nobackup/users/dirksen/data/auxcillary_NED/Buurtenkaart2018/",layer="gem_2018")
Utrecht<-municipality[which(municipality$GM_NAAM=="Utrecht"),]

greenness<-stack("/nobackup/users/dirksen/data/auxcillary_NED/rivm_20170415_g_groenkaart_10m/rivm_20170415_g_groenkaart_10m.tif")
Utrecht<-spTransform(Utrecht,crs(greenness))

greenness.u<-crop(greenness,extent(Utrecht))
greenness.u<-mask(greenness.u,Utrecht)

mapview(Utrecht) + greenness.u
#############################Point information Utrecht
library(data.table)
df<-fread("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/meta_data.txt")
coordinates(df)<- ~Lon + Lat
crs(df)<-CRS("+proj=longlat +datum=WGS84")
df<-spTransform(df,crs(Utrecht))

df.in.poly<-df[!is.na(over(df,Utrecht))[,1],]
df.utrecht<-data.frame(df.in.poly)
df.utrecht$exists<-unlist(lapply(df.utrecht$Station.ID,function(x) dir.exists(paste0("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/UHImax_Utrecht/",x))))
df.utrecht<-df.utrecht[df.utrecht$exists,] #only select stations for which we do have data

mapview(Utrecht) + greenness.u + df.in.poly


#############################Information from the rural station
Cabauw<-fread("/nobackup/users/dirksen/data/AWS/Cabauw_T_S_W_2016_201809.csv")
Cabauw$IT_DATETIME<-as.POSIXct(Cabauw$IT_DATETIME,format="%Y%m%d_%H%M00_000000")
Cabauw.S<-subset(Cabauw[which(Cabauw$DS_CODE=="348_S_a"),],select = c("IT_DATETIME","DS_CODE","TOS.Q_GLOB_10"))
Cabauw.T<-subset(Cabauw[which(Cabauw$DS_CODE=="348_T_a"),],select = c("IT_DATETIME","DS_CODE","TOT.T_DRYB_10"))
Cabauw.W<-subset(Cabauw[which(Cabauw$DS_CODE=="348_W_a"),],select = c("IT_DATETIME","DS_CODE","TOW.FF_SENSOR_10"))

S<-calc_S(Cabauw.S$IT_DATETIME,Cabauw.S$TOS.Q_GLOB_10)
DTR<-calc_DTR(Cabauw.T$IT_DATETIME,Cabauw.T$TOT.T_DRYB_10)
U<-calc_U(Cabauw.W$IT_DATETIME,Cabauw.W$TOW.FF_SENSOR_10)

Meteo_params<-data.frame(DTR,"S"=S$S[1:length(DTR$start)],"U"=U$W)
saveRDS(Meteo_params,"inst/Rdata/Meteo_params_cabauw.rds")
