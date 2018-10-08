library(data.table)
library(dplyr)
library(tidyr)
library(RCurl)
library(lubridate)
library(data.table)
library(stringr)
library(rgdal)

#GGD Amsterdam
meta<-fread("/nobackup/users/dirksen/data/GGD_temperature_ams/temperaturen/metadata_GGD_Amsterdam.txt")
# df<-fread("/nobackup/users/dirksen/data/GGD_temperature_ams/temperaturen/Temp outdoor uur 2014 2015.CSV",skip=1)
df<-fread("/nobackup/users/dirksen/data/GGD_temperature_ams/temperaturen/Temp outdoor uur 2016 2017.CSV",skip=1)

get_station_numbers<-function(sensor_nms){
  sensor_nms<-gsub("^[0-9]*[A-Z]* - ","",sensor_nms) # then find the station numbers and add them as rownames
  station_numbers<-gsub(" [A-Z].*","",sensor_nms)
  station_numbers<-as.numeric(station_numbers)
  return(station_numbers)
}

I<-grep("^V",names(df))
df<-subset(df,select=-I)
sensor_nms<-names(df)[2:length(names(df))] #these names to long format

df2<-gather(df,key=Measurement,value=Temperature,sensor_nms,-Dates)
I<-grep("*Indoor*",df2$Measurement)

df2<-df2[-I,]
df2<-df2[complete.cases(df2$Temperature),]
df2$STN<-get_station_numbers(df2$Measurement)
df2$Day<-as.Date(as.POSIXct(df2$Dates,format="%d-%m-%Y %H:%M"))

#how many different sensors per location? calculate daymean values for each sensor first and count the obs!
Tmean<-df2 %>%
  select(STN,Day,Temperature) %>%
  group_by(STN,Day) %>%
  summarise_all(funs(mean,n()))

# un_m<-Tmean %>% select(STN,Day,Measurement) %>% count(n_distinct(Measurement)) # 1,2 or 3 measurements per location

#files from 2014-2015 and 2016-2017 saved (as daymean per location)
# saveRDS(Tmean,file="/nobackup/users/dirksen/data/GGD_temperature_ams/daymean/GGD_Amsterdam_daymean_2016_2017.rds")

#Veenkampen
link<-"http://www.met.wur.nl/veenkampen/data/"
dates<-seq(as.Date("2011/06/01"),as.Date("2018/09/01"),by="day")
years<-year(dates)
months<-str_pad(month(dates),2,pad="0")
filenames<-paste0("10min_",format(dates,"%Y%m%d"),".txt")
url<-paste0(link,years,"/",months,"/",filenames)

get_Veenkampen_data<-function(url,date){
df<-try(fread(url,
              select=c(1,2,3)
              ),
              TRUE)
if(inherits(df,"try-error")){return(FALSE)}    
  
names(df)<-c("Date","min","Tmean")
df$Tmean[df$Tmean < -50] <- NA

df<-df[complete.cases(df$Tmean),]

nobs<-length(df$Tmean)
if(nobs < floor(0.9*(24*6))){
  message("less than 90% of the daily data available, returning FALSE")
  return(FALSE)
}
# df$POSIXct<-as.POSIXct(paste(df$Date,df$min))
Tg<-mean(df$Tmean) #cannot aggregate to daymeans since there is a measurement in these files of the next day
df<-data.frame(date,Tg,nobs)
# names(df)<-c("Date","Tmean")
write.table(df,file = "/nobackup/users/dirksen/data/Veenkampen/daymean.txt",
            row.names = FALSE,
            col.names = !file.exists("/nobackup/users/dirksen/data/Veenkampen/daymean.txt"),
            append = file.exists("/nobackup/users/dirksen/data/Veenkampen/daymean.txt"),
            sep = ",")
return(TRUE)
}

veenkampen<-mapply(get_Veenkampen_data,url=url,date=dates)



#########Haarweg --> not downloadable (formatting and naming is inconsistent)
# link<-"http://www.met.wur.nl/haarwegdata/dayfiles/NETCDF_ALLDATA/"
# fname<-"Hweg1974_1989_24h.nc"
library(lubridate)
library(data.table)
library(stringr)
link<-"http://www.met.wur.nl/haarwegdata/dayfiles/YYYY/MM/mYYYYMM.txt"
tm<-seq(from=as.Date("2001-08-01"),to=as.Date("2012-06-01"),by="month")
yyyy<-year(tm)
mm<-str_pad(month(tm),2,pad="0")

for(i in 1:length(tm)){
url<-gsub("YYYY",yyyy[i],link)
url<-gsub("MM",mm[i],url)
df.haarweg<-fread(url,select = c(1,2,3,4))
names(df.haarweg)<-c("year","month","day","Tg")

write.table(df.haarweg,file = "/nobackup/users/dirksen/data/Veenkampen/Haarweg_daymean.txt",
            row.names = FALSE,
            col.names = !file.exists("/nobackup/users/dirksen/data/Veenkampen/Haarweg_daymean.txt"),
            append = file.exists("/nobackup/users/dirksen/data/Veenkampen/Haarweg_daymean.txt"),
            sep = ",")
}

# Haarweg_meta<-list("Station Name" = "Wageningen - Haarweg",
#                   "Country Name" = "Netherlands",
#                   "Country Code" = "NL",
#                   "Elevation [m]" = 7.0,
#                   "Latitude" = 51.967,
#                   "Longitude" = 5.633,
#                   "EPSG" = CRS("+init=epsg:4326"),
#                   "source metadata" = "http://www.met.wur.nl/haarwegdata/",
#                   "source data" = "http://www.met.wur.nl/haarwegdata/dayfiles")
# 
# saveRDS(Haarweg_meta,"/nobackup/users/dirksen/data/Veenkampen/haarweg_meta.rds")
#modis lst
library(raster)
library(mapview)
r<-raster("/nobackup/users/dirksen/data/MODIS_LST/eurolst_clim.bio01/eurolst_clim.bio01.tif")
r<-r/10

# #GGD Amsterdam
# station_name<-c(
# "HAMS,Amsterdam-Spaarnwoude",
# "PNH, Oude Meer",
# "PNH, De Rijp",
# "GGD Amsterdam,Nieuwendammerdijk",
# "GGD Amsterdam, Vondelpark",
# "GGD Amsterdam,Westerpark",
# "GGD Amsterdam,Oude Schans",
# "GGD Amsterdam,Kantershof",
# "GGD Amsterdam,Sportpark Ook Meer",
# "ZNSTD,Zaandam",
# "GGD Amsterdam,Haarlemmerweg",
# "GGD Amsterdam,Einsteinweg",
# "GGD Amsterdam,Van Diemenstraat",
# "GGD Amsterdam,Stadhouderskade",
# "GGD Amsterdam,Jan van Galenstraat",
# "GGD Amsterdam, Ring A10 zuid",
# "HAMS Hoogtij",
# "PNH, IJmuiden",
# "PNH, Wijk aan Zee",
# "PNH, Hemkade",
# "PNH, Staalstraat",
# "PNH, Reijndersweg",
# "PNH, Beverwijk-West",
# "PNH, Hoofddorp",
# "PNH, Badhoevedorp"
# )
# # 
# station_number<-c(
# 703,565,556,3,14,16,
# 19,21,22,701,2,7,12,
# 17,20,18,704,551,553,
# 546,572,573,570,564,
# 561
# )
# # 
# RD_x<-c(
# 110174,112925,119365,
# 124816,119509,119806,
# 122136,127809,114552,
# 116224,120178,118098,
# 121027,121790,119129,
# 121640,113224,101625,
# 101701,117236,103460,
# 100117,104279,109178,
# 113160
# )
# # 
# RD_y<-c(
# 490270,477070,508578,
# 489144,485885,489695,
# 487245,481491,486712,
# 495739,488736,488302,
# 489238,485683,487569,
# 483008,493541,497545,
# 500987,492637,498790,
# 499322,500443,482381,
# 483003
# )
# # 
# meta<-data.frame(station_name,station_number,RD_x,RD_y)
# write.table(meta,"/nobackup/users/dirksen/data/GGD_temperature_ams/metadata_GGD_Amsterdam.txt",
#             sep=",",
#             row.names=FALSE,
#             col.names = TRUE)

#Veenkampen
# veenkampen_meta<-list("Station Name" = "Wageningen - Veenkampen",
#                       "Station ID" = "NLE00152462",
#                       "Country Name" = "Netherlands",
#                       "Country Code" = "NL",
#                       "Elevation [m]" = 5.0,
#                       "Latitude" = 51.9814,
#                       "Longitude" = 5.6217,
#                       "EPSG" = CRS("+init=epsg:4326"),
#                       "source metadata" = "https://geographic.org/global_weather/netherlands/wageningen___veenkampen_462.html",
#                       "source data" = "http://www.met.wur.nl/veenkampen/data/")
# saveRDS(veenkampen_meta,"/nobackup/users/dirksen/data/Veenkampen/veenkampen_meta.rds")