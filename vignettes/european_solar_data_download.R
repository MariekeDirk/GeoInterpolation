

library(XML)
# library(rvest)
library(RCurl)
library(tidyr)
library(countrycode)

#Get all the country names -->paste links later
url_country<-"http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/t6.html"
doc.html<-htmlTreeParse(url_country,useInternal=TRUE)
doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
doc.text = gsub('\r\r\n', ',', doc.text)
countries<-data.frame(t(read.table(textConnection(doc.text[2]),stringsAsFactors = FALSE,sep=",")))
names(countries)<-"country"
countries$country<-as.character(countries$country)
countries<-countries[complete.cases(countries$country),]
countries<-gsub(" ","_",countries)
saveRDS(countries,"./data/countrieswrdc.rds")
################################################
#Get station names from the countries
get_station_names<-function(nm){
  url_station<-paste0("http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/",nm,"/",nm,".html")
  doc.html<-htmlTreeParse(url_station,useInternal=TRUE)
  doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
  doc.text = gsub('\r\r\n', ',', doc.text)
  
  stations<-unlist(strsplit( doc.text[3], ',,' ))
  names(stations)<-"stn"
  # stations<-stations$stn[complete.cases(stations$stn)]
  stations<-gsub(",","",stations)
  stations<-gsub("\\. / ","_",stations)
  stations<-gsub("\\. ","_",stations)
  stations<-gsub("\\.","_",stations)
  stations<-gsub(" / ","_",stations)
  stations<-gsub(" /","_",stations)
  stations<-gsub("/ ","_",stations)
  stations<-gsub("/","_",stations)
  stations<-gsub("\\'","_",stations)
  stations<-gsub(" ","_",stations)
  return(stations)
}

get_station_meta<-function(nm,stn){
  url_years<-paste0("http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/",nm,"/",stn,"/",stn,".html")
  doc.html<-htmlTreeParse(url_years,useInternal=TRUE)
  doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
  doc.text = gsub('\r\r\n', ',', doc.text)
  
  start<-data.frame(t(read.table(textConnection(doc.text[6]),stringsAsFactors = FALSE,sep=",")))[2,]
  # stop<-tail(data.frame(t(read.table(textConnection(doc.text[length(doc.text)]),stringsAsFactors = FALSE,sep=","))),n=2)[1,]
  # 
  # years<-seq(as.numeric(start),as.numeric(stop),by=1)
  ################################################
  yr<-start[1]
  #get Metadata from the station
  link<-paste0("http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/",nm,"/",stn,"/",stn,"_",yr,"_t6.html")
  # link<-"http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/italy/s_valentino/s_valentino_2012_t6.html"
  
  #station id all unique numbers starting from 16324
  #altitude in 0.1m
  #lat and lon in seconds
  doc.html<-htmlTreeParse(link,useInternal=TRUE)
  doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
  doc.text = gsub('\n', ' ', doc.text)
  
  country<-gsub("Country: ","",doc.text[2])
  coun_id<-countrycode(country,'country.name','iso3c')
  
  name<-gsub("Station: ","",doc.text[3])
  
  lat<-gsub("Latitude =","",doc.text[5])
  ns<-substr(lat,nchar(lat),nchar(lat))
  
  lat<-(as.numeric(str_match(lat,"(.*?)°")[,2])+(as.numeric(str_match(lat,"°(.*?)'[A-Z]")[,2])/60))*3600
  
  
  if(ns=="N"){
    lat = lat
  } else {lat = -lat}
  
  lon<-gsub("Longitude =","",doc.text[6])
  ew<-substr(lon,nchar(lon),nchar(lon))
  
  lon<-(as.numeric(str_match(lon,"(.*?)°")[,2])+(as.numeric(str_match(lon,"°(.*?)'[A-Z]")[,2])/60))*3600
  if(ew=="E"){
    lon = lon
  } else {lon = -lon}
  
  elv<-gsub("Elevation = ","",doc.text[7])
  elev<-as.numeric(elv)*10
  
  wmo_id<-gsub("WMO Identifier: ","",doc.text[8])
  year<-gsub("Year ","",doc.text[9])
  df.meta<-data.frame(wmo_id,name,coun_id,lat,lon,elev)
  return(df.meta)
}

get_station_start<-function(nm,stn){
  url_years<-paste0("http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/",nm,"/",stn,"/",stn,".html")
  doc.html<-htmlTreeParse(url_years,useInternal=TRUE)
  doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
  doc.text = gsub('\r\r\n', ',', doc.text)
  
  start<-data.frame(t(read.table(textConnection(doc.text[6]),stringsAsFactors = FALSE,sep=",")))[2,]
}

get_station_data<-function(nm,stn,yr){
  link<-paste0("http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/",nm,"/",stn,"/",stn,"_",yr,"_t6.html")
  url<-getURL(link)
  tab<-readHTMLTable(url)
  
  df<-data.frame(tab)
  df<-data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
  df.clean<-df[1:(nrow(df)-2),]
  
  names(df.clean)<-gsub("NULL.","",names(df.clean))
  
  
  drop<-seq(3,length(names(df.clean)),by=2) #exclude the quality flag
  
  df.qc<-subset(df.clean,select=drop)
  df.qc<-gather(df.qc,Flag,qc)
  
  df.clean<-subset(df.clean,select=-drop)
  
  
  
  #transpose to long data format
  colin<-names(df.clean)[2:length(names(df.clean))]
  df.clean<-gather(df.clean,  Month, Radiation,colin)
  df.clean$qc<-df.qc$qc
  
  df.clean$Radiation<-as.numeric(df.clean$Radiation)
  df.clean<-df.clean[complete.cases(df.clean$Radiation),]
  df.clean$year<-year
  df.clean$id<-id
  df.clean$date<-as.Date(paste0(df.clean$year,df.clean$Month,df.clean$DATE),format="%Y%b%d")
  
  df.wrdc.ecad<-df.clean[,c("id","date","Radiation","qc")]
  names(df.wrdc.ecad)<-c("wmo_id","ser_date","qq","qc")
  df.wrdc.ecad$qcm<--9
  df.wrdc.ecad$qca<--9
  return(df.wrdc.ecad)
}

###############################################################
#loop trough all the countries and stations
for(j in 1:length(countries)){
nm<-countries[j]
print(nm)
stations<-get_station_names(nm)

for(i in 1:length(stations)){
stn<-stations[i]
print(stn)

df.meta<-get_station_meta(stn)

df.wrdc.ecad<-get_station_data(stn)
# write.table(df.meta,paste0("/nobackup/users/dirksen/data/radiation_europe/WRDC/meta/",nm,"_meta.txt"),
#             row.names = FALSE,
#             col.names = !file.exists(paste0("/nobackup/users/dirksen/data/radiation_europe/WRDC/meta/",nm,"_meta.txt")),
#             sep=",",
#             append = TRUE)
}
}
###############################################################

#Get the table

