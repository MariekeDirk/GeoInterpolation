#' Download Wunderground data
#' @title Wunderground time series from URL
#' @description Function to download 5min weather data from the \url{www.wunderground.nl} website. For a specific time period, 
#' station and variable(s) the data can be downloaded and saved in a directory. For each stationname a new folder is created (unless)
#' the folder already exists. After downloading the daily files all the .txt files are combined and the daily files are deleted. 
#' @param stationname Character, name of the station
#' @param start Date, start of the download period
#' @param stop Date, stop period of the download period
#' @param var Character, variable(s) which you want to download
#' @param folder.loc Character, location were the data will be stored. For each stationname a new folder is created. 
#' @examples 
#' #Run the example below to store data at the current location
#' download_time_seq(stationname = "IZUIDHOL87",
#'                   start = as.Date("2018/04/01"),
#'                   stop = as.Date("2018/05/01"),
#'                   var = "TemperatureC",
#'                   folder.loc = ".")
#' @export
download_time_seq<-function(stationname = "IZUIDHOL87",
                            start = as.Date("1999/03/01"),
                            stop = as.Date("2018/05/01"),
                            var = "TemperatureC",
                            folder.loc = "/nobackup/users/dirksen/data/wunderground/"){

requireNamespace("lubridate")
requireNamespace("data.table")

if (dir.exists(paste0(folder.loc,"/",stationname))) {
  message(paste0("Already a directory for available for ",stationname))
} else {
message(paste0("Creating a new directory for ",stationname))
dir.create(paste0(folder.loc,"/",stationname)) # maakt nieuw mapje voor het station waar een textfile per dag in komt
}

time.seq<-seq(start,stop,by="day")


for(i in 1:length(time.seq)){
t<-time.seq[i]
URL<-paste0("https://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=",stationname,"&day=",mday(t),"&year=",year(t),"&month=",month(t),"&format=1")

df<-fread(URL,sep=",",fill=TRUE)

#Main point of the complete.cases is to get rid of the <br> NA NA NA ... lines in the file
df<-df[complete.cases(df[[var]])] # In my case I'm only interested in the temperature
write.table(df,paste0(folder.loc,"/",stationname,"/","daydata_",t,".txt"),row.names = FALSE,sep=",")
}

# voegt alle dag-files samen in één grote datafile voor één station
# setwd(loc) # ga in de map staan met files die onder elkaar moeten staan. 
txt.files=list.files(path=paste0(folder.loc,"/",stationname,"/"), full.names=TRUE, pattern=".txt")
df<-lapply(txt.files,function(x)fread(x))
df.t<-lapply(df,function(x)subset(x,select=c(var,"DateUTC<br>"))) #Subsetting Columns in all the txt files
df.m<-Reduce(function(x,y) rbind(x,y,fill=TRUE),df.t)
names(df.m)<-c(var,"Timestamp_UTC")

write.table(df.m,paste0(folder.loc,"/",stationname,"/","daydata_rbind_",start,"_until_",stop,".txt"),row.names = FALSE,sep=",")
message(paste0("Combined all the files into: ","daydata_rbind_",start,"_until_",stop,".txt"))

unlink(txt.files)   # haalt alle dagfiles weg, alleen combined_stationname.txt bestaat nog.
message("removing all the single dayfiles")

}
###########################################

# Amsterdam Area: no good quality sensors

#Groningen

# IGRONING45
# Davis Vantage Pro2 Plus
# 53.232 6.604
# 0m

# IGRONING106
# Davis Vantage Pro2 Plus
# 53.238 6.602 
# 0m

# Eindhoven

# IEINDHOV167
# Davis Vantage Pro2 Plus
# 51.418 5.493
# 20m

# IEINDHOV175
# Oregon Scientific Professional Weather Center
# 51.460 5.480
# 3m

# stations<-c('IGRONING45','IGRONING106','IEINDHOV167','IEINDHOV175')
# 
# lapply(stations,download_time_seq)

## Script om Wunderground data te downloaden		##
## 1. Identificeer stations binnen een bepaald gebied	##
## 2. Download tijdreeksen				##
## 3. Header error check				##
##########################################################

######################################################
# 1. Identificeer stations binnen een bepaald gebied #
######################################################
# 
# rm(list=ls(all=TRUE))
# library(RCurl)
# 
# day <- 			# vul dag in als dd 
# month <- 		# vul maand in als MM
# year <- 		# vul jaar in als YYYY
# 
# # lijst met voorvoegsels van stationsnamen. Zie Wundermap voor veelvoorkomende voorvoegsels in het gebied. Bijv: in Wageningen zijn de voorvoegsels vaak "IWAGENIN".
# stationname_list <- c("IWAGENIN", "IBENNEKO")	
# 
# dir.create("Wundergroundstations")
# 
# for(m in 1:length(stationname_list)){
# stationname <- stationname_list[m]	
# dir.create(paste0("Wundergroundstations/",stationname)) 
# 
# for(n in 1:30){	# de getallen 1 t/m 300 worden geprobeerd als getal in stationID, dit is het geval in een middelgrote stad
# 	print(n)
# 		tryCatch({download.file(paste0("https://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=",stationname, n,"&day=",day,"&year=",year,"&month=",month,"&format=1"),
#                     paste0("Wundergroundstations/",stationname,"/",stationname, n,".txt"),quiet=TRUE)}, silent=FALSE, condition = function(err) {} )	
# }
# 
# 
# files <- list.files(path = paste0("Wundergroundstations/", stationname), all.files=TRUE, full.names=TRUE, recursive=FALSE, pattern=".txt")	
# unlink(files[which(file.info(files)$size<188)])	# files met alleen de header hebben grootte 185* byte (*op sommige computers 187)
# }
# 
# 	stationlocationsFileName <- paste0("stationlocations_",day, month, year,".txt")
# 	unlink(stationlocationsFileName)	
# 	input <- c("stationID lon lat")
# 	writeLines(input, stationlocationsFileName)
# 	
# 
#  
# for(stationname in stationname_list){
# list <- list.files(path = paste0("Wundergroundstations/", stationname), all.files=FALSE, full.names=FALSE, recursive=FALSE, pattern=".txt")
# if(length(list) < 1){next}	# lege mappen worden overgeslagen
# for(k in 1:length(list)){ 	
# st <- substr(list[k], 1, (nchar(list[k]) - 4))	# knipt laatste 4 letters eraf
# u <- paste0("http://www.wunderground.com/personal-weather-station/dashboard?ID=", st)	# mogelijk moet 'http' veranderd worden in 'https'
# txt <- getURL(u) 
# 
# 	#latitude:
# 	pos <- regexpr("lat:", txt)
# 	lat <- substr(txt, pos+6, pos+16)
# 	#longitude:
# 	pos <- regexpr("lon:", txt)
# 	lon <- substr(txt, pos+6, pos+15)
# 
# temp_list <- matrix(nrow=1, ncol=3)
# temp_list[,1] <- st		# k'de station uit map met voorvoegsel stationname, zonder .txt
# temp_list[,2] <- lon
# temp_list[,3] <- lat
# write.table(temp_list, stationlocationsFileName, append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)
# }
# }
# 
# # grenzen in graden van het gebied waarin je geinteresseerd bent:
# lon_min <- 4.67 				
# lon_max <- 5.05 
# lat_min <- 52.24
# lat_max <- 52.44
# 
# # let op: stationlocations.txt wordt hiermee overschreven 
# stationlocations <- read.table(stationlocationsFileName, header=TRUE)
# stationlocations <- stationlocations[which(stationlocations$lon >= lon_min & stationlocations$lon <= lon_max & stationlocations$lat >= lat_min & stationlocations$lat <= lat_max),]
# write.table(stationlocations, stationlocationsFileName, row.names=FALSE, col.names=TRUE, quote=FALSE)


