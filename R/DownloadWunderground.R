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
#' For multiple stations use lapply:
#' stations<-c('IZUIDHOL36','ILIMBURG36')
#' lapply(stations,download_time_seq)
#' @export
download_time_seq<-function(stationname = "IZUIDHOL87",
                            start = as.Date("2013/03/01"),
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

#only select files with data
rows<-unlist(lapply(df,nrow))
I.sub<-which(rows!=0)

if(length(I.sub)==0){
  fld<-paste0(folder.loc,"/",stationname,"/")
  unlink(fld,recursive=TRUE)
  return(FALSE)
}
df<-df[I.sub]

#check if the columns exist, rm non existing 
colnms<-unlist(lapply(df,function(x) "DateUTC<br>" %in% colnames(x) & var %in% colnames(x)))
df<-df[colnms]

if(length(df)==0){
  fld<-paste0(folder.loc,"/",stationname,"/")
  unlink(fld,recursive=TRUE)
  return(FALSE)
}

df.t<-lapply(df,function(x)subset(x,select=c(var,"DateUTC<br>"))) #Subsetting Columns in all the txt files
df.m<-Reduce(function(x,y) rbind(x,y,fill=TRUE),df.t)

#exit if there is no data in the files 
if(nrow(df.m)==0){
  fld<-paste0(folder.loc,"/",stationname,"/")
  unlink(fld,recursive=TRUE)
  return(FALSE)
}
names(df.m)<-c(var,"Timestamp_UTC")

write.table(df.m,paste0(folder.loc,"/",stationname,"/","daydata_rbind_",start,"_until_",stop,".txt"),row.names = FALSE,sep=",")
message(paste0("Combined all the files into: ","daydata_rbind_",start,"_until_",stop,".txt"))

unlink(txt.files)   # haalt alle dagfiles weg, alleen combined_stationname.txt bestaat nog.
message("removing all the single dayfiles")

}
