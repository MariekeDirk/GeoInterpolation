library(data.table)
library(dplyr)
main_loc<-"/nobackup/users/dirksen/data/wunderground/"

meta_data<-fread(paste0(main_loc,"meta_data.csv"),
                 header=TRUE,
                 dec=".",
                 colClasses = c("character","numeric","numeric","character","numeric","character"))

meta_data<-meta_data[which(meta_data$`Data available`=="Y"),]
wur_paths<-paste0(main_loc,meta_data$'Station ID',"/")
wur_files<-lapply(wur_paths,list.files)
wur_full_names<-paste0(wur_paths,unlist(wur_files))

#first 9 files processed
#IEINDHOV175 empty

df<-lapply(wur_full_names,fread)
df<-lapply(df,function(x) data.frame(x$TemperatureC,as.Date(x$Timestamp_UTC)))
df<-lapply(df,setNames,nm=c("TemperatureC","day")) #remove unrealistic values or NA

df<-lapply(df,function(x) x[-which(x$TemperatureC < -50),]) #remove unrealistic values or NA

#for the first station the time interfall is 5 min
max_obs<-288
min_obs<-round(288*0.9)
#
    
obs_per_day<-lapply(df,function(x) plyr::count(x,vars="day"))

I_select<-lapply(obs_per_day, function(x) -which(x$freq < max_obs & x$freq > min_obs))

day_subset<-mapply(x=df,y=I_select,function(x,y) x[y,],SIMPLIFY = FALSE)

# inner join with the original dataset
df_filter<-unlist(lapply(day_subset,function(x) nrow(x)))
I_filter<-which(df_filter!=0)
day_subset<-day_subset[I_filter]

df_daymean<-lapply(day_subset,function(x) aggregate(TemperatureC ~ day, data = x, mean))

df_meta<-meta_data$`Station ID`[I_filter]

mapply(x=df_daymean,y=df_meta,function(x,y) write.table(x,
                                                      file = paste0("/nobackup/users/dirksen/data/wunderground/Daymean/",y,".txt"),
                                                      row.names = FALSE,
                                                      sep=","
                                                      ))
# write.table(df_daymean,
#             file = paste0("/nobackup/users/dirksen/data/wunderground/Daymean/",meta_data$'Station ID'[i],".txt"),
#             row.names = FALSE,
#             sep=",")

################adding information to the meta data
meta_data<-meta_data[I_filter,]
day_means<-list.files("/nobackup/users/dirksen/data/wunderground/Daymean/",full.names = TRUE)
df.means<-lapply(day_means,fread)

day.min<-sapply(df.means,function(x) min(x$day))
day.max<-sapply(df.means,function(x) max(x$day))
day.nr<-sapply(df.means,function(x) length(x$day))

meta_data$start<-day.min
meta_data$stop<-day.max
meta_data$number_obs<-day.nr
setkey(meta_data,"Station ID")

write.table(meta_data,file="/nobackup/users/dirksen/data/wunderground/meta_data_start_stop.txt",
            row.names = FALSE,
            col.names = TRUE,
            sep = ",")
