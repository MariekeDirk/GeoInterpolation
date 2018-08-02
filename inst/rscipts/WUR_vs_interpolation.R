library(data.table)
library(dplyr)
main_loc<-"/nobackup/users/dirksen/data/wunderground/"

meta_data<-fread(paste0(main_loc,"meta_data.csv"),
                 header=TRUE,
                 dec=".",
                 colClasses = c("character","numeric","numeric","character","numeric"))


wur_paths<-paste0(main_loc,meta_data$'Station ID',"/")
wur_files<-lapply(wur_paths,list.files)
wur_full_names<-paste0(wur_paths,unlist(wur_files))

#first 9 files processed
#IEINDHOV175 empty
i=9
df<-fread(wur_full_names[i])
df$day<-as.Date(df$Timestamp_UTC)

#remove unrealistic values
df<-df[-which(df$TemperatureC < -50),]


#for the first station the time interfall is 5 min
max_obs<-288
min_obs<-round(288*0.9)
#
    
obs_per_day<-plyr::count(df,vars="day")
I_select<-which(obs_per_day$freq < max_obs & obs_per_day$freq > min_obs)

day_subset<-obs_per_day[I_select,]

# inner join with the original dataset
df_filtered<-inner_join(df,day_subset)

df_daymean<-aggregate(TemperatureC ~ day, data = df_filtered, mean)

write.table(df_daymean,
            file = paste0("/nobackup/users/dirksen/data/wunderground/Daymean/",meta_data$'Station ID'[i],".txt"),
            row.names = FALSE,
            sep=",")

################adding information to the meta data

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
