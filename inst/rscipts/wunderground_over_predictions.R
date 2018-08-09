library(data.table)
library(raster)

data("wunderground_meta")

#list with all the station locations and their meta data
wur_files<-list.files("/nobackup/users/dirksen/data/wunderground/Daymean/",full.names = TRUE)
wur_names<-gsub(".txt","",list.files("/nobackup/users/dirksen/data/wunderground/Daymean/",full.names = FALSE))
wur_df<-data.frame(wur_files,wur_names)
wur_df<-data.frame(lapply(wur_df,as.character),stringsAsFactors = FALSE)
wur_df<-merge(wunderground_meta,wur_df,by.x="Station.ID",by.y="wur_names")

#looping through all the raster predictions
#(1) starting with 1 station (file.nr loop)
#(2) and looping through the dates (i loop)
lm_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/linear_mod/Predictions"
cubist_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/Cubist_mod/Predictions"
svm_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/SVM_radial_mod/Predictions"
ok_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/ok_model/prediction"

predictions<-c(lm_predictions,cubist_predictions,svm_predictions,ok_predictions)

model_names<-c("lm","cubist","svm","ok")

for(file.nr in 1:length(wunderground_meta$Station.ID)){
wunderground_data <- fread(wur_df$wur_files[file.nr])
wunderground_data$day <- as.Date(wunderground_data$day)
setkey(wunderground_data,day)
all_days<-unique(wunderground_data$day)
fname<-paste0("/nobackup/users/dirksen/data/wunderground/overlay_predictions/",wur_df[file.nr,]$Station.ID,".txt")

for(i in 1:length(all_days)){
print(all_days[i])
#Loop through all the days
df1<-wunderground_data[which(wunderground_data$day==all_days[i]),]
  
#make the data spatial
wunderground_df1<-cbind(wur_df[file.nr,],df1)
coordinates(wunderground_df1)<-~Lon+Lat
crs(wunderground_df1)<-CRS("+init=epsg:4326")  
  
ml_raster<-paste0(predictions[1:3],"/temperature_lm_pca_harmonie",all_days[i],".grd")
ok_raster<-paste0(predictions[4],"/temperature_kriging_pca_harmonie",all_days[i],".grd")
st_raster<-c(ml_raster,ok_raster)

spdf = wunderground_df1

if(file.exists(st_raster[1])){
  raster_prediction<-stack(st_raster) 
  names(raster_prediction)<-model_names
  spdf<-spTransform(spdf,crs(raster_prediction))
    
  int.value<-extract(raster_prediction,spdf,method="bilinear") #get information from grid point
    
  df.new<-cbind(data.frame(spdf),int.value)
  # names(df.new)<-c(names(data.frame(spdf)),model_names)
    
  write.table(df.new,
                file = fname,
                sep = ",",
                row.names = FALSE,
                col.names = !file.exists(fname),
                append = TRUE)  

  
} else{message("file does not exist")}

} #end i loop

} #end file.nr loop

library(ggmap)
library(ggplot2)
df<-fread("/nobackup/users/dirksen/data/wunderground/meta_data_start_stop.txt")
map<-get_map(location = 'Holland',zoom=7)

ggmap(map) + geom_point(data=df,aes(x=Lon,y=Lat),size=2,alpha=0.5,col='red') +
  geom_label_repel(data=df,aes(x=Lon,y=Lat,label=`Station ID`),size=3)

#############Analysis of the overlay files
R2<-function(pred,obs){1-sum((obs-pred)^2)/sum((obs-mean(pred))^2)}
rmse<-function(pred,obs){(mean(pred-obs)^2)^0.5}
me<-function(pred,obs){mean(pred-obs)}
quant<-function(pred,obs){quantile(pred-obs,probs=c(0.1,0.33,0.5,0.66,0.9))}
main_path<-"/nobackup/users/dirksen/data/wunderground/overlay_predictions"
overlay_files<-list.files(main_path,pattern = ".txt",full.names = TRUE)
overlay_station<-gsub(".txt","",list.files(main_path,pattern = ".txt",full.names = FALSE))

fname<-"/nobackup/users/dirksen/data/wunderground/ME.txt"
for(i in 1:length(overlay_station)){
df<-fread(overlay_files[i])
# sapply(df[,c("ok","lm","cubist","svm")],function(x,y) rmse(x,y),y=df$TemperatureC)
diff<-sapply(df[,c("ok","lm","cubist","svm")],function(x,y) me(x,y),y=df$TemperatureC)
diff<-round(diff,2)
diff$'Station ID'<-overlay_station[i]
df.diff<-data.frame(diff)
df.diff<-df.diff[,c(5,1,2,3,4)]

write.table(df.diff,
            file = fname,
            sep = ",",
            row.names = FALSE,
            col.names = !file.exists(fname),
            append = TRUE)  
# sapply(df[,c("ok","lm","cubist","svm")],function(x,y) quant(x,y),y=df$TemperatureC)
# sapply(df[,c("ok","lm","cubist","svm")],function(x,y) R2(x,y),y=df$TemperatureC)
}

