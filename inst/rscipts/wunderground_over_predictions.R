library(data.table)
library(raster)
library(dplyr)

data("wunderground_meta")

lm_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/linear_mod/Predictions"
cubist_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/Cubist_mod/Predictions"
svm_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/SVM_radial_mod/Predictions"
ok_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/ok_model/prediction"
predictions<-c(lm_predictions,cubist_predictions,svm_predictions,ok_predictions)
model_names<-c("lm","cubist","svm","ok")

get_predictions_day<-function(date){  
  ml_raster<-paste0(predictions[1:3],"/temperature_lm_pca_harmonie",date,".grd")
  ok_raster<-paste0(predictions[4],"/temperature_kriging_pca_harmonie",date,".grd")
  st_raster<-c(ml_raster,ok_raster)
  return(st_raster)
}
#Haarweg
haarweg<-fread("/nobackup/users/dirksen/data/Veenkampen/Haarweg_daymean.txt")
haarweg$date<-as.Date(paste0(haarweg$year,"-",haarweg$month,"-",haarweg$day))
day_n<-unique(haarweg$date)
haarweg_meta<-readRDS("/nobackup/users/dirksen/data/Veenkampen/haarweg_meta.rds")
fname<-paste0("/nobackup/users/dirksen/data/GGD_temperature_ams/GGD_over_predictions/haarweg_over_predictions.txt")
haarweg_sp<-haarweg
haarweg_sp$lon<-haarweg_meta$Longitude
haarweg_sp$lat<-haarweg_meta$Latitude

for(i in 1:length(day_n)){
  st_raster<-get_predictions_day(day_n[i])
  haarweg_sp_n<-haarweg_sp[which(haarweg_sp$date==day_n[i]),]
  coordinates(haarweg_sp_n)<-~lon+lat
  crs(haarweg_sp_n)<-haarweg_meta$EPSG
  
  if(file.exists(st_raster[1])){
    raster_prediction<-stack(st_raster) 
    names(raster_prediction)<-model_names
    spdf<-spTransform(haarweg_sp_n,crs(raster_prediction))
    
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
}  

haarweg_out<-fread("/nobackup/users/dirksen/data/GGD_temperature_ams/GGD_over_predictions/haarweg_over_predictions.txt")

df<-haarweg_out %>% 
  select(Tg,ok,lm,svm,cubist) %>%
  summarize(wur=mean(Tg),
            ok=mean(ok),
            lm=mean(lm),
            svm=mean(svm),
            cubist=(mean(cubist)))

#Veenkampen
veenkampen<-fread("/nobackup/users/dirksen/data/Veenkampen/daymean.txt")
veenkampen<-veenkampen[!which(veenkampen$nobs<130),]
day_n<-unique(veenkampen$date)
fname<-paste0("/nobackup/users/dirksen/data/GGD_temperature_ams/GGD_over_predictions/veenkampen_over_predictions.txt")
meta<-readRDS("/nobackup/users/dirksen/data/Veenkampen/veenkampen_meta.rds")
veenkampen_sp<-veenkampen
veenkampen_sp$lon<-meta$Longitude
veenkampen_sp$lat<-meta$Latitude

for(i in 1:length(day_n)){
  st_raster<-get_predictions_day(day_n[i])
  veenkampen_sp_n<-veenkampen_sp[which(veenkampen_sp$date==day_n[i]),]
  coordinates(veenkampen_sp_n)<-~lon+lat
  crs(veenkampen_sp_n)<-meta$EPSG
  
  if(file.exists(st_raster[1])){
    raster_prediction<-stack(st_raster) 
    names(raster_prediction)<-model_names
    spdf<-spTransform(veenkampen_sp_n,crs(raster_prediction))
    
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
}

veenkampen_out<-fread("/nobackup/users/dirksen/data/GGD_temperature_ams/GGD_over_predictions/veenkampen_over_predictions.txt")

df<-veenkampen_out %>% 
  select(Tg,ok,lm,svm,cubist) %>%
  summarize(wur=mean(Tg),
            ok=mean(ok),
            lm=mean(lm),
            svm=mean(svm),
            cubist=(mean(cubist)))
#GGD Amsterdam
GGDmeta<-fread("/nobackup/users/dirksen/data/GGD_temperature_ams/metadata_GGD_Amsterdam.txt")
Ams2016<-readRDS("/nobackup/users/dirksen/data/GGD_temperature_ams/daymean/GGD_Amsterdam_daymean_2016_2017.rds")
Ams2015<-readRDS("/nobackup/users/dirksen/data/GGD_temperature_ams/daymean/GGD_Amsterdam_daymean_2014_2015.rds")
GGDAms<-rbind(Ams2015,Ams2016)

GGDAms_sp<-merge(GGDmeta,GGDAms,by.x="station_number",by.y="STN")

min_day<-GGDAms_sp %>% group_by(station_number) %>% filter(Day %in% min(Day))
max_day<-GGDAms_sp %>% group_by(station_number) %>% filter(Day %in% max(Day))

fname<-paste0("/nobackup/users/dirksen/data/GGD_temperature_ams/GGD_over_predictions/GGD_over_predictions.txt")

day_n<-unique(GGDAms_sp$Day)

for(i in 1:length(day_n)){
st_raster<-get_predictions_day(day_n[i])
GGDAms_sp_n<-GGDAms_sp[which(GGDAms_sp$Day==day_n[i]),]
coordinates(GGDAms_sp_n)<-~RD_x+RD_y
crs(GGDAms_sp_n)<-CRS("+init=epsg:28992")  

if(file.exists(st_raster[1])){
  raster_prediction<-stack(st_raster) 
  names(raster_prediction)<-model_names
  spdf<-spTransform(GGDAms_sp_n,crs(raster_prediction))
  
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
}

GGD_output<-fread("/nobackup/users/dirksen/data/GGD_temperature_ams/GGD_over_predictions/GGD_over_predictions.txt")

df<-GGD_output %>% 
  select(station_name,mean,ok,lm,svm,cubist) %>% group_by(station_name) %>%
  summarize(ggd=mean(mean),
            ok=mean(ok),
            lm=mean(lm),
            svm=mean(svm),
            cubist=(mean(cubist)))
#looping through all the raster predictions
#(1) starting with 1 station (file.nr loop)
#(2) and looping through the dates (i loop)
#list with all the station locations and their meta data
wur_files<-list.files("/nobackup/users/dirksen/data/wunderground/Daymean/",full.names = TRUE)
wur_names<-gsub(".txt","",list.files("/nobackup/users/dirksen/data/wunderground/Daymean/",full.names = FALSE))
wur_df<-data.frame(wur_files,wur_names)
wur_df<-data.frame(lapply(wur_df,as.character),stringsAsFactors = FALSE)
wur_df<-merge(wunderground_meta,wur_df,by.x="Station.ID",by.y="wur_names")

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

st_raster<-get_prediction_day(all_days[i])


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
library(ggrepel)
df<-fread("/nobackup/users/dirksen/data/wunderground/meta_data_start_stop.txt")

if (exists('.GeocodedInformation')) rm(.GeocodedInformation)
map<-get_map(location='Holland',zoom=7)


ggmap(map) + 
  geom_point(data=df,aes(x=Lon,y=Lat),size=2,alpha=0.5,col='red') +
  geom_label_repel(data=df,aes(x=Lon,y=Lat,label=`Station ID`),size=3)

library(leaflet)
leaflet(data = df) %>% addTiles() %>%
  addCircleMarkers(~Lon, ~Lat, popup = ~as.character(number_obs), label = ~as.character(number_obs),
                   labelOptions = labelOptions(noHide = TRUE,textOnly = TRUE,direction='auto',textsize = "9px"))

#############Analysis of the overlay files
library(dplyr)
R2<-function(pred,obs){1-sum((obs-pred)^2)/sum((obs-mean(pred))^2)}
rmse<-function(pred,obs){(mean(pred-obs)^2)^0.5}
me<-function(pred,obs){mean(pred-obs)}
(0.1,0.33,0.5,0.66,0.9)
main_path<-"/nobackup/users/dirksen/data/wunderground/overlay_predictions"
overlay_files<-list.files(main_path,pattern = ".txt",full.names = TRUE)
overlay_station<-gsub(".txt","",list.files(main_path,pattern = ".txt",full.names = FALSE))

fname<-"/nobackup/users/dirksen/data/wunderground/ME.txt"

df<-lapply(overlay_files,fread)
df<-do.call("rbind",df)

df.me<-df %>% do(data.frame(list("ok"=.$ok-.$TemperatureC, 
                         "lm"=.$lm-.$TemperatureC, 
                         "cubist" = .$cubist-.$TemperatureC,
                         "svm" = .$svm-.$TemperatureC)))

df.me<-stack(df.me)
df_meds<-ddply(df.me, .(ind),summarise,med=round(median(values,na.rm = TRUE),2))

p<-ggplot(df.me,aes(x= ind, y=values)) +
  geom_boxplot(fill = "skyblue2", colour= "#1F3552",outlier.size=-1) + #, outlier.size=-1
  scale_y_continuous(name = "ME", limits = c(-2.5,2)) +
  scale_x_discrete(name = "model") +
  theme_bw() +
  geom_text(data = df_meds, aes(x = ind, y = med, label = med),size = 2.5, vjust = -0.6)

ggsave(p,filename = "/nobackup/users/dirksen/data/Temperature/fig/ME_wunderground.png")

#quantiles
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

df<-df %>% mutate(quantile = ntile(lm,5))

ok<-df %>% group_by(.dots=c("quantile")) %>% do(data.frame(t(mean(.$ok-.$TemperatureC))))
lm<-df %>% group_by_(.dots=c("quantile")) %>% do(data.frame(t(mean(.$lm-.$TemperatureC))))
lm.min<-df %>% group_by_(.dots=c("quantile")) %>% do(data.frame(t(min(.$lm))))
lm.max<-df %>% group_by_(.dots=c("quantile")) %>% do(data.frame(t(max(.$lm))))
cubist<-df %>% group_by_(.dots=c("quantile")) %>% do(data.frame(t(mean(.$cubist-.$TemperatureC))))
svm<-df %>% group_by_(.dots=c("quantile")) %>% do(data.frame(t(mean(.$svm-.$TemperatureC))))

df.quants<-list(lm.min,lm.max,ok,lm,cubist,svm)
df.quants<-lapply(df.quants,data.frame)
df.quants<-Reduce(function(x,y) merge(x,y,by="quantile"),df.quants)
names(df.quants)<-c("Q","Tmin","Tmax","ok","lm","cubist","svm")
df.quants.round<-round_df(df.quants,2)

write.table(df.quants.round,
            file = "/nobackup/users/dirksen/data/wunderground/quant5.txt",
            sep = ",",
            row.names = FALSE,
            col.names = TRUE) 
#brier skill score
library(verification)
main_path<-"/nobackup/users/dirksen/data/wunderground/overlay_predictions"
overlay_files<-list.files(main_path,pattern = ".txt",full.names = TRUE)

df<-lapply(overlay_files,fread)
df<-do.call("rbind",df)

hist(df$TemperatureC)
low.treshhold<- as.numeric(quantile(df$TemperatureC,c(0,0.2,0.4,0.6,0.8)))
upper.treshhold<- as.numeric(quantile(df$TemperatureC,c(0.2,0.4,0.6,0.8,1)))

obs<-as.numeric(df$TemperatureC>low.treshhold[1] & df$TemperatureC<upper.treshhold[1])
pred<-lapply(df[,c("ok","lm","cubist","svm")],function(x) as.numeric(x>low.treshhold[1] & x<upper.treshhold[1]))

bs<-lapply(pred,FUN=brier,obs=obs,simplify = FALSE)

bs.sub<-lapply(bs,"[",c("bs","bs.reliability","bs.resol","bs.uncert"))

df.bs<-data.frame(matrix(unlist(bs.sub),nrow=4,byrow=TRUE),stringsAsFactors = FALSE)
colnames(df.bs)<-c("BS","REL","RES","UNC")
rownames(df.bs)<-c("ok","lm","cubist","svm")

round(df.bs,4)
# sapply(df[,c("ok","lm","cubist","svm")],function(x,y) rmse(x,y),y=df$TemperatureC)
# diff<-sapply(df[,c("ok","lm","cubist","svm")],function(x,y) me(x,y),y=df$TemperatureC)


# diff<-round(diff,2)
# diff$'Station ID'<-overlay_station[i]
# df.diff<-data.frame(diff)
# df.diff<-df.diff[,c(5,1,2,3,4)]
# 
# write.table(df.diff,
#             file = fname,
#             sep = ",",
#             row.names = FALSE,
#             col.names = !file.exists(fname),
#             append = TRUE)  
# sapply(df[,c("ok","lm","cubist","svm")],function(x,y) quant(x,y),y=df$TemperatureC)
# sapply(df[,c("ok","lm","cubist","svm")],function(x,y) R2(x,y),y=df$TemperatureC)

