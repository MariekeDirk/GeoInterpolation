#This example includes the DTR and spatial buffers
#total of 48 predictors
rm(list=ls())

library(raster)
library(sp)
library(rgdal)
library(data.table)
library(config)
library(GeoInterpolation)
library(dplyr) # join function
library(caret)
library(lubridate)
library(GSIF)
library(ranger)
library(geoR)
library(spatialEco)
#https://hangouts.google.com/hangouts/_/calendar/dG9tLmhlbmdsQGdtYWlsLmNvbQ.2inu37ooolahjll1egan41nob1?hs=121
cfg <- config::get(file = "/nobackup/users/dirksen/Temperature/GeoInterpolation/config/config.yml")

#importing the datasets for the interpolation
devtools::load_all()
data("temperature_climate")
data("coords_aws")

temperature_climate<-temperature_climate[complete.cases(temperature_climate),]

#Auxiliary data v
aux.df<-fread("/nobackup/users/dirksen/data/auxcillary_NED/aux_coords_aws.txt")
aux.df$datum<-as.Date(aux.df$datum)

#(1)merge temperature and auxiliary data v
temp_aux_df<-merge(temperature_climate,aux.df,by.x=c("STN","Datum"),by.y=c("STN","datum"))

#(2)make data spatial v
temp_aux_sp<-temp_aux_df
coordinates(temp_aux_sp)<-~RDN_X+RDN_Y
projection(temp_aux_sp)<-cfg$pro

#(3)make formula v
variables<-names(temp_aux_sp)[8:55]
fm<-as.formula(paste0("Tg ~",paste(variables,collapse='+')))

#(4)run for subset
temp_aux_df<-temp_aux_df[complete.cases(temp_aux_df),]
temp_aux_df.sub<-temp_aux_df[sample.int(size=50000,nrow(temp_aux_df)),]

#Rangersp model 
m.Tg<-ranger(fm,temp_aux_df.sub, 
             num.trees=100, mtry=10,seed=1,
             quantreg = TRUE, importance='impurity',
             case.weights=rep(1/(0.13),length(temp_aux_df.sub$STN))) #total uncertainty error of temperature measurements=0.13


print(m.Tg$variable.importance)
varimp<-data.table(m.Tg$variable.importance)
varimp$name<-names(m.Tg$variable.importance)
varimp<-setorder(varimp,V1)
varimp$nr<-seq(1,48,1)
ggplot(varimp,aes(V1,nr))+
  geom_point() + 
  geom_text(aes(label=name),hjust=-0.2,vjust=1) +
  xlab("variable importance") +
  ylab("ranking")

print(m.Tg)


print(paste0("RMSE = ", round(sqrt(m.Tg$prediction.error),2)))
             # , 
             # case.weights=rep(1/(0.1^2),length(data.aux.df.sub$STN)), 
             # quantreg = TRUE)
#########################################################
###########INTERPOLATION EXAMPLE: PREDICT WITH THE MODEL#
#########################################################
datum<-as.Date("2000-11-01")
spdf_aux<-load_auxiliarydata(datum)

p.Tg<-predict(m.Tg,spdf_aux@data,type="quantiles",quantiles=c(0.159,0.5,0.841)) #prediction

spdf_aux@data<-do.call(cbind, list(spdf_aux@data, 
                                   data.frame(p.Tg$predictions[,2]),
                                   data.frame((p.Tg$predictions[,3]-p.Tg$predictions[,1])/2))) # add the data
gridded(spdf_aux)<-TRUE                   #spatialpixeldataframe
st<-stack(spdf_aux)                       #convert to raster

#plotting routine
points<-temp_aux_df[which(temp_aux_df$Datum==datum),]
coordinates(points)<-~RDN_X+RDN_Y
proj4string(points)<-cfg$pro

r.median<-st[[11]]
#Values outside measurement area's are currently overpredicted by the model, 
#the wow data is needed to add information of the cities 
#To do: add modis lst data 8 day climatology
#1) overlay with the data
#2)smooth 8 days with a spline for the predictions (otherwise large jumps)

# library(leaflet)
# 
# pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r.median),
#                     na.color = "transparent")
# leaflet() %>% addTiles() %>%
#   addRasterImage(r.median, colors = pal, opacity = 0.8) %>%
#   addLegend(pal = pal, values = values(r.median),
#             title = "Surface temp")

#problem with the prediction is the missing data values in spdf_aux

# aux.ov = sp::over(inputdata , spdf_aux)
# df<-cbind(data.frame(inputdata),aux.ov)
# names(df)<-c("STN","Datum",
#              "Tg","DS_ALT",
#              "DS_NAME","DS_LON",
#              "DS_LAT","optional",
#              "RDN_X","RDN_Y",
#              "optional.1","Height",
#              "Distsea","Population",
#              "Albedo","Roughness",
#              "Precipitation_monthly","NDVI","Radiation")
# 
# write.table(df,
#             file = fname,
#             row.names = FALSE,
#             col.names = !file.exists(fname),
#             append = TRUE,
#             sep = ",")

