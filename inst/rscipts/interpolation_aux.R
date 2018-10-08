library(raster)
library(sp)
library(rgdal)
library(data.table)
library(config)
library(GeoInterpolation)
library(dplyr) # join function
library(caret)
library(lubridate)

cfg <- config::get(file = "/nobackup/users/dirksen/Temperature/GeoInterpolation/config/config.yml")

#importing the datasets for the interpolation
devtools::load_all()
data("temperature_climate")
data("coords_aws")



datums<-unique(temperature_climate$Datum)
I<-which(datums>as.Date("1990-01-01"))
datums<-datums[I]


temperature_climate<-temperature_climate[complete.cases(temperature_climate),]
# date_sations_count<-table(temperature_climate$Datum)
# date_sations_count<-data.frame(date_sations_count)
# names(date_sations_count)<-c("Date","Count")
# date_sations_count$Date<-as.POSIXct(date_sations_count$Date,format="%Y%m%d")
# ggplot(date_sations_count,aes(Date,Count))+geom_point()+
#   scale_x_datetime() + xlab("") + ylab("Number of observations") +geom_line()
main_save_path<-"/nobackup/users/dirksen/data/Temperature/Aux_results/"    

fname<-"/nobackup/users/dirksen/data/Temperature/KNMIstations/temperature_auxiliary_over.txt"
for (i in  1:length(datums)){

datum<-datums[i]
print(datum)
spdf_aux<-load_auxiliarydata(datum)


inputdata<-temperature_climate[which(temperature_climate$Datum==datum),]
inputdata<-inputdata[complete.cases(inputdata$Tg),]
inputdata<-inner_join(inputdata,coords_aws,by="STN")
coordinates(inputdata)<-~RDN_X+RDN_Y
projection(inputdata)<-cfg$pro

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

# if(nrow(inputdata)>3){

# 
# 
# #Cubist model
# cubist.mod<-cubist_model_caret(groundstations = inputdata,
#                                formula=formula(paste0("Tint ~ ",names(spdf_aux)[1],"+",
#                                                       names(spdf_aux)[2],"+",
#                                                       names(spdf_aux)[3],"+",
#                                                       names(spdf_aux)[4],"+",
#                                                       names(spdf_aux)[5],"+",
#                                                       names(spdf_aux)[6],"+",
#                                                       names(spdf_aux)[7],"+",
#                                                       names(spdf_aux)[8])),
#                                variable="Tg",
#                                grid_prediction=spdf_aux,
#                                method.overlay = 'extract')
# 
# cubist.mod$statistical_summary$datum<-datum
# 
# writeRaster(cubist.mod$spatial,paste0(main_save_path,"Cubist_mod/Predictions/temperature_lm_pca_harmonie",datum,".grd"),overwrite=TRUE)
# 
# saveRDS(cubist.mod$model,paste0(main_save_path,"Cubist_mod/Model/model_",datum,".rds"))
# 
# write.table(cubist.mod$statistical_summary,paste0(main_save_path,"cubist_statistical_summary.txt"),
#             row.names=FALSE,col.names = !file.exists(paste0(main_save_path,"cubist_statistical_summary.txt")),
#             append = TRUE,sep=",")
# # 
# # #svmRadial model
# svmRadial.mod<-svmRadial_model_caret(groundstations = inputdata,
#                                   formula=formula(paste0("Tint ~ ",names(spdf_aux)[1],"+",
#                                                          names(spdf_aux)[2],"+",
#                                                          names(spdf_aux)[3],"+",
#                                                          names(spdf_aux)[4],"+",
#                                                          names(spdf_aux)[5],"+",
#                                                          names(spdf_aux)[6],"+",
#                                                          names(spdf_aux)[7],"+",
#                                                          names(spdf_aux)[8])),
#                                   variable="Tg",
#                                   grid_prediction=spdf_aux,
#                                   method.overlay = 'extract'
# )
# 
# svmRadial.mod$statistical_summary$datum<-datum
# writeRaster(svmRadial.mod$spatial,paste0(main_save_path,"SVM_radial_mod/Predictions/temperature_lm_pca_harmonie",
#                                          datum,".grd"),overwrite=TRUE)
# 
# saveRDS(svmRadial.mod$model,paste0(main_save_path,"SVM_radial_mod/Model/model_",
#                                    datum,".rds"))
# 
# write.table(svmRadial.mod$statistical_summary,paste0(main_save_path,"svmRadial_statistical_summary.txt"),
#             row.names=FALSE,col.names = !file.exists(paste0(main_save_path,"svmRadial_statistical_summary.txt")),
#             append = TRUE,sep=",")
# # 
# # #linear model
# lin.mod<-linear_model_caret(groundstations = inputdata,
#                             formula=formula(paste0("Tint ~ ",names(spdf_aux)[1],"+",
#                                                    names(spdf_aux)[2],"+",
#                                                    names(spdf_aux)[3],"+",
#                                                    names(spdf_aux)[4],"+",
#                                                    names(spdf_aux)[5],"+",
#                                                    names(spdf_aux)[6],"+",
#                                                    names(spdf_aux)[7],"+",
#                                                    names(spdf_aux)[8])),
#                             variable="Tg",
#                             grid_prediction=spdf_aux,
#                             method.overlay = 'extract'
# )
# 
# lin.mod$statistical_summary$datum<-datum
# writeRaster(lin.mod$spatial,paste0("/nobackup/users/dirksen/data/Temperature/Aux_results/linear_mod/Predictions/temperature_lm_pca_harmonie",datum,".grd"),overwrite=TRUE)
# 
# saveRDS(lin.mod$model,paste0("/nobackup/users/dirksen/data/Temperature/Aux_results/linear_mod/Model/model_",datum,".rds"))
# #
# write.table(lin.mod$statistical_summary,paste0(main_save_path,"lm_statistical_summary.txt"),
#             row.names=FALSE,col.names = !file.exists(paste0(main_save_path,"lm_statistical_summary.txt")),
#             append = TRUE,sep=",")
# 
# if(nrow(inputdata)>9){
# #Kriging with auxcillary data
#   
  # out.ked_pca<-interpolation_krigingdrift(groundstations = inputdata,
  #                                         formula=formula(paste0("Tint ~ ",names(spdf_aux)[1],"+",
  #                                                                names(spdf_aux)[2],"+",
  #                                                                names(spdf_aux)[3],"+",
  #                                                                names(spdf_aux)[4],"+",
  #                                                                names(spdf_aux)[5],"+",
  #                                                                names(spdf_aux)[6],"+",
  #                                                                names(spdf_aux)[7],"+",
  #                                                                names(spdf_aux)[8])),
  #                                         variable="Tg",
  #                                         grid_drift=spdf_aux,
  #                                         method.overlay = 'extract',
  #                                         conditional.sim=FALSE)
  # out.ked_pca$statistical_summary_cv$datum<-datum
  # 
  # r<-raster(out.ked_pca$spatial)
  # writeRaster(r,paste0("/nobackup/users/dirksen/data/Temperature/Aux_results/ked_model/prediction/temperature_kriging_pca_harmonie",datum,".grd"),overwrite=TRUE)
  # write.csv(out.ked_pca$cv$krige.cv_output,
  #           paste0("/nobackup/users/dirksen/data/Temperature/Aux_results/ked_model/cv/crossval_",datum,".txt"),
  #           row.names = FALSE)
  # write.table(out.ked_pca$statistical_summary_cv,"/nobackup/users/dirksen/data/Temperature/Aux_results/ked_statistical_summary_cv.txt",
  #             row.names=FALSE,col.names = !file.exists("/nobackup/users/dirksen/data/Temperature/Aux_results/ked_statistical_summary_cv.txt"),
  #             append = TRUE,sep=",")
  # write.table(out.ked_pca$statistical_summary_pred,"/nobackup/users/dirksen/data/Temperature/Aux_results/ked_statistical_summary_pred.txt",
  #             row.names=FALSE,col.names = !file.exists("/nobackup/users/dirksen/data/Temperature/Aux_results/ked_statistical_summary_pred.txt"),
  #             append = TRUE,sep=",")
# 
#Ordinary Kriging
out.ok<-interpolation_ordinarykriging(groundstations = inputdata,
                                        variable="Tg",
                                        grid_drift=spdf_aux[1]
                                         )
out.ok$statistical_summary_cv$datum<-datum

# r<-raster(out.ok$spatial)
# writeRaster(r,paste0("/nobackup/users/dirksen/data/Temperature/Aux_results/ok_model/prediction/temperature_kriging_pca_harmonie",datum,".grd"),overwrite=TRUE)
write.csv(out.ok$cv$krige.cv_output,
          paste0("/nobackup/users/dirksen/data/Temperature/Aux_results/ok_model/cv/crossval_",datum,".txt"),
          row.names = FALSE)
write.table(out.ok$statistical_summary_cv,"/nobackup/users/dirksen/data/Temperature/Aux_results/ok_statistical_summary_cv.txt",
            row.names=FALSE,col.names = !file.exists("/nobackup/users/dirksen/data/Temperature/Aux_results/ok_statistical_summary_cv.txt"),
            append = TRUE,sep=",")
write.table(out.ok$statistical_summary_pred,"/nobackup/users/dirksen/data/Temperature/Aux_results/ok_statistical_summary_pred.txt",
            row.names=FALSE,col.names = !file.exists("/nobackup/users/dirksen/data/Temperature/Aux_results/ok_statistical_summary_pred.txt"),
            append = TRUE,sep=",")
# } else print("no fit for kriging, only linear model output")
# 
# } else print("less than 4 stations")
}
