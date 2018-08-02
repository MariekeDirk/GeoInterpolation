library(raster)
library(sp)
library(rgdal)
library(data.table)
library(config)
library(GeoInterpolation)
library(dplyr) # join function
library(caret)

cfg <- config::get(file = "/nobackup/users/dirksen/Temperature/GeoInterpolation/config/config.yml")

#importing the datasets for the interpolation
devtools::load_all()
data("temperature_climate")
data("coords_aws")

pca_harm<-readRDS(cfg$pca_harmonie_mask)
proj4string(pca_harm$map)<-cfg$pro
spdf_pca_harm<-as(pca_harm$map,"SpatialGridDataFrame")

# plot(pca_harm$map[[1:4]],main=c("PC1 (58.4%)","PC2 (22.2%)","PC3 (5.7%)","PC4 (5.2%)"))

# f<-formula("Tint ~ PC1")
f3<-formula("Tint ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7")
f2<-formula("Tint ~ PC1 + PC2 + PC3 + PC4 ")
f1<-formula("Tint ~ PC1 + PC2 ")
f_options<-list("f1"=f1,"f2"=f2,"f3"=f3)

datums<-unique(temperature_climate$Datum)
I<-which(datums>"1990-01-01")
#I<-which(as.numeric(datums)>20120722)
# I<-which(as.numeric(datums)==19060822)
datums<-datums[I]


#1950==number 40121

temperature_climate<-temperature_climate[complete.cases(temperature_climate),]
# date_sations_count<-table(temperature_climate$Datum)
# date_sations_count<-data.frame(date_sations_count)
# names(date_sations_count)<-c("Date","Count")
# date_sations_count$Date<-as.POSIXct(date_sations_count$Date,format="%Y%m%d")
# ggplot(date_sations_count,aes(Date,Count))+geom_point()+
#   scale_x_datetime() + xlab("") + ylab("Number of observations") +geom_line()

#main path savedir
lm_f1<-"/nobackup/users/dirksen/data/Temperature/PCA_results/linear_mod_PCA_f1/"
lm_f2<-"/nobackup/users/dirksen/data/Temperature/PCA_results/linear_mod_PCA_f2/"
lm_f3<-"/nobackup/users/dirksen/data/Temperature/PCA_results/linear_mod_PCA_f3/"
lm_f<-list("lm_f1"=lm_f1,"lm_f2"=lm_f2,"lm_f3"=lm_f3)

ked_f1<-"/nobackup/users/dirksen/data/Temperature/PCA_results/ked_model_PCA_f1/"
ked_f2<-"/nobackup/users/dirksen/data/Temperature/PCA_results/ked_model_PCA_f2/"
ked_f3<-"/nobackup/users/dirksen/data/Temperature/PCA_results/ked_model_PCA_f3/"
ked_f<-list("ked_f1"=ked_f1,"ked_f2"=ked_f2,"ked_f3"=ked_f3)

for (i in 1:length(datums)){

datum<-datums[i]
print(datum)
inputdata<-temperature_climate[which(temperature_climate$Datum==datum),]
inputdata<-inputdata[complete.cases(inputdata$Tg),]

if(nrow(inputdata)>3){
inputdata<-inner_join(inputdata,coords_aws,by="STN")
coordinates(inputdata)<-~RDN_X+RDN_Y
projection(inputdata)<-cfg$pro

#Cubist model
# cubist.mod<-cubist_model_caret(groundstations = inputdata,
#                                   formula=f,
#                                   variable="Tg",
#                                   grid_prediction=spdf_pca_harm,
#                                   method.overlay = 'extract'
# )
# 
# cubist.mod$statistical_summary$datum<-datum
# 
# writeRaster(cubist.mod$spatial,paste0("/nobackup/users/dirksen/data/Temperature/MLmodels/Cubist_mod_PCA/Predictions/temperature_lm_pca_harmonie",datum,".grd"),overwrite=TRUE)
# 
# saveRDS(cubist.mod$model,paste0("/nobackup/users/dirksen/data/Temperature/MLmodels/Cubist_mod_PCA/Model/model_",datum,".rds"))
# 
# write.table(cubist.mod$statistical_summary,"/nobackup/users/dirksen/data/Temperature/MLmodels/Cubist_statistical_summary.txt",
#             row.names=FALSE,col.names = !file.exists("/nobackup/users/dirksen/data/Temperature/MLmodels/Cubist_mod_PCA/statistical_summary.txt"),
#             append = TRUE,sep=",")

#svmRadial model
# svmRadial.mod<-linear_model_caret(groundstations = inputdata,
#                             formula=f,
#                             variable="Tg",
#                             grid_prediction=spdf_pca_harm,
#                             method.overlay = 'extract'
# )
# 
# svmRadial.mod$statistical_summary$datum<-datum
# writeRaster(svmRadial.mod$spatial,paste0("/nobackup/users/dirksen/data/Temperature/MLmodels/svmRadial_mod_PCA/Predictions/temperature_lm_pca_harmonie",datum,".grd"),overwrite=TRUE)
# 
# saveRDS(svmRadial.mod$model,paste0("/nobackup/users/dirksen/data/Temperature/MLmodels/svmRadial_mod_PCA/Model/model_",datum,".rds"))
# 
# write.table(svmRadial.mod$statistical_summary,"/nobackup/users/dirksen/data/Temperature/MLmodels/svmRadial_statistical_summary.txt",
#             row.names=FALSE,col.names = !file.exists("/nobackup/users/dirksen/data/Temperature/MLmodels/svmRadial_mod_PCA/statistical_summary.txt"),
#             append = TRUE,sep=",")

#linear model

for(j in 1:length(f_options)){
lin.mod<-linear_model_caret(groundstations = inputdata,
                            formula=f_options[[j]],
                            variable="Tg",
                            grid_prediction=spdf_pca_harm,
                            method.overlay = 'extract'
)

lin.mod$statistical_summary$datum<-datum
writeRaster(lin.mod$spatial,paste0(lm_f[[j]],"Predictions/temperature_lm_pca_harmonie",datum,".grd"),overwrite=TRUE)

saveRDS(lin.mod$model,paste0(lm_f[[j]],"Model/model_",datum,".rds"))

write.table(lin.mod$statistical_summary,paste0(lm_f[[j]],"linear_statistical_summary.txt"),
            row.names=FALSE,col.names = !file.exists(paste0(lm_f[[j]],"linear_statistical_summary.txt")),
            append = TRUE,sep=",")

#Kriging external Drift
out.ked_pca<-interpolation_krigingdrift(groundstations = inputdata,
                                        formula=f_options[[j]],
                                        variable="Tg",
                                        grid_drift=spdf_pca_harm,
                                        method.overlay = 'extract',
                                        conditional.sim=FALSE)
out.ked_pca$statistical_summary_cv$datum<-datum

r<-raster(out.ked_pca$spatial)
writeRaster(r,paste0(ked_f[[j]],"prediction/temperature_kriging_pca_harmonie",datum,".grd"),overwrite=TRUE)
write.csv(out.ked_pca$cv$krige.cv_output,
          paste0(ked_f[[j]],"cv/crossval_",datum,".txt"),
          row.names = FALSE)
write.table(out.ked_pca$statistical_summary_cv,paste0(ked_f[[j]],"ked_statistical_summary_cv.txt"),
            row.names=FALSE,col.names = !file.exists(paste0(ked_f[[j]],"ked_statistical_summary_cv.txt")),
            append = TRUE,sep=",")
write.table(out.ked_pca$statistical_summary_pred,paste0(ked_f[[j]],"ked_statistical_summary_pred.txt"),
            row.names=FALSE,col.names = !file.exists(paste0(ked_f[[j]],"ked_statistical_summary_pred.txt")),
            append = TRUE,sep=",")
}





#Ordinary Kriging
# out.ok<-interpolation_ordinarykriging(groundstations = inputdata,
#                                         variable="Tg",
#                                         grid_drift=spdf_pca_harm[1]
#                                          )
# out.ok$statistical_summary_cv$datum<-datum
# 
# r<-raster(out.ok$spatial)
# writeRaster(r,paste0("/nobackup/users/dirksen/data/Temperature/PCA_results/ok_model/prediction/temperature_kriging_pca_harmonie",datum,".grd"),overwrite=TRUE)
# write.csv(out.ok$cv$krige.cv_output,
#           paste0("/nobackup/users/dirksen/data/Temperature/PCA_results/ok_model/cv/crossval_",datum,".txt"),
#           row.names = FALSE)
# write.table(out.ok$statistical_summary_cv,"/nobackup/users/dirksen/data/Temperature/PCA_results/ok_statistical_summary_cv.txt",
#             row.names=FALSE,col.names = !file.exists("/nobackup/users/dirksen/data/Temperature/PCA_results/ok_statistical_summary_cv.txt"),
#             append = TRUE,sep=",")
# write.table(out.ok$statistical_summary_pred,"/nobackup/users/dirksen/data/Temperature/PCA_results/ok_statistical_summary_pred.txt",
#             row.names=FALSE,col.names = !file.exists("/nobackup/users/dirksen/data/Temperature/PCA_results/ok_statistical_summary_pred.txt"),
#             append = TRUE,sep=",")


} else print("less than 4 stations")
}
