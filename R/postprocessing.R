#'Load data paths
#'
#'@title Load data paths
#'@description Loading the interpolation results file names into memory. See also 
#'\link[GeoInterpolation]{calc_save_climatology} and \link[GeoInterpolation]{plot_climatology}
#'@param results.aux results from the auxiliary data runs
#'@param results.pca results from the principle component runs
#'@author Marieke Dirksen
#'@export

load.data.paths<-function(results.aux=results.aux,results.pca=results.pca){
  ok<-list.files(paste0(results.aux,"ok_model/prediction/"),pattern=".grd",full.names = TRUE)
  
  #Prediction data from the Auxiliary models
  lin.aux<-list.files(paste0(results.aux,"linear_mod/Predictions/"),
                      pattern="^temperature_lm_pca_harmonie.*.grd",full.names = TRUE)
  cubist.aux<-list.files(paste0(results.aux,"Cubist_mod/Predictions/"),
                         pattern="^temperature_lm_pca_harmonie.*.grd",full.names = TRUE)
  svm.aux<-list.files(paste0(results.aux,"SVM_radial_mod/Predictions/"),
                      pattern="^temperature_lm_pca_harmonie.*.grd",full.names = TRUE)
  ked.aux<-list.files(paste0(results.aux,"ked_model/prediction/"),pattern=".grd",full.names = TRUE)
  
  #Prediction data from the PCA models
  lin.pca.f1<-list.files(paste0(results.pca,"linear_mod_PCA_f1/Predictions/"),
                         pattern="^temperature_lm_pca_harmonie.*.grd",full.names = TRUE)
  lin.pca.f2<-list.files(paste0(results.pca,"linear_mod_PCA_f2/Predictions/"),pattern=".grd",full.names = TRUE)
  lin.pca.f3<-list.files(paste0(results.pca,"linear_mod_PCA_f3/Predictions/"),pattern=".grd",full.names = TRUE)
  ked.pca.f1<-list.files(paste0(results.pca,"ked_model_PCA_f1/prediction/"),pattern=".grd",full.names = TRUE)
  ked.pca.f2<-list.files(paste0(results.pca,"ked_model_PCA_f2/prediction/"),pattern=".grd",full.names = TRUE)
  ked.pca.f3<-list.files(paste0(results.pca,"ked_model_PCA_f3/prediction/"),pattern=".grd",full.names = TRUE)
  
  return(list("ok"=ok,
              "lin.aux"=lin.aux,
              "cubist.aux"=cubist.aux,
              "svm.aux"=svm.aux,
              "ked.aux"=ked.aux,
              "lin.pca.f1"=lin.pca.f1,
              "lin.pca.f2"=lin.pca.f2,
              "lin.pca.f3"=lin.pca.f3,
              "ked.pca.f1"=ked.pca.f1,
              "ked.pca.f2"=ked.pca.f2,
              "ked.pca.f3"=ked.pca.f3))
}

#'Calculate climatology
#'
#'@title Calculate climatology
#'@description calculate the climatology for the entire run period, uses \link[GeoInterpolation]{load.data.paths}. 
#'See also \link[GeoInterpolation]{plot_climatology}
#'@param path.to.file all the full names of the files to stack
#'@param file.to.write name of the climatology file to save
#'@author Marieke Dirksen
#'@export
#'
calc_save_climatology <- function(path.to.file , file.to.write){
  requireNamespace("raster", quietly = TRUE)
  print("stacking files")
  st <- stack(path.to.file)
  print("calculating mean")
  st.mean <- stackApply(st, 
                        1, 
                        fun = mean)
  print(paste0("writing raster for", file.to.write))
  writeRaster(st.mean, 
              file.to.write, 
              overwrite = TRUE)
  return(st.mean)
}

#'Plotting climatology
#'
#'@title plot climatology
#'@description plots the climatology calculated from \link[GeoInterpolation]{calc_save_climatology}. 
#'See also \link[GeoInterpolation]{load.data.paths}
#'@param list.of.files climatologies (grd)
#'@param names.attr names of the different subplots
#'@param layout how should the plots be ordered?
#'@author Marieke Dirksen
#'@export
plot_climatology <- function(list.of.files,
                             names.attr,
                             layout,
                             at=seq(9.5,12.3,length=30)){
  requireNamespace("raster", quietly = TRUE)
  requireNamespace("rasterVis", quietly = TRUE)
  
  st_stack<-stack(list.of.files)
  
  # png(png.to.write,width=2500,height=2000,res=300)
  p<-levelplot(st_stack,
            layout=layout,
            scales=list(draw=FALSE),
            names.attr=names.attr,
            at=at,
            col.regions=colorRampPalette(c("blue","cyan","green","yellow","orange","red"))
  )
  return(p)
  # dev.off()
  #print p
  
}

#'Loading the statistical summary files
#'
#'@title load_statistical_summary
#'@description Loads all de text files with RMSE and R2 values into memory. 
#'@param results.aux location were the text files are 
#'@param results.pca location were the text files from the PCA analysis are
#'@author Marieke Dirksen
#'@export
#'
#'
load_statistical_summary<-function(results.aux = results.aux, results.pca = results.pca){
  requireNamespace("data.table", quietly = TRUE)
  #Auxiliary data results statistical summary
  ok.cv<-fread(paste0(results.aux,"ok_statistical_summary_cv.txt"))
  names(ok.cv)<-c("R2.cv"  ,   "RMSE.cv"  , "RMSEsd.cv", "ME.cv"   ,  "MEmean.cv","MAE","datum")
  ok.pred<-fread(paste0(results.aux,"ok_statistical_summary_pred.txt"))
  ok<-data.frame(ok.pred,ok.cv)

  
  cubist<-fread(paste0(results.aux,"cubist_statistical_summary.txt"))
  lm<-fread(paste0(results.aux,"lm_statistical_summary.txt"))
  svm<-fread(paste0(results.aux,"svmRadial_statistical_summary.txt"))
  
  
  #PCA data results statistical summary
  # ked.cv.f1<-fread(paste0(results.pca,"ked_model_PCA_f1/ked_statistical_summary_cv.txt"))
  # names(ked.cv.f1)<-c("R2.cv"  ,   "RMSE.cv"  , "RMSEsd.cv", "ME.cv"   ,  "MEmean.cv","datum")
  # ked.pred.f1<-fread(paste0(results.pca,"ked_model_PCA_f1/ked_statistical_summary_pred.txt"))
  # ked.f1<-data.frame(ked.pred.f1,ked.cv.f1)
  # 
  # ked.cv.f2<-fread(paste0(results.pca,"ked_model_PCA_f2/ked_statistical_summary_cv.txt"))
  # names(ked.cv.f2)<-c("R2.cv"  ,   "RMSE.cv"  , "RMSEsd.cv", "ME.cv"   ,  "MEmean.cv","datum")
  # ked.pred.f2<-fread(paste0(results.pca,"ked_model_PCA_f2/ked_statistical_summary_pred.txt"))
  # ked.f2<-data.frame(ked.pred.f2,ked.cv.f2)
  # 
  # ked.cv.f3<-fread(paste0(results.pca,"ked_model_PCA_f3/ked_statistical_summary_cv.txt"))
  # names(ked.cv.f3)<-c("R2.cv"  ,   "RMSE.cv"  , "RMSEsd.cv", "ME.cv"   ,  "MEmean.cv","datum")
  # ked.pred.f3<-fread(paste0(results.pca,"ked_model_PCA_f3/ked_statistical_summary_pred.txt"))
  # ked.f3<-data.frame(ked.pred.f3,ked.cv.f3)
  # 
  # lm.f1<-fread(paste0(results.pca,"linear_mod_PCA_f1/linear_statistical_summary.txt"))
  # lm.f2<-fread(paste0(results.pca,"linear_mod_PCA_f2/linear_statistical_summary.txt"))
  # lm.f3<-fread(paste0(results.pca,"linear_mod_PCA_f3/linear_statistical_summary.txt"))
  
  return(list("ok"=ok,
              # "ked"=ked,
              "lm"=lm,
              "cubist"=cubist,
              "svm"=svm))
              # "ked.f1"=ked.f1,
              # "ked.f2"=ked.f2,
              # "ked.f3"=ked.f3,
              # "lm.f1"=lm.f1,
              # "lm.f2"=lm.f2,
              # "lm.f3"=lm.f3))
  
}
