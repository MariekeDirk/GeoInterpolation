library(raster)
library(rts)

results.aux<-"/nobackup/users/dirksen/data/Temperature/Aux_results/"
# results.pca<-"/nobackup/users/dirksen/data/Temperature/PCA_results/"

#calculate and save the climatology for all files
all.paths<-paste0(results.aux,c("ok_model/prediction/",
                                "linear_mod/Predictions/",
                                "Cubist_mod/Predictions/",
                                "SVM_radial_mod/Predictions/"))
all.files<-mapply(list.files,
                  all.paths,
                  full.names = TRUE, 
                  pattern=".grd",
                  SIMPLIFY = FALSE)

files.to.write<-paste0("/nobackup/users/dirksen/data/Temperature/climatology/",c("ok","linear","cubist","svm"),".grd")

clim.results<-mapply(calc_save_climatology,
                    file.to.write=files.to.write,
                    path.to.file=all.files,
                    SIMPLIFY = FALSE)

# aux.list<-grep(list.files("/nobackup/users/dirksen/data/Temperature/climatology/",
#                           pattern="*.grd",full.names = TRUE),
#                           pattern = "pca",inv=TRUE,value=TRUE)
# pca.list<-list.files("/nobackup/users/dirksen/data/Temperature/climatology/",pattern = "pca.*.grd",
#                      full.names = TRUE)
# png("/nobackup/users/dirksen/data/Temperature/fig/ok.png",width=2000,height=2000,res=300)
# plot_climatology(list.of.files = aux.list[4],
#                  names.attr = "ordinary kriging",
#                  layout=c(1,1),
#                  at=seq(9.5,11.5,length=30)
#                  )
# dev.off()

st<-stack(files.to.write)
png("/nobackup/users/dirksen/data/Temperature/fig/aux.png",width=2100,height=2000,res=300)
plot_climatology(list.of.files = st,
                 names.attr = c("(a) ok","(b) lm" ,"(c) cubist", "(d) svm"),
                 layout=c(2,2))
dev.off()

# png("/nobackup/users/dirksen/data/Temperature/fig/pca2.png",width=2500,height=2000,res=300)
plot_climatology(list.of.files = pca.list,
                 names.attr = c("(a) ked f1","(b) ked f2" ,"(c) ked f3", "(d) lm f1", "(e) lm f2", "(f) lm f3"),
                 layout=c(3,2),
                 at=seq(9.5,11.5,length=30)
                 )
                 
# dev.off()


#Statistical summary
stat<-load_statistical_summary(results.aux = results.aux)

tables_paper<-function(statistical_summary){
  ###########auxiliary data median and sd################# 
  #R2 values for auxiliary data
  ok.R2.pred<-median(statistical_summary$ok$R2,na.rm = TRUE)
  ok.R2.val<-median(statistical_summary$ok$R2.cv,na.rm = TRUE)
  
  lm.R2.pred<-median(statistical_summary$lm$RsquaredApparent,na.rm = TRUE)
  lm.R2.val<-median(statistical_summary$lm$Rsquared,na.rm = TRUE) 
  # ked.R2.pred<-median(statistical_summary$ked$R2,na.rm = TRUE)
  # ked.R2.val<-median(statistical_summary$ked$R2.cv,na.rm = TRUE)
  
  cubist.R2.pred<-median(statistical_summary$cubist$RsquaredApparent,na.rm = TRUE)
  cubist.R2.val<-median(statistical_summary$cubist$Rsquared,na.rm = TRUE)
  
  svm.R2.pred<-median(statistical_summary$svm$RsquaredApparent,na.rm = TRUE)
  svm.R2.val<-median(statistical_summary$svm$Rsquared,na.rm = TRUE)  
  

  
  #RMSE values for auxiliary data
  ok.RMSE.pred<-median(statistical_summary$ok$RMSE,na.rm = TRUE)
  ok.RMSE.val<-median(statistical_summary$ok$RMSE.cv,na.rm = TRUE)
  
  lm.RMSE.pred<-median(statistical_summary$lm$RMSEApparent,na.rm = TRUE)
  lm.RMSE.val<-median(statistical_summary$lm$RMSE,na.rm = TRUE)
  
  # ked.RMSE.pred<-median(statistical_summary$ked$RMSE,na.rm = TRUE)
  # ked.RMSE.val<-median(statistical_summary$ked$RMSE.cv,na.rm = TRUE)
  # 
  cubist.RMSE.pred<-median(statistical_summary$cubist$RMSEApparent,na.rm = TRUE)
  cubist.RMSE.val<-median(statistical_summary$cubist$RMSE,na.rm = TRUE)
  
  svm.RMSE.pred<-median(statistical_summary$svm$RMSEApparent,na.rm = TRUE)
  svm.RMSE.val<-median(statistical_summary$svm$RMSE,na.rm = TRUE)  
  

  #table 2
  ok<-data.frame("R2 val"=ok.R2.val,"RMSE val"=ok.RMSE.val,"R2 pred"= ok.R2.pred,"RMSE pred"=ok.RMSE.pred)
  # ked<-data.frame("R2 val"=ked.R2.val,"RMSE val"=ked.RMSE.val,"R2 pred"= ked.R2.pred,"RMSE pred"=ked.RMSE.pred)
  cubist<-data.frame("R2 val"=cubist.R2.val,"RMSE val"=cubist.RMSE.val,"R2 pred"= cubist.R2.pred,"RMSE pred"=cubist.RMSE.pred)
  svm<-data.frame("R2 val"=svm.R2.val,"RMSE val"=svm.RMSE.val,"R2 pred"= svm.R2.pred,"RMSE pred"=svm.RMSE.pred)
  lm<-data.frame("R2 val"=lm.R2.val,"RMSE val"=lm.RMSE.val,"R2 pred"= lm.R2.pred,"RMSE pred"=lm.RMSE.pred)
  
  median_aux<-rbind(ok,lm,cubist,svm)
  row.names(median_aux)<-c("ok","lm","cubist","svm")
  
  #R2 values for auxiliary data
  ok.R2.pred<-sd(statistical_summary$ok$R2,na.rm = TRUE)
  ok.R2.val<-sd(statistical_summary$ok$R2.cv,na.rm = TRUE)
  
  # ked.R2.pred<-sd(statistical_summary$ked$R2,na.rm = TRUE)
  # ked.R2.val<-sd(statistical_summary$ked$R2.cv,na.rm = TRUE)
  
  cubist.R2.pred<-sd(statistical_summary$cubist$RsquaredApparent,na.rm = TRUE)
  cubist.R2.val<-sd(statistical_summary$cubist$Rsquared,na.rm = TRUE)
  
  svm.R2.pred<-sd(statistical_summary$svm$RsquaredApparent,na.rm = TRUE)
  svm.R2.val<-sd(statistical_summary$svm$Rsquared,na.rm = TRUE)  
  
  lm.R2.pred<-sd(statistical_summary$lm$RsquaredApparent,na.rm = TRUE)
  lm.R2.val<-sd(statistical_summary$lm$Rsquared,na.rm = TRUE) 
  
  #RMSE values for auxiliary data
  ok.RMSE.pred<-sd(statistical_summary$ok$RMSE,na.rm = TRUE)
  ok.RMSE.val<-sd(statistical_summary$ok$RMSE.cv,na.rm = TRUE)
  
  # ked.RMSE.pred<-sd(statistical_summary$ked$RMSE,na.rm = TRUE)
  # ked.RMSE.val<-sd(statistical_summary$ked$RMSE.cv,na.rm = TRUE)
  # 
  cubist.RMSE.pred<-sd(statistical_summary$cubist$RMSEApparent,na.rm = TRUE)
  cubist.RMSE.val<-sd(statistical_summary$cubist$RMSE,na.rm = TRUE)
  
  svm.RMSE.pred<-sd(statistical_summary$svm$RMSEApparent,na.rm = TRUE)
  svm.RMSE.val<-sd(statistical_summary$svm$RMSE,na.rm = TRUE)  
  
  lm.RMSE.pred<-sd(statistical_summary$lm$RMSEApparent,na.rm = TRUE)
  lm.RMSE.val<-sd(statistical_summary$lm$RMSE,na.rm = TRUE)
  
  #table 2
  ok<-data.frame("R2 val"=ok.R2.val,"RMSE val"=ok.RMSE.val,"R2 pred"= ok.R2.pred,"RMSE pred"=ok.RMSE.pred)
  # ked<-data.frame("R2 val"=ked.R2.val,"RMSE val"=ked.RMSE.val,"R2 pred"= ked.R2.pred,"RMSE pred"=ked.RMSE.pred)
  cubist<-data.frame("R2 val"=cubist.R2.val,"RMSE val"=cubist.RMSE.val,"R2 pred"= cubist.R2.pred,"RMSE pred"=cubist.RMSE.pred)
  svm<-data.frame("R2 val"=svm.R2.val,"RMSE val"=svm.RMSE.val,"R2 pred"= svm.R2.pred,"RMSE pred"=svm.RMSE.pred)
  lm<-data.frame("R2 val"=lm.R2.val,"RMSE val"=lm.RMSE.val,"R2 pred"= lm.R2.pred,"RMSE pred"=lm.RMSE.pred)
  
  sd_aux<-rbind(ok,lm,cubist,svm)
  row.names(sd_aux)<-c("ok","lm","cubist","svm")
  ###########PCA median##################################
  # ked.f1.R2.pred<-median(statistical_summary$ked.f1$R2,na.rm = TRUE)
  # ked.f1.R2.val<-median(statistical_summary$ked.f1$R2.cv,na.rm = TRUE)
  # 
  # ked.f2.R2.pred<-median(statistical_summary$ked.f2$R2,na.rm = TRUE)
  # ked.f2.R2.val<-median(statistical_summary$ked.f2$R2.cv,na.rm = TRUE)
  # 
  # ked.f3.R2.pred<-median(statistical_summary$ked.f3$R2,na.rm = TRUE)
  # ked.f3.R2.val<-median(statistical_summary$ked.f3$R2.cv,na.rm = TRUE)
  # 
  # lm.f1.R2.pred<-median(statistical_summary$lm.f1$RsquaredApparent,na.rm = TRUE)
  # lm.f1.R2.val<-median(statistical_summary$lm.f1$Rsquared,na.rm = TRUE)  
  # 
  # lm.f2.R2.pred<-median(statistical_summary$lm.f2$RsquaredApparent,na.rm = TRUE)
  # lm.f2.R2.val<-median(statistical_summary$lm.f2$Rsquared,na.rm = TRUE) 
  # 
  # lm.f3.R2.pred<-median(statistical_summary$lm.f3$RsquaredApparent,na.rm = TRUE)
  # lm.f3.R2.val<-median(statistical_summary$lm.f3$Rsquared,na.rm = TRUE) 
  # 
  # #RMSE values for auxiliary data
  # ked.f1.RMSE.pred<-median(statistical_summary$ked.f1$RMSE,na.rm = TRUE)
  # ked.f1.RMSE.val<-median(statistical_summary$ked.f1$RMSE.cv,na.rm = TRUE)
  # 
  # ked.f2.RMSE.pred<-median(statistical_summary$ked.f2$RMSE,na.rm = TRUE)
  # ked.f2.RMSE.val<-median(statistical_summary$ked.f2$RMSE.cv,na.rm = TRUE)
  # 
  # ked.f3.RMSE.pred<-median(statistical_summary$ked.f3$RMSE,na.rm = TRUE)
  # ked.f3.RMSE.val<-median(statistical_summary$ked.f3$RMSE.cv,na.rm = TRUE)
  # 
  # lm.f1.RMSE.pred<-median(statistical_summary$lm.f1$RMSEApparent,na.rm = TRUE)
  # lm.f1.RMSE.val<-median(statistical_summary$lm.f1$RMSE,na.rm = TRUE)  
  # 
  # lm.f2.RMSE.pred<-median(statistical_summary$lm.f2$RMSEApparent,na.rm = TRUE)
  # lm.f2.RMSE.val<-median(statistical_summary$lm.f2$RMSE,na.rm = TRUE) 
  # 
  # lm.f3.RMSE.pred<-median(statistical_summary$lm.f3$RMSEApparent,na.rm = TRUE)
  # lm.f3.RMSE.val<-median(statistical_summary$lm.f3$RMSE,na.rm = TRUE) 
  # 
  # #table 4
  # ked.f1<-data.frame("R2 val"=ked.f1.R2.val,"RMSE val"=ked.f1.RMSE.val,"R2 pred"= ked.f1.R2.pred,"RMSE pred"=ked.f1.RMSE.pred)
  # ked.f2<-data.frame("R2 val"=ked.f2.R2.val,"RMSE val"=ked.f2.RMSE.val,"R2 pred"= ked.f2.R2.pred,"RMSE pred"=ked.f2.RMSE.pred)
  # ked.f3<-data.frame("R2 val"=ked.f3.R2.val,"RMSE val"=ked.f3.RMSE.val,"R2 pred"= ked.f3.R2.pred,"RMSE pred"=ked.f3.RMSE.pred)
  # lm.f1<-data.frame("R2 val"=lm.f1.R2.val,"RMSE val"=lm.f1.RMSE.val,"R2 pred"= lm.f1.R2.pred,"RMSE pred"=lm.f1.RMSE.pred)
  # lm.f2<-data.frame("R2 val"=lm.f2.R2.val,"RMSE val"=lm.f2.RMSE.val,"R2 pred"= lm.f2.R2.pred,"RMSE pred"=lm.f2.RMSE.pred)
  # lm.f3<-data.frame("R2 val"=lm.f3.R2.val,"RMSE val"=lm.f3.RMSE.val,"R2 pred"= lm.f3.R2.pred,"RMSE pred"=lm.f3.RMSE.pred)
  # 
  # median_pca<-rbind(ked.f1,ked.f2,ked.f3,lm.f1,lm.f2,lm.f3)
  # row.names(median_pca)<-c("ked.f1","ked.f2","ked.f3","lm.f1","lm.f2","lm.f3")
  # ###########PCA sd##################################
  # ked.f1.R2.pred<-sd(statistical_summary$ked.f1$R2,na.rm = TRUE)
  # ked.f1.R2.val<-sd(statistical_summary$ked.f1$R2.cv,na.rm = TRUE)
  # 
  # ked.f2.R2.pred<-sd(statistical_summary$ked.f2$R2,na.rm = TRUE)
  # ked.f2.R2.val<-sd(statistical_summary$ked.f2$R2.cv,na.rm = TRUE)
  # 
  # ked.f3.R2.pred<-sd(statistical_summary$ked.f3$R2,na.rm = TRUE)
  # ked.f3.R2.val<-sd(statistical_summary$ked.f3$R2.cv,na.rm = TRUE)
  # 
  # lm.f1.R2.pred<-sd(statistical_summary$lm.f1$RsquaredApparent,na.rm = TRUE)
  # lm.f1.R2.val<-sd(statistical_summary$lm.f1$Rsquared,na.rm = TRUE)  
  # 
  # lm.f2.R2.pred<-sd(statistical_summary$lm.f2$RsquaredApparent,na.rm = TRUE)
  # lm.f2.R2.val<-sd(statistical_summary$lm.f2$Rsquared,na.rm = TRUE) 
  # 
  # lm.f3.R2.pred<-sd(statistical_summary$lm.f3$RsquaredApparent,na.rm = TRUE)
  # lm.f3.R2.val<-sd(statistical_summary$lm.f3$Rsquared,na.rm = TRUE) 
  # 
  # #RMSE values for auxiliary data
  # ked.f1.RMSE.pred<-sd(statistical_summary$ked.f1$RMSE,na.rm = TRUE)
  # ked.f1.RMSE.val<-sd(statistical_summary$ked.f1$RMSE.cv,na.rm = TRUE)
  # 
  # ked.f2.RMSE.pred<-sd(statistical_summary$ked.f2$RMSE,na.rm = TRUE)
  # ked.f2.RMSE.val<-sd(statistical_summary$ked.f2$RMSE.cv,na.rm = TRUE)
  # 
  # ked.f3.RMSE.pred<-sd(statistical_summary$ked.f3$RMSE,na.rm = TRUE)
  # ked.f3.RMSE.val<-sd(statistical_summary$ked.f3$RMSE.cv,na.rm = TRUE)
  # 
  # lm.f1.RMSE.pred<-sd(statistical_summary$lm.f1$RMSEApparent,na.rm = TRUE)
  # lm.f1.RMSE.val<-sd(statistical_summary$lm.f1$RMSE,na.rm = TRUE)  
  # 
  # lm.f2.RMSE.pred<-sd(statistical_summary$lm.f2$RMSEApparent,na.rm = TRUE)
  # lm.f2.RMSE.val<-sd(statistical_summary$lm.f2$RMSE,na.rm = TRUE) 
  # 
  # lm.f3.RMSE.pred<-median(statistical_summary$lm.f3$RMSEApparent,na.rm = TRUE)
  # lm.f3.RMSE.val<-median(statistical_summary$lm.f3$RMSE,na.rm = TRUE) 
  # 
  # #table 4
  # ked.f1<-data.frame("R2 val"=ked.f1.R2.val,"RMSE val"=ked.f1.RMSE.val,"R2 pred"= ked.f1.R2.pred,"RMSE pred"=ked.f1.RMSE.pred)
  # ked.f2<-data.frame("R2 val"=ked.f2.R2.val,"RMSE val"=ked.f2.RMSE.val,"R2 pred"= ked.f2.R2.pred,"RMSE pred"=ked.f2.RMSE.pred)
  # ked.f3<-data.frame("R2 val"=ked.f3.R2.val,"RMSE val"=ked.f3.RMSE.val,"R2 pred"= ked.f3.R2.pred,"RMSE pred"=ked.f3.RMSE.pred)
  # lm.f1<-data.frame("R2 val"=lm.f1.R2.val,"RMSE val"=lm.f1.RMSE.val,"R2 pred"= lm.f1.R2.pred,"RMSE pred"=lm.f1.RMSE.pred)
  # lm.f2<-data.frame("R2 val"=lm.f2.R2.val,"RMSE val"=lm.f2.RMSE.val,"R2 pred"= lm.f2.R2.pred,"RMSE pred"=lm.f2.RMSE.pred)
  # lm.f3<-data.frame("R2 val"=lm.f3.R2.val,"RMSE val"=lm.f3.RMSE.val,"R2 pred"= lm.f3.R2.pred,"RMSE pred"=lm.f3.RMSE.pred)
  # 
  # sd_pca<-rbind(ked.f1,ked.f2,ked.f3,lm.f1,lm.f2,lm.f3)
  # row.names(sd_pca)<-c("ked.f1","ked.f2","ked.f3","lm.f1","lm.f2","lm.f3")
  
  return(list("median_aux"=median_aux,
              "sd_aux"=sd_aux
              # "median_pca"=median_pca,
              # "sd_pca"=sd_pca
              ))
}

tables_paper(stat)


############################           OLD CODE           #####################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
time.vector<-list.files(paste0(results.path,"linear_mod_PCA/Predictions/"),pattern=".grd")
time.vector<-as.Date(time.vector,format="temperature_lm_pca_harmonie%Y%m%d.grd")

heatwave2003<-which(time.vector=="2003-08-07")
cold2012<-which(time.vector=="2012-02-04")
lin.heatwave2003<-raster(lin.pred[heatwave2003])
lin.cold2012<-raster(lin.pred[cold2012])

#FROM ORDINARY KRIGING SUMMARIZING THE RESULTS
ok.pred<-list.files(paste0(results.path,"ok_model/prediction/"),pattern=".grd",full.names = TRUE)
time.vector<-list.files(paste0(results.path,"ok_model/prediction/"),pattern=".grd")
time.vector<-as.Date(time.vector,format="temperature_kriging_pca_harmonie%Y%m%d.grd")
heatwave2003<-which(time.vector=="2003-08-07")
cold2012<-which(time.vector=="2012-02-04")
ok.heatwave2003<-raster(ok.pred[heatwave2003])
ok.cold2012<-raster(ok.pred[cold2012])
# I1990<-which(time.vector>"1990-01-01" & time.vector<"2017-01-01")
# ok.pred.sub1990_2017<-stack(ok.pred[I1990])
# ok.mean1990_2017<-stackApply(ok.pred.sub1990_2017,1,fun=mean)
# writeRaster(ok.mean1990_2017,file="/nobackup/users/dirksen/data/Temperature/climatology/ok.mean1990_2017.grd")

#FROM KRIGING EXTERNAL DRIFT SUMMARIZING THE RESULTS
ked.pred<-list.files(paste0(results.path,"ked_model_PC1/prediction/"),pattern=".grd",full.names = TRUE)
time.vector<-list.files(paste0(results.path,"ked_model_PC1/prediction/"),pattern=".grd")
time.vector<-as.Date(time.vector,format="temperature_kriging_pca_harmonie%Y%m%d.grd")
heatwave2003<-which(time.vector=="2003-08-07")
cold2012<-which(time.vector=="2012-02-04")
ked.heatwave2003<-raster(ked.pred[heatwave2003])
ked.cold2012<-raster(ked.pred[cold2012])
# I1990<-which(time.vector>"1990-01-01" & time.vector<"2017-01-01")
# ked.pred.sub1990_2017<-stack(ked.pred[I1990])
# ked.mean1990_2017<-stackApply(ked.pred.sub1990_2017,1,fun=mean)
# writeRaster(ked.mean1990_2017,file="/nobackup/users/dirksen/data/Temperature/climatology/ked.mean1990_2017.grd")

# lin.mean1906_1930<-raster("/nobackup/users/dirksen/data/Temperature/climatology/lin.mean1906_1930.grd")
# lin.mean1930_1960<-raster("/nobackup/users/dirksen/data/Temperature/climatology/lin.mean1930_1960.grd")
# lin.mean1960_1990<-raster("/nobackup/users/dirksen/data/Temperature/climatology/lin.mean1960_1990.grd")



sarah<-calc_save_climatology(path.to.file = "/nobackup/users/dirksen/data/auxcillary_NED/insolation/",
                             file.to.write = "/nobackup/users/dirksen/data/auxcillary_NED/insulation_monthly_climatology/clim_sarah.grd")
#######################################################################
#######################################################################
#######################################################################
# mask_buffer<-read.asciigrid("/nobackup/users/dirksen/data/Temperature/KNMIstations/wn_distshore_001.asc")
# 
# 
# gridded(mask_buffer)<-TRUE
# proj4string(mask_buffer)<-proj4string(lin.mean1906_1930)
# mask_buffer<-raster(mask_buffer)
# extent(mask_buffer)<-extent(lin.mean1906_1930)
# mask_buffer<-resample(mask_buffer,lin.mean1906_1930)

  
# lin.mean1906_1930<-mask(lin.mean1906_1930,mask_buffer)
# lin.mean1930_1960<-mask(lin.mean1930_1960,mask_buffer)
# lin.mean1960_1990<-mask(lin.mean1960_1990,mask_buffer)
# lin.mean1990_2017<-mask(lin.mean1990_2017,mask_buffer)
# ok.mean1990_2017<-mask(ok.mean1990_2017,mask_buffer)
# ked.mean1990_2017<-mask(ked.mean1990_2017,mask_buffer)

st<-stack(ok.mean1990_2017,ked.mean1990_2017,lin.mean1990_2017)
names(st)<-c("ok","ked","lm")
spplot(st,col.regions=terrain.colors(n=200))

st.heatwave2003<-stack(ok.heatwave2003,ked.heatwave2003,lin.heatwave2003)
st.heatwave2003<-mask(st.heatwave2003,mask_buffer)
names(st.heatwave2003)<-c("ok","ked","lm")
spplot(st.heatwave2003,col.regions=terrain.colors(n=200))

st.cold2012<-stack(ok.cold2012,ked.cold2012,lin.cold2012)
st.cold2012<-mask(st.cold2012,mask_buffer)
names(st.cold2012)<-c("ok","ked","lm")
spplot(st.cold2012,col.regions=terrain.colors(n=200))

cfg <- config::get(file = "/nobackup/users/dirksen/Temperature/GeoInterpolation/config/config.yml")

pca_harm<-readRDS(cfg$pca_harmonie_mask)
proj4string(pca_harm$map)<-cfg$pro
# pca_harm$map<-mask(pca_harm$map,mask_buffer)
pca.var<-pca_variance(pca_harm$model$sdev)

# st_mask<-stack(list.files("/nobackup/users/dirksen/data/Temperature/climatology/PCAwith_mask/",pattern=".grd",full.names = TRUE))
lin.mean1990_2017<-raster("/nobackup/users/dirksen/data/Temperature/climatology/PCAwith_mask/lin.mean1990_2017.grd")
ok.mean1990_2017<-raster("/nobackup/users/dirksen/data/Temperature/climatology/PCAwith_mask/ok.mean1990_2017.grd")
ked.meanPC1_1990_2017<-raster("/nobackup/users/dirksen/data/Temperature/climatology/PCAwith_mask/ked.meanPC1_1990_2017.grd")
ked.meanPC12_1990_2017<-raster("/nobackup/users/dirksen/data/Temperature/climatology/PCAwith_mask/ked.meanPC12_1990_2017.grd")
ked.meanPC17_1990_2017<-raster("/nobackup/users/dirksen/data/Temperature/climatology/PCAwith_mask/ked.meanPC17_1990_2017.grd")
lin.mean.aux1990_2017<-raster("/nobackup/users/dirksen/data/Temperature/climatology/PCAwith_mask/lin.mean.aux1990_2017.grd")
ked.mean.aux1990_2017<-raster("/nobackup/users/dirksen/data/Temperature/climatology/PCAwith_mask/ked.mean.aux1990_2017.grd")

lin.mean.aux1990_2017<-crop(lin.mean.aux1990_2017,lin.mean1990_2017)
ked.mean.aux1990_2017<-crop(ked.mean.aux1990_2017,lin.mean1990_2017)

st_stack<-stack(ok.mean1990_2017,
                ked.meanPC1_1990_2017,
                ked.meanPC12_1990_2017,
                ked.meanPC17_1990_2017,
                lin.mean1990_2017,
                ked.mean.aux1990_2017)

names(st_stack)<-c("ok","ked PC1","ked PC12","ked PC17","lm PC17","ked aux")

st_aux<-stack("/nobackup/users/dirksen/data/auxcillary_NED/auxcillary_stacks/center_scale.grd")

sarah_aux<-stack("/nobackup/users/dirksen/data/auxcillary_NED/insulation_monthly_climatology/clim_sarah.grd")
sarah_aux<-scale(sarah_aux)

st_aux<-addLayer(st_aux,sarah_aux)

# st_raw<-stack("/nobackup/users/dirksen/data/auxcillary_NED/auxcillary_stacks/raw.grd")
roughness<-stack("/nobackup/users/dirksen/data/auxcillary_NED/roughness/roughness_summer_winter.grd")

roughness<-scale(roughness)
st_aux<-addLayer(st_aux,roughness)

precipitation<-stack(list.files("/nobackup/users/dirksen/data/auxcillary_NED/precipitation/monthly_clim/",
                                pattern=".grd",
                                full.names = TRUE))
precipitation<-scale(precipitation)
st_aux<-addLayer(st_aux,precipitation)

clim_precip<-stack("/nobackup/users/dirksen/data/auxcillary_NED/precipitation/clim_precip.grd")
st_aux<-addLayer(st_aux,clim_precip)

clim_ndvi<-stack("/nobackup/users/dirksen/data/auxcillary_NED/NDVI/modis_ndvi_clim.grd")
st_aux<-addLayer(st_aux,clim_ndvi)

st_dtr<-stack(list.files("/nobackup/users/dirksen/data/auxcillary_NED/DTR/",pattern = ".grd",full.names = TRUE))
st_dtr.mean<-stackApply(st_dtr,1,mean)
st_aux<-addLayer(st_aux,st_dtr.mean)

st_aux<-scale(st_aux)
# writeRaster(st_aux,"/nobackup/users/dirksen/data/auxcillary_NED/auxcillary_stacks/center_scale_rough.grd",
            # overwrite=TRUE)

I<-c(36,16,17,18,19,20,34,35)
library(rasterVis)

levelplot(st_aux[[I]],
          layout=c(4,2),
          scales=list(draw=FALSE ),
          names.attr=c("(a) DTR", #mean daily temperature range
                       "(b) Population Density", #year 2014
                       "(c) Height", 
                       "(d) Albedo", 
                       "(e) Insolation", #climatology SARAH
                       "(f) Roughness", # summer roughness as example
                       "(g) Precipitation",#mean precipitation
                       "(h) NDVI") 
          #col.regions=colorRampPalette(c("navy","brown4","indianred3","salmon","lightgoldenrod"))(500)
          #col.regions=colorRampPalette(c("slateblue4","blueviolet","goldenrod","yellow"))
         
)

####correlation auxiliary datasets climatologies
st_sub<-st_aux[[I]]
names(st_sub)<-c("DTR","Population","Height","Albedo","Insolation","Roughness","Precipitation","NDVI")
correlations<-layerStats(st_sub,'pearson',na.rm = TRUE)
corr_matrix=correlations$'pearson correlation coefficient'

library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

png(filename = "/usr/people/dirksen/reports/DailyTemperaturePatternsSince1951/fig/corr_aux.png",
    width = 2000,
    height = 2000,
    res=300)
corrplot(corr_matrix,
         p.mat = corr_matrix, 
         pch="p<0.05",
         pch.cex = 0.1,
         pch.col="black",
         insig = "p-value", 
         sig.level = -1,
         method="color",
         tl.col = "black",
         tl.cex = 1,
         col = col(200),
         number.cex=1)
dev.off()


##################
levelplot(st_stack,
          layout=c(3,2),
          scales=list(draw=FALSE ),
          names.attr=c("(a) ok","(b) ked PC1","(c) ked PC12","(d) ked PC17","(e) lm PC17","(f) ked aux"),
          col.regions=colorRampPalette(c("blue","cyan","green","yellow","orange","red"))
)

x11()
levelplot(pca_harm$map[[1:4]],
          layout=c(2,2),
          scales=list(draw=FALSE ),
          names.attr=c("PC1 (38.6%)","PC2 (35.5%)","PC3 (8.1%)","PC4 (6.2%)")
)

x11()
levelplot(pca_harm$map[[1:7]],
          layout=c(4,2),
          scales=list(draw=FALSE ),
          names.attr=c("PC1 (38.6%)","PC2 (35.5%)","PC3 (8.1%)","PC4 (6.2%)","PC5 (1.9%)","PC6 (1.8%)","PC7 (1.0%)")
          )

pca_old<-readRDS("/nobackup/users/dirksen/data/PCA/HARMONIE_temperature_10comp_8000nSamples_6940days_pca.rds")
pca_old$map<-mask(pca_old$map,pca_harm$map[[1]])

x11()
levelplot(pca_old$map[[1:7]],
          layout=c(4,2),
          scales=list(draw=FALSE ),
          names.attr=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7"),
          at=seq(-180,140,10)
)
# calculate_time_average_raster(raster.path="/nobackup/users/dirksen/data/Temperature/PCA_results/linear_mod_PCA/Predictions/"
#                               time.format="temperature_lm_pca_harmonie%Y-%m-%d.grd",
#                               time.period="whole period")

# Statistical summary
library(data.table)
lm_stat<-fread("/nobackup/users/dirksen/data/Temperature/PCA_results/linear_statistical_summary.txt")
lm_r2_stat<-fread("/nobackup/users/dirksen/data/Temperature/PCA_results/linear_R2traditional.txt")
lm_stat$r2_trad<-lm_r2_stat
lm_stat$datum<-as.character(lm_stat$datum)
lm_stat$datum<-as.Date(lm_stat$datum,format="%Y%m%d")
lm1990<-which(lm_stat$datum>"1990-01-01" & lm_stat$datum<"2017-01-01")
summary(lm_stat[lm1990,])

ok_stat<-fread("/nobackup/users/dirksen/data/Temperature/PCA_results/ok_statistical_summary_pred.txt")
ok_stat<-fread("/nobackup/users/dirksen/data/Temperature/PCA_results/ok_statistical_summary_cv.txt")
ok_stat$datum<-as.character(ok_stat$datum)
ok_stat$datum<-as.Date(ok_stat$datum,format="%Y%m%d")
ok1990<-which(ok_stat$datum>"1990-01-01" & ok_stat$datum<"2017-01-01")
summary(ok_stat[ok1990,])

ked_stat<-fread("/nobackup/users/dirksen/data/Temperature/PCA_results/ked_model_PC1/ked_statistical_summary_cv.txt")
ked_stat<-fread("/nobackup/users/dirksen/data/Temperature/PCA_results/ked_model_PC1/ked_statistical_summary_pred.txt")
ked_stat$datum<-as.character(ked_stat$datum)
ked_stat$datum<-as.Date(ked_stat$datum,format="%Y%m%d")
ked1990<-which(ked_stat$datum>"1990-01-01" & ked_stat$datum<"2017-01-01")
summary(ked_stat[ked1990,])

ked_stat<-fread("/nobackup/users/dirksen/data/Temperature/PCA_results/ked_model_PC12/ked_statistical_summary_cv.txt")
ked_stat<-fread("/nobackup/users/dirksen/data/Temperature/PCA_results/ked_model_PC12/ked_statistical_summary_pred.txt")
ked_stat$datum<-as.character(ked_stat$datum)
ked_stat$datum<-as.Date(ked_stat$datum,format="%Y%m%d")
ked1990<-which(ked_stat$datum>"1990-01-01" & ked_stat$datum<"2017-01-01")
summary(ked_stat[ked1990,])

ked_stat<-fread("/nobackup/users/dirksen/data/Temperature/PCA_results/ked_model_PC17/ked_statistical_summary_cv.txt")
ked_stat<-fread("/nobackup/users/dirksen/data/Temperature/PCA_results/ked_model_PC17/ked_statistical_summary_pred.txt")

#With auxiliary data
lm<-fread("/nobackup/users/dirksen/data/Temperature/Aux_results/linear_mod/linear_statistical_summary.txt")
ked_pred<-fread("/nobackup/users/dirksen/data/Temperature/Aux_results/ked_model/ked_statistical_summary_pred.txt")
ked_cv<-fread("/nobackup/users/dirksen/data/Temperature/Aux_results/ked_model/ked_statistical_summary_cv.txt")
# ked_stat<-fread("/nobackup/users/dirksen/data/Temperature/Aux_results/ked_model_distINWheight/ked_statistical_summary_cv.txt")
# ked_stat<-fread("/nobackup/users/dirksen/data/Temperature/Aux_results/ked_model_distINWheight/ked_statistical_summary_pred.txt")

##########correlation between rasters and PCs
I<-c(1,16,17,18,19,20,34,35)
st.aux<-stack("/nobackup/users/dirksen/data/auxcillary_NED/auxcillary_stacks/center_scale_rough.grd")
st.aux<-st.aux[[I]]
names(st.aux)<-c("Distance to the sea",
                  "Population Density", #year 2014
                  "Height", 
                  "Albedo", 
                  "Insulation", #climatology SARAH
                  "Roughness", # summer roughness as example
                  "Precipitation",
                  "Vegetation Index")

pca<-readRDS(cfg$pca_harmonie_mask)
st.pca<-pca$map

crs(st.pca)<-crs(st.aux)
st.aux<-crop(st.aux,st.pca)

z<-stack(st.aux,st.pca)
correlations<-layerStats(z,'pearson',na.rm = TRUE)
corr_matrix=correlations$'pearson correlation coefficient'
useful_corr<-corr_matrix[9:15,1:8]

useful_corr_abs<-abs(useful_corr)
pca_importance_aux<-rowSums(useful_corr_abs)
pca_scaled_varimp<-pca_importance_aux/sum(pca_importance_aux)*93
  
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

png(filename = "/usr/people/dirksen/reports/DailyTemperaturePatternsSince1951/fig/corr_matrix.png",
    width = 1700,
    height = 2000,
    res=300)
corrplot(useful_corr,
         p.mat = useful_corr, 
         pch="p<0.05",
         pch.cex = 0.1,
         pch.col="black",
         insig = "p-value", 
         sig.level = -1,
         method="color",
         tl.col = "black",
         tl.cex = 1,
         col = col(200),
         number.cex=1)
dev.off()


###########SELECTING PCs
# Several rules are available online on stackexchange
# https://stats.stackexchange.com/questions/33917/how-to-determine-significant-principal-components-using-bootstrapping-or-monte-c
library(config)
library(svd)
library(ggplot2)
library(psych)
library(nFactors)
library(raster)

cfg <- config::get(file = "/nobackup/users/dirksen/Temperature/GeoInterpolation/config/config.yml")
pca_variance<-function(sdev){
  eigenvalue<-sdev^2 #not scaled
  
  eb <- (1-1/length(sdev))*eigenvalue # https://stat.ethz.ch/pipermail/r-help/2005-August/076610.html
  es <- (length(sdev) - 1) * eigenvalue # https://stat.ethz.ch/pipermail/r-help/2005-August/076610.html
  proportion_variance<-sdev^2/sum(sdev^2)*100 #equal to the proportional eigenvalue
  cumulative_proportion_variance<-cumsum(proportion_variance)
  return(list("proportion_variance"=proportion_variance,
              "cumulative_proportion_variance"=cumulative_proportion_variance,
              "eigenvalue"=eigenvalue,
              "bias"=eb,
              "estimated_squares"=es))
}

pca_harm<-readRDS(cfg$pca_harmonie_mask)
proj4string(pca_harm$map)<-cfg$pro
pca.var<-pca_variance(pca_harm$model$sdev)

pc.to.keep<-nScree(eig=pca.var$proportion_variance)

pca.sub<-data.frame(pca.var$proportion_variance[1:10])
pca.sub$num<-seq(1,10,by=1)
names(pca.sub)<-c("POV","nr")
ggplot(pca.sub,aes(nr,POV))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = round(seq(min(pca.sub$nr), max(pca.sub$nr), by = 1),1))+
  geom_vline(xintercept = 2,linetype="dotted") +
  annotate("text", 3,4,label="acceleration factor") +
  geom_vline(xintercept = 7,linetype="dotted") +
  annotate("text", 8,10,label="optimal coordinate\n &\n parallel analysis")
