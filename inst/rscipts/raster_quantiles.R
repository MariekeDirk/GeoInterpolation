library(raster)
library(stats)

lm_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/linear_mod/Predictions"
cubist_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/Cubist_mod/Predictions"
svm_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/SVM_radial_mod/Predictions"
ok_predictions<-"/nobackup/users/dirksen/data/Temperature/Aux_results/ok_model/prediction"

predictions<-c(lm_predictions,cubist_predictions,svm_predictions,ok_predictions)

model_names<-c("lm","cubist","svm","ok")

raster_quantile<-function(raster_list,prob=0.10){
st<-stack(raster_list)  
Q <- calc(st, fun = function(x) quantile(x,probs = prob,na.rm=TRUE))
}

raster_list<-lapply(predictions, list.files,pattern=".grd",full.names=TRUE)  
Q10 <- lapply(raster_list,raster_quantile)
Q90 <- lapply(raster_list,raster_quantile,prob=0.90)

stQ10<-stack(Q10)
names(stQ10)<-model_names

stQ90<-stack(Q90)
names(stQ90)<-model_names

# writeRaster(stQ10,filename = "/nobackup/users/dirksen/data/Temperature/climatology/quantiles/Q10.grd")
# writeRaster(stQ90,filename = "/nobackup/users/dirksen/data/Temperature/climatology/quantiles/Q90.grd")
library(lattice)
library(raster)
library(rasterVis)

stQ10<-subset(stQ10,order(c(2,3,4,1)))
stQ90<-subset(stQ90,order(c(2,3,4,1)))

png("/nobackup/users/dirksen/data/Temperature/fig/Q10.png",width=2500,height=2000,res=300)
levelplot(stQ10,
             scales=list(draw=FALSE),
             layout=c(2,2),
             names.attr=c("(a) ok", "(b) lm","(c) cubist", "(d) svm"),
             at=seq(1,4.9,length=30),
             col.regions=colorRampPalette(c("blue","cyan","green","yellow")))
dev.off()



png("/nobackup/users/dirksen/data/Temperature/fig/Q90.png",width=2500,height=2000,res=300)
levelplot(stQ90,
          scales=list(draw=FALSE),
          layout=c(2,2),
          names.attr=c("(a) ok", "(b) lm","(c) cubist", "(d) svm"),
          at=seq(17,20.2,length=30),
          col.regions=colorRampPalette(c("green","yellow","orange","red")))
dev.off()
