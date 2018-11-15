#' @title Self Organizing Maps for rasters
#' @author Marieke Dirksen
#' @description Self organizing maps for raster datasets. The function uses the som function from the kohonen package to calculate the 
#' SOMs. The map nodes are typically 3x4 or 6x4. The update radius is based on the input data. The learning rate should decrease with the number of iterations.
#' Richardson(2003) used the following settings: 
#' 
#' \itemize{
#' \item 1000 iterations with learning rate of 0.2
#' \item 10.000 iterations with a learning rate of 0.1
#' \item 50.000 iterations with a learning rate of 0.002
#' }
#' @param x Raster* format
#' @param xdim x-dimension of the map nodes 
#' @param ydim y-dimension of the map nodes.
#' @param rlen number of iterations to fit the SOMs
#' @param alpha learning rate
#' @param datum optional. Used to calculate the relative frequency of each SOM for each month.
#' @examples 
#' #not run
#' #plot(som_model,type="changes")
#' @export
rasterSOM<-function(x,xdim=3,ydim=5,rlen=1000,alpha=0.2,datum=NULL){
  requireNamespace("kohonen")
  requireNamespace("raster")
#post-processing function: froms weights to raster grids
reconstruct_img<-function(weights = som_model$codes,node.nr,train_data=x){
  node.weights<-data.frame(weights)[node.nr,] #reconstruction of the first node
  # node.weights.scaled<-(node.weights-min(node.weights))/(max(node.weights)-min(node.weights))
  st.N<-(train_data*as.numeric(node.weights)) /length(node.weights)
  r.node<-stackApply(st.N,indices = 1,fun="mean")
  return(r.node)
}
# recontruct the relative frequency from the som model
rel_freq<-function(weights = som_model$codes){
  df<-abs(data.frame(som_model$codes))
  df.sum<-rowSums(df)
  df.freq<-df.sum/sum(df.sum)*100
  return(df.freq)
}

message("Creating input matrix")
train_data.m<-as.matrix(x)
# train_data.scaled<-(train_data.m-min(train_data.m))/(max(train_data.m)-min(train_data.m))

som_grid<-somgrid(xdim = xdim, ydim = ydim,topo = "rectangular") #set dimensions of the som

message(paste0("Building som model with a ",xdim," by ",ydim," grid"))
som_model<-som(train_data.m,
               grid = som_grid,
               rlen = rlen,
               alpha = alpha) #calculate the weights for each vector

rel.freq<-rel_freq()
names(rel.freq)<-paste0("SOM",seq(1,xdim*ydim,1))

message("Reconstructing images from node weights")
st.ls<-mapply(reconstruct_img,node.nr = seq(1,(xdim*ydim),1))
st<-stack(st.ls)

if(!is.null(datum)){
  requireNamespace("lubridate")
  months<-month(datum)
  df<-t(abs(data.frame(som_model$codes)))
  rel_freq_month<-aggregate(df,list(months),sum)
  rel_freq_month_prec<-100*(rel_freq_month[,-1]/rowSums(rel_freq_month[,-1]))
  names(rel_freq_month_prec)<-paste0("SOM",seq(1,xdim*ydim,1))
  output<-list("map"=st,
               "model"=som_model,
               "relative_monthly_frequency"=rel_freq_month_prec,
               "relative_frequency"=rel.freq)
  return(output)
}

output<-list("map"=st,"model"=som_model,"relative_frequency"=rel.freq)
return(output)
}
