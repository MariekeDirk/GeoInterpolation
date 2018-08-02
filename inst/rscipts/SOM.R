#self organizing maps in R
library(sp)
library(raster)

data("st.test")

#Reading the data and required packages for the Self Organizing Maps (SOMs)
library(kohonen)

#post-processing function: froms weights to raster grids
reconstruct_img<-function(weights = som_model$codes,node.nr=1,train_data=st.test){
  node.weights<-data.frame(weights)[node.nr,] #reconstruction of the first node
  # node.weights.scaled<-(node.weights-min(node.weights))/(max(node.weights)-min(node.weights))
  st.N<-train_data*as.numeric(node.weights)
  r.node<-stackApply(st.N,indices = 1,fun="mean")
  return(r.node)
}

rel_freq<-function(weights = som_model$codes){
  df<-data.frame(som_model$codes)
  df.sum<-rowSums(df)
  df.freq<-df.sum/sum(df.sum)*100
  return(df.freq)
}

#basic settings
#100 img ~= 22% memory ~ 11GB
#300 img ~= 67% memory ~ 33GB
nr.img <-250 
xdim <-2
ydim <- 2



st.vect<-rasterToPoints(st.test)

train_data<-st.vect[,3:(nr.img+2)]
train_data.m<-as.matrix(train_data)
train_data.scaled<-(train_data.m-min(train_data.m))/(max(train_data.m)-min(train_data.m))

som_grid<-somgrid(xdim = xdim, ydim = ydim,topo = "rectangular") #set dimensions of the som

som_model<-som(train_data.scaled,
               grid = som_grid,
               rlen = 3000,
               alpha = 0.5) #calculate the weights for each vector

plot(som_model, type="changes") #the training process should become "flat" after several iterations, in case of a continuous decrease the rlen is too small
plot(som_model, type="counts") #look at the occurance of the different nodes (4x2)
plot(som_model, type="dist.neighbours") #determine the differences between the node neighbours, 
rel.freq<-rel_freq()

st.ls<-mapply(reconstruct_img,node.nr = seq(1,(xdim*ydim),1))
st<-stack(st.ls)

library(rworldmap)
newmap <- getMap(resolution = "low")

bg.layer <- list("sp.lines", newmap, col = "green",first=FALSE)
spplot(st,names.attr=paste0("Node ",seq(1,(xdim*ydim),1)," (",round(rel.freq),"%)"),sp.layout=bg.layer,col="transparent")
