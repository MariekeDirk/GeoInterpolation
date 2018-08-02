#'@title Points over raster
#'@description Overlay spatial points over the auxiliary datasets for a specific day. Uses \code{\link{load_auxiliarydata}}
#'@param datum date in the format YYYY-MM-DD
#'@param spdf spatial pixel dataframe 
#'@return returns a dataframe with all auxiliary data and the datum
#'@examples
#'datum<-"1991-05-14"
#'data("coords_aws")
#'
#'coordinates(coords_aws)<-~RDN_X+RDN_Y
#'plot(coords_aws)
#'
#'aux.data<-points_over_raster(datum = datum, spdf = coords_aws)
#'@export
points_over_raster<-function(datum,spdf){
  requireNamespace("raster")
  
  r.aux<-load_auxiliarydata(datum)
  gridded(r.aux)<-TRUE
  crs(spdf)<-crs(r.aux)
  
  spdf$datum<-datum
  
  distshore.ov = extract(x=stack(r.aux),
                         y=spdf,
                         method = 'bilinear',
                         fun = mean,
                         df = TRUE) 
  # distshore.ov<-sp::over(spdf,r.aux)

  var = spdf
  
  
  var@data[names(r.aux)]=distshore.ov[names(r.aux)]
  
  #Prepare input  
  field = r.aux
  field@data = cbind(field@data, coordinates(field))
  # names(field@data) = c("s","x","y")
  #field$log = log(field$s) 
  #for(fieldlog in field$s) {try(print(paste("log of", fieldlog, "=", log(fieldlog))))}
  
  #field$log=fieldlog
  # var$x = sp::over(var,field)$x 
  # var$y = sp::over(var,field)$y
  # var$s = sp::over(var,field)$s
  
  # Remove nodata from dataframe based on missing distshore
  df = data.frame(var[complete.cases(var@data[names(r.aux)]),])
  I<-grep("optional",names(df))
  df<-df[-I]
  
  return(df)
}
