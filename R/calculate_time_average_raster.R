#' Calculate time average raster
#' 
#' \code{calculate_time_average_raster} uses raster and rts to calculate time averages for an extent. Stacking the raster files 
#' doesn't load everything into memory. Note that for large files stackApply can consume a long time. 
#' 
#' @title Calculate time averages from raster
#' 
#' @param raster.path is the full path to the raster file with a pattern ".grd".
#' @param ext is the extent in lon lat
#' @param time.format extracts the date out of the file name
#' @param time.period averages over months, years, the whole period or yearmonths. Input includes: "month", "yearmonth","whole period".
#' @export
#'

calculate_time_average_raster<-function(raster.path,ext=extent(-11.5,15.1,45.8,61.1),time.format,time.period){
  requireNamespace("raster", quietly = TRUE)
  # if(time.period!="month"){
  #   message("sorry, no function written for this time period...\n")
  #   return(NULL)
  # }
  # 
  if  (inherits(raster.path, 'raster'))    {
    warning(sprintf("raster files not found in %s \n",raster.path))
    return(NULL)
  }
  message("going to stack raster files \n")
  #cropstack.time<-system.time(
  if  (!inherits(raster.path, 'raster')){
    
    raster.files <- list.files(raster.path,pattern=".grd",full.names = TRUE)
    raster.times <- list.files(raster.path,pattern=".grd",full.names = FALSE)
    
    raster.times <- as.POSIXct(raster.times,format=time.format)
    
    st<-stack(raster.files)
    message("raster files stacked \n")
    st<-crop(st,ext)
    message("raster files cropped to new extent \n")
  }
  #)
  #message(paste0("It took ", cropstack.time, "to stack and crop all the rasters"))
  
  if(time.period=="whole period"){
    message("calculating an average for the whole period \n")
    mean.wholeperiod<-stackApply(st,1,fun=mean)
    
    message("done")
    return(mean.wholeperiod)  
  }  
  #st.ts<-rts(st,raster.times)
  if(time.period=="month"){
    message("calculating montly average \n")
    indices <- format(raster.times,format="%m")
    indices <- as.numeric(indices)
    
    #NOTE: calc could be faster for large datasets with complex formula's
    mean.month <- stackApply(st,indices,fun=mean) # Calculating the mean for each month
    names(mean.month) <- c("Jan","Feb","March","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")
    
    message("done")
    return(mean.month)
  }
  
  if(time.period=="yearmonth"){
    message("calculating monthly averages for every year \n")
    indices <- format(raster.times,format="%Y%m")
    indices <- as.numeric(indices)
    
    #NOTE: calc could be faster for large datasets with complex formula's
    mean.yearmonth <- stackApply(st,indices,fun=mean) # Calculating the mean for each month
    names(mean.yearmonth) <- unique(format(raster.times,format="%Y%m"))
   
    message("done")
    return(mean.month)
  }
  
  
  
}
