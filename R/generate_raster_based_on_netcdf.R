#' Create raster files from NetCDF
#' 
#' \code{generate_raster_based_on_netcdf} 
#' @title Generate raster from NetCDF
#' 
#' @param ncdf_path location were the NetCDF files are stored.
#' @param save_dir path were you want to store the raster files.
#' @param start_time SARAH uses hours after 1983-01-01, this is the default. The hours are added to the start time. 
#' @param varname The NetCDF parameter of interest. 
#' @export
#'
#'
generate_raster_based_on_netcdf = function(ncdf_path,save_dir,start_time=as.POSIXct("1983-01-01 00:00:00"),varname) {
  requireNamespace("ncdf4", quietly = TRUE)
  requireNamespace("raster", quietly = TRUE)
  # First check if there is a ncdf4 file
  if (!inherits(ncdf_path, 'ncdf4')) {
    
    ncin = nc_open(ncdf_path)
    variable <- ncvar_get(ncin,varname)
  }
  
  if(inherits(ncdf_path, 'ncdf4')){
    warning(sprintf('No NetCDF files found in: %s \n', ncdf_path))
    return(FALSE)
  }
  
  # Second check: is the file empty?
  if (length(which(is.na(variable))) != length(variable)){
    
    nc.time <- ncvar_get(ncin,"time")
    time <- start_time+nc.time*60*60 #https://stackoverflow.com/questions/11922181/adding-time-to-posixct-object-in-r
    save_file <- gsub("datum",time,save_dir)
  } 
  if(length(which(is.na(variable))) == length(variable)){
    warning(sprintf('Found a file but is empty %s \n', ncdf_path))
    nc_close(ncin)
    return(FALSE)}
  
  # Finally: write a raster is everyting is OK
  st <- stack(ncdf_path,varname = varname)
  message(sprintf('Writing raster file for %s',time))
  writeRaster(st,save_file,overwrite=TRUE)
  nc_close(ncin)
  return(TRUE)
}