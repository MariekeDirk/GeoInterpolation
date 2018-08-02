#' Preprocess SICCS files
#'
#'@title SICCS preprocess
#'@param file_loc location of the h5 files
#'@param crs1 original crs
#'@param crs2 crs to which the file needs to be converted
#'@param new_grid grid to which the file will be cropped, resampled and masked
#'@param write_raster TRUE or FALSE, standard set to FALSE
#'@param save_dir only required if write_raster=TRUE, file name should end with ".grd"
#'@param file.format "h5" and "hdf5" both accepted
#'@description Converts the h5 files from SICCS to grid files 
#'@author Marieke Dirksen
#'@export


SICCS_preprocess<-function(file_loc,
                           crs1=NA,#cfg$WGS84
                           crs2=NA,#cfg$pro
                           new_grid,#r.distsea
                           write_raster=FALSE,
                           save_dir=NULL,
                           file.format="h5"){
  requireNamespace("sp", quietly = TRUE)
  requireNamespace("methods", quietly = TRUE)
  requireNamespace("raster", quietly = TRUE)
  requireNamespace("rhdf5", quietly = TRUE)

if(file.format=="h5"){  
lat<-h5read(file_loc,"/lat")
lon<-h5read(file_loc,"/lon")
dir<-h5read(file_loc,"direct irradiance")
dif<-h5read(file_loc,"diffuse irradiance")
tot.ir<-dir+dif
tot.ir<-raster::t(tot.ir)

r<-raster::raster(tot.ir,crs=crs1,
          xmn=range(lon)[1],
          xmx=range(lon)[2],
          ymn=range(lat)[1],
          ymx=range(lat)[2])
r<-raster::flip(r,direction="y")

} 
  
if(file.format=="hdf5"){
  dir<-rhdf5::h5read(file_loc,"direct irradiance")
  dif<-rhdf5::h5read(file_loc,"diffuse irradiance")
  tot.ir<-dir+dif
  tot.ir<-raster::t(tot.ir)
  r<-raster::raster(tot.ir, crs=crs1,
            xmn=-180,
            xmx=180,
            ymn=-90,
            ymx=90)
  r<-raster::crop(r,extent(2,8,49,55))
}  

r<-raster::projectRaster(r,crs=crs2)
r.ned<-raster::crop(r,new_grid)
r.ned<-raster::resample(r.ned,new_grid,"bilinear")
r.ned<-raster::mask(r.ned,new_grid)

if(write_raster==TRUE){
  raster::writeRaster(r.ned,save_dir,overwrite=TRUE)
  }
return(r.ned)
}
