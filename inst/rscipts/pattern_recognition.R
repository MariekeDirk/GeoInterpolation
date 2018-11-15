##########################################################################
#PCA ceres
#https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}
#Q: how do the PCs change after masking the sea?
library(raster)
library(RStoolbox)
library(lubridate)
library(kohonen)
ceres1<-stack("/nobackup/users/dirksen/data/CERES/CERES_SYN1deg-Day_Terra-Aqua-MODIS_Ed4A_Subset_20000301-20120504.nc",
              varname="ini_sfc_sw-down_all_daily")
ceres2<-stack("/nobackup/users/dirksen/data/CERES/CERES_SYN1deg-Day_Terra-Aqua-MODIS_Ed4A_Subset_20000301-20120504.nc",
              varname="ini_sfc_sw-down_all_daily")
ceres<-stack(ceres1,ceres2)
ceres.dates<-as.Date(names(ceres),format = "X%Y.%m.%d")

ceres.month<-month(ceres.dates)
ceres.week<-week(ceres.dates)

# eu.mask<-stack("/nobackup/users/dirksen/data/radiation_europe/DEM/gtopo30_gis_1km.grd")
# eu.mask<-eu.mask[[1]]
# values(eu.mask)<-!is.na(getValues(eu.mask))
# values(eu.mask)[values(eu.mask)==0]=NA
# 
# p<-gdal_polygonizeR(eu.mask)
# saveRDS(p,"/nobackup/users/dirksen/R_packages/Temperature/GeoInterpolation/inst/Rdata/mask_gtopo30_gis_1km.rds")
# p<-readRDS("/nobackup/users/dirksen/R_packages/Temperature/GeoInterpolation/inst/Rdata/mask_gtopo30_gis_1km.rds")

#######CREATE A BUFFER AROUND THE MASK WITH SF PACKAGE
# library(sf)
# p.sf<-as(p,"sf")
# p.sf<-st_transform(p.sf,28992)
# p.sf<-st_combine(p.sf) #combine into one polygon to overcome overlapping features with st_simplify
# p.sf.simple<-st_simplify(p.sf,dTolerance = 900) #simplify the geometry so the buffer function becomes much faster
# p.sf.bf<-st_buffer(p.sf.simple,dist=40000) #buffer of 30km
# p.sf.bf<-st_transform(p.sf.bf,4326)
# p.bf<-as(p.sf.bf,"Spatial")
# p.bf<-crop(p.bf,extent(p))
# saveRDS(p.bf,"/nobackup/users/dirksen/R_packages/Temperature/GeoInterpolation/inst/Rdata/mask_40km_buffer_gtopo30_gis_1km.rds")
p.bf<-readRDS("/nobackup/users/dirksen/R_packages/Temperature/GeoInterpolation/inst/Rdata/mask_40km_buffer_gtopo30_gis_1km.rds")


ceres.mask<-mask(ceres,p.bf)
#OPTIONAL CODE
##########################check if there are any na values in the raster before running the PCA
get_na_raster<-function(X,i){
  length_na<-length(which(is.na(getValues(X[[i]]))))
  
  if(length_na==0){
    print(length_na)
    return(FALSE)
  } else{
    print(length_na)
    return(TRUE)
  }
  
}
raster_na<-mapply(get_na_raster,MoreArgs=list(X=ceres),i=seq(1:length(names(ceres))))
##########################
#subset a time period (note: Ridchardson(2003) removed weekly climatology from his sea surface temperature dataset)
month.nr<-8
I<-which(ceres.month==month.nr)

#Normalize CERES data with 18 year weekly mean irradiance
normalize_weekly<-function(week.nr){
I<-which(ceres.week==week.nr)
ceres_week<-ceres.mask

ceres_period<-ceres_week[[I]]
ceres_mean<-stackApply(ceres_period,indices = 1,fun="mean")

ceres_normal<-ceres_period-ceres_mean #way of looking at the anamolies
names(ceres_normal)<-names(ceres_period)
return(ceres_normal)
}

ceres_normal<-lapply(seq(1,53,1),normalize_weekly)
ceres_normal<-stack(ceres_normal)
#SOM analysis
ceres_som<-rasterSOM(ceres_normal,datum=ceres.dates)

#monthly relative frequency
library(ggplot2)
corr<-ceres_som$relative_monthly_frequency
corr$month<-seq(1,12)
df.heatmap<-melt(corr,id.vars="month")
ggplot(data=df.heatmap,aes(x=month,y=variable))+
  geom_tile(aes(fill=value)) +
  geom_text(aes(label=round(value,1)))+
  theme_light() +
  scale_fill_gradientn(colours = brewer.pal(9,"Blues")) +
  labs(fill = "value") +
  xlab("Month of the year") +
  ylab("")

names(ceres_som$map)<-paste0(names(ceres_som$relative_frequency),"_",round(ceres_som$relative_frequency,1))
spplot(ceres_som$map)

#PCA Analysis
ceres_pca<-rasterPCA(ceres_normal,
                     maskCheck=FALSE,
                     nComp=12,
                     spca = TRUE)
mapview(ceres_pca$map[[1]]) #plot the first PC
summary(ceres_pca$model)    #look at the proportion of variance (fraction 0-1)
