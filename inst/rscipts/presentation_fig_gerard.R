library(data.table)
library(mapview)
library(raster)

groundstations<-"/nobackup/users/dirksen/data/radiation_europe/WRDC/meta/"
wrdc_files<-list.files(groundstations,full.names = TRUE)
wrdc_df<-lapply(wrdc_files,fread)
wrdc_df<-do.call("rbind",wrdc_df)
wrdc_df$lat<-wrdc_df$lat/3600
wrdc_df$lon<-wrdc_df$lon/3600

coordinates(wrdc_df)<-~lon+lat
proj4string(wrdc_df)<-CRS('+init=epsg:4226')

# wrdc_df_84<-spTransform(wrdc_df, CRS("+proj=longlat"))
library(RColorBrewer)
n <- length(unique(wrdc_df$coun_id))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
clrs = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

mapview(wrdc_df,zcol="coun_id",col.regions = clrs)
# proj4string <- "+proj=utm +zone=19 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "

height<-stack("/nobackup/users/dirksen/data/radiation_europe/DEM/gtopo30_gis_1km.grd")
r.h<-height$alt
mapview(r.h,maxpixels =  2E5)

sat_files<-list.files("/nobackup/users/dirksen/data/CERES_SARAH_mosaic",full.names = TRUE,pattern=".grd")
sarah_cmsaf<-raster(sat_files[150])
mapview(sarah_cmsaf,alpha=0.7)

df<-fread("/usr/people/dirksen/Documents/coords_ine.csv")
df$lat<-as.numeric(gsub(",","",df$lat))
df$lon<-as.numeric(gsub(",","",df$lon))



coordinates(df)<-~lon+lat
crs(df)<-CRS("+init=epsg:25831")
df<-spTransform(df,CRS('+init=epsg:4226'))


# lat_dms<-dd2dms(df$lat)
# lon_dms<-dd2dms(df$lon)
df<-data.frame(df)



df$lat_dms<-measurements::conv_unit(df$lat,from = "dec_deg",to="deg_min_sec")
df$lon_dms<-measurements::conv_unit(df$lon,from = "dec_deg",to="deg_min_sec")

write.table(df,"/usr/people/dirksen/Documents/coords_ine_dms.txt",sep=",",row.names=FALSE,col.names = TRUE)
mapview(df)

library(data.table)
library(ggplot2)
df<-fread("/usr/people/dirksen/Documents/ss.txt",colClasses = rep("numeric",3))
df<-df[1:(length(df$Year)-1),]

df<-df %>% gather(df,key=measurement,qq,ss)

ggplot(data=df,aes(x=Year,y=df,colour=measurement)) + 
  geom_point() + geom_line() + theme_bw() +
  theme(legend.position=c(0.2,0.8)) + ylab("count")

