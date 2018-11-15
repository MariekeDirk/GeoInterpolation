library(raster)
library(mapview)
library(leaflet)

datum<-"2009.05.03"
datum_string<-gsub("\\.","",datum)
datum_dt<-gsub("\\.","-",datum)

ceres<-stack("/nobackup/users/dirksen/data/CERES/CERES_SYN1deg-Day_Terra-Aqua-MODIS_Ed4A_Subset_20000301-20120504.nc")
sarah<-list.files("/nobackup/users/dirksen/data/SARAH/",full.names=TRUE)
cs_mosaic<-list.files("/nobackup/users/dirksen/data/CERES_SARAH_mosaic/",full.names = TRUE,pattern=".grd")
# CERES<-ceres[[which(names(ceres)==paste0("X",datum))]]
cs_mosaic<-stack(cs_mosaic[grep(datum_dt,cs_mosaic)])
names(cs_mosaic)<-"qq"
SARAH<-stack(sarah[grep(paste0("SISdm",datum_string),sarah)])
names(SARAH)<-"qq"

m<-mapview(SARAH,layer.name=names(SARAH),alpha.regions=0.7)
m@map %>% setView(5.4,52.4,zoom = 4)
mapshot(m,file="/usr/people/dirksen/Pictures/SARAH_full_ext.png",remove_controls = c("homeButton", "layersControl"))


SARAH<-crop(SARAH,extent(cs_mosaic))

# mapview(CERES)
m<-mapview(SARAH,layer.name=names(SARAH),alpha.regions=0.7)
m@map %>% setView(5.4,52.4,zoom = 4)
mapshot(m,file="/usr/people/dirksen/Pictures/SARAH.png",remove_controls = c("homeButton", "layersControl"))

m<-mapview(cs_mosaic,layer.name=names(cs_mosaic),alpha.regions=0.7)
m@map %>% setView(5.4,52.4,zoom = 4)
mapshot(m,file="/usr/people/dirksen/Pictures/CERES_SARAH.png",remove_controls = c("homeButton", "layersControl"))

# mapview(CERES,alpha.regions=0.7,at=seq(min(values(SARAH),na.rm=TRUE),max(values(SARAH),na.rm=TRUE),length.out = 265))+m

#sunshine duration and radiation measurements vs time
library(data.table)
library(ggplot2)

ss<-fread("/usr/people/dirksen/Documents/ss.txt")
ss<-ss[,2:3]
names(ss)<-c("year","nr.obs")
ss$year<-as.numeric(ss$year)
ss$nr.obs<-as.numeric(ss$nr.obs)

ggplot(data=ss,aes(y=nr.obs,x=year)) + 
  geom_point() + 
  theme_bw() + 
  geom_line() +
  ggtitle("Sunshine Duration Observations within Europe")
