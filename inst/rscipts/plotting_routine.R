library(GeoInterpolation)
library(raster)
library(mapview)
library(leaflet)

data("coords_aws")
aws_in<-c(251,242,277,270,
          280,286,235,267,
          279,273,249,269,
          278,290,225,#257, #wijk aan zee started measuring in 2001, ijmuiden has a continuous measurement
          240,210,260,#265, #soesterberg only measured till 2007
          275,348,330,344,
          356,375,391,319,
          310,323,340,350,
          370,377,380)

coords_aws<-coords_aws[coords_aws$STN %in% aws_in,]
coordinates(coords_aws)<-~RDN_X+RDN_Y
proj4string(coords_aws)<-CRS("+init=epsg:28992")
coords_aws<-spTransform(coords_aws,CRS=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
labs<-paste(coords_aws@data$DS_NAME) #,coords_aws@data$STN, sep = "<br/>") #html break doesnt work...

l.osm<-leaflet(coords_aws,options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("Stamen.TerrainBackground") %>%
 addScaleBar(position = "topleft") %>% 
 addCircleMarkers(color="orange",
                   radius=2,opacity = 1,fillOpacity = 0.7,
                  options = markerOptions(riseOnHover = TRUE),
                   labelOptions = labelOptions(noHide = TRUE,textOnly = TRUE, 
                                               offset=c(0,10),
                                               direction = 'top',
                                               style=list("font-size" = "12px",
                                                          "font-family" = "serif",
                                                          "font-style" = "italic")),
                   label = labs) %>%  
  addSimpleGraticule(interval = 1) %>%
  addLabelOnlyMarkers(lng=coords_aws@coords[,1],
                      lat=coords_aws@coords[,2],
                      label=coords_aws@data$STN,
                      labelOptions = labelOptions(noHide = TRUE,textOnly = TRUE, 
                                                direction = 'right',
                                                style=list("font-size" = "12px",
                                                           "font-family" = "serif",
                                                           "font-style" = "italic"))) %>% 
  setView(lng=mean(coords_aws@coords[,1]),lat=mean(coords_aws@coords[,2]),zoom=8)
l.osm
#width=400, height=450
mapshot(l.osm,file="C:/Users/marie/OneDrive/Afbeeldingen/Temperature_paper/AWS_mapshot.png")
