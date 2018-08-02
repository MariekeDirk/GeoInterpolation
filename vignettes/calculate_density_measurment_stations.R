#calculate distances between measurement points

setwd('E:\\Temperature_interpolation')
library(raster)
#DATA IMPORT
inputdata<-read.table("./input/Temperatuur20Jr.txt",header=T, sep=",",dec=".",na.strings="/////",colClasses =c(rep("character",2),rep("numeric",5)))
#some files have difficulties because the station at Vlieland has no data, 
#which is displayed in the text file as /////
#so in read.table na.strings="/////" is added
#Now still the data will be read as factor, to overwrite this comand colClasses is used, here the class
#of each column is specified
#inputdata = inputdata[inputdata$Jaar==RACMO_datum,] #The datum of the inputdata equals the RACMO_datum
inputdata[] <- lapply(inputdata, function(x) type.convert(as.character(x)))
#This is to avoid "Factor" problems with the .txt file

coordinates(inputdata) = ~RDN_X+RDN_Y #the coordinates of inputdata, compare to headers

#################
######special case: trainingset
# inputdata = inputdata[!(inputdata$Stn==235) & !(inputdata$Stn==240)
#                           & !(inputdata$Stn==260)& !(inputdata$Stn==267)
#                           & !(inputdata$Stn==269)& !(inputdata$Stn==277)
#                           & !(inputdata$Stn==278)& !(inputdata$Stn==280)
#                           & !(inputdata$Stn==283)& !(inputdata$Stn==310)
#                           & !(inputdata$Stn==330)& !(inputdata$Stn==340)
#                           & !(inputdata$Stn==344)& !(inputdata$Stn==356)
#                           & !(inputdata$Stn==370)& !(inputdata$Stn==391)
#                           & !(inputdata$Stn==377)& !(inputdata$Stn==257),]
# 
# inputdata = inputdata[!(inputdata$Stn==380) & !(inputdata$Stn==375)
#                           & !(inputdata$Stn==350)& !(inputdata$Stn==319)
#                           & !(inputdata$Stn==275)& !(inputdata$Stn==348)
#                           & !(inputdata$Stn==290)& !(inputdata$Stn==210)
#                           & !(inputdata$Stn==279)& !(inputdata$Stn==249)
#                           & !(inputdata$Stn==286)& !(inputdata$Stn==270)
#                           & !(inputdata$Stn==251)& !(inputdata$Stn==242)
#                           & !(inputdata$Stn==273),]
##########################
#distance between all coordinates
#mean=132km
distance_all=pointDistance(coordinates(inputdata), coordinates(inputdata), lonlat=FALSE,allpairs=TRUE)
D_all=distance[distance!=0]

Dist=matrix(NA,length(inputdata$Stn),1)
#calculate the minimum distance to the next station
for (i in 1:33){
distance=pointDistance(coordinates(inputdata[i,]), coordinates(inputdata), lonlat=FALSE,allpairs=TRUE)
D=distance[distance!=0]
Dist[i]=min(D)

}

#plotting the minimum distance
inputdata$mindist=Dist #add minimum distance to inputdata (now it has coordinates)

#######maps
pro=CRS("+init=epsg:28992") # The projection to use

mymap.unpro=readOGR(dsn="E:\\Final_R_scripts\\NaturalEarthData\\ne_10m_admin_0_countries",layer="ne_10m_admin_0_countries") # Read in (unprojected) map data
mymap.pro=spTransform(mymap.unpro, pro) # Reproject the map

mymap.unpro_lakes=readOGR(dsn="E:\\Final_R_scripts\\NaturalEarthData\\ne_10m_lakes",layer="ne_10m_lakes") # Read in (unprojected) map data
mymap.pro_lakes=spTransform(mymap.unpro_lakes, pro) # Reproject the map

mymap.unpro_lakes_eu=readOGR(dsn="E:\\Final_R_scripts\\NaturalEarthData\\ne_10m_lakes_europe",layer="ne_10m_lakes_europe") # Read in (unprojected) map data
mymap.pro_lakes_eu=spTransform(mymap.unpro_lakes_eu, pro) # Reproject the map

setwd("E:\\Final_Figures\\Png\\Discussion")
# testpts=data.frame(inputdata$RDN_X,inputdata$RDN_Y)
# colnames(testpts)<-c("x","y")

chuld <-structure(list(x = c(30475,68209,88750,101750,114480,125250,152200,208750,272775,
                               235125,257978,241500,211100,181750,181300,156800,123495,82820,48425), 
                         y = c(385125,445550,464425,502250,548610,585325,600600,602725,580150,
                               529750,477076,454500,390150,324500,356375,383950,397640,384700,360600)),
                        .Names = c("x", "y"))

# chuld <- lapply(testpts,"[",chull(testpts))

# png("Distance_stations.png",width=10,height=10,units="in",res=300)
# png("Distance_stations_convexhull.png",width=10,height=10,units="in",res=300)
plot(inputdata$RDN_X,inputdata$RDN_Y,cex=abs(inputdata$mindist/10000),pch=19,
     col = "cadetblue4",axes=FALSE,xlab="",ylab="",
     xlim=c(12621.630033977,278621.630033977),ylim=c(305583.0457758,620583.0457758),asp=1)
#img_1<-mymap.pro[mymap.pro$NAME_SORT  %in% c('Belgium','Germany','France'),]
#country_colors<-list(Belgium='gray',Germany='gray',France='gray')
#plot(img_1,col=unlist(country_colors[as.character(img_1$NAME_SORT)]),xlim=c(12621.630033977,278621.630033977),ylim=c(305583.0457758,620583.0457758),add=TRUE)
plot.xy(xy.coords(inputdata$RDN_X,inputdata$RDN_Y),type="p",pch=".",col="black",cex=3)
img_2<-mymap.pro[mymap.pro$NAME_SORT  %in% c('Netherlands'),]
plot(img_2,xlim=c(12621.630033977,278621.630033977),ylim=c(305583.0457758,620583.0457758),add=TRUE)
plot(mymap.pro_lakes, xlim=c(12621.630033977,278621.630033977),ylim=c(305583.0457758,620583.0457758),add=TRUE)
#plot(mymap.pro_lakes2, col='darkgray',xlim=c(12621.630033977,278621.630033977),ylim=c(305583.0457758,620583.0457758),add=TRUE)
#plot(mymap.pro_rivers, col='darkgray',xlim=c(12621.630033977,278621.630033977),ylim=c(305583.0457758,620583.0457758),add=TRUE)
plot(mymap.pro_lakes_eu, xlim=c(12621.630033977,278621.630033977),ylim=c(305583.0457758,620583.0457758),add=TRUE)
text(inputdata$RDN_X,inputdata$RDN_Y,round(inputdata$mindist/1000),cex=0.7,pos=3)
text(inputdata$RDN_X,inputdata$RDN_Y,inputdata$Locatie,cex=0.5,pos=1)  
polygon(chuld,lty=2,border="blue",lwd=2)

dev.off()

###########convex hull#########################

# Splining a polygon.
#
#   The rows of 'xy' give coordinates of the boundary vertices, in order.
#   'vertices' is the number of spline vertices to create.
#              (Not all are used: some are clipped from the ends.)
#   'k' is the number of points to wrap around the ends to obtain
#       a smooth periodic spline.
#
#   Returns an array of points. 
# 





# polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border="red",lwd=2)
# 
# spline.poly <- function(xy, vertices, k=3, ...) {
#   # Assert: xy is an n by 2 matrix with n >= k.
#   
#   # Wrap k vertices around each end.
#   n <- dim(xy)[1]
#   if (k >= 1) {
#     data <- rbind(xy[(n-k+1):n,], xy, xy[1:k, ])
#   } else {
#     data <- xy
#   }
#   
#   # Spline the x and y coordinates.
#   data.spline <- spline(1:(n+2*k), data[,1], n=vertices, ...)
#   x <- data.spline$x
#   x1 <- data.spline$y
#   x2 <- spline(1:(n+2*k), data[,2], n=vertices, ...)$y
#   
#   # Retain only the middle part.
#   cbind(x1, x2)[k < x & x <= n+k, ]
# }