#runApp("/nobackup/users/dirksen/Temperature/GeoInterpolation/inst/shiny_interpolation")
#LOAD PACKAGES
library(methods)
library(sp)
#library(gstat)
library(automap)
library(grid)
library(maps)
library(spam)
library(shiny)
#library(fields)
library(rgdal)

pro=CRS("+init=epsg:28992")
# mymap.unpro=readOGR(dsn='/nobackup/users/dirksen/data/NaturalEarthData/ne_10m_admin_0_countries/',layer="ne_10m_admin_0_countries") # Read in (unprojected) map data
# mymap.pro=spTransform(mymap.unpro, pro) # Reproject the map
# 
# mymap.unpro_lakes=readOGR(dsn='/nobackup/users/dirksen/data/NaturalEarthData/ne_10m_lakes/',layer="ne_10m_lakes") # Read in (unprojected) map data
# 
# mymap.pro_lakes=spTransform(mymap.unpro_lakes, pro) # Reproject the map
# 
# fun <- function() {
#   plot(mymap.pro,add=TRUE)
#   plot(mymap.pro_lakes,add=TRUE)
# }

#####initialization#######################
#tempset="t2m" #set for outputdir!

# startdate=as.Date("1995/01/02") #start day of the interpolation
# stopdate=as.Date("2014/06/29") #stop day of the interpolation
# time=seq(startdate,stopdate,by="days") 
##########################################
#locations
HARMONIE_loc_new<-'/nobackup/users/dirksen/data/Temperature/HARMONIE/'
KNMItemperature<-'/nobackup/users/dirksen/Temperature/GeoInterpolation/R/' 
source (paste0(KNMItemperature,"crossvalidate.r"))

shinyServer(function(input,output) {
# Dynamic variables
# blocksize = input$blocksize
# blocksize=20000
# output$statistics<-renderText({

output$plotKrige<-renderPlot({   
mxdkrige=Inf # maxdist Krige

time<-input$t
Y=as.numeric(format(time,"%Y"))
t=as.numeric(format(time, "%Y%m%d"))

#inputdata all 
inputdata_all<-read.table("/nobackup/users/dirksen/data/Temperature/KNMIstations/TemperatuurDag.txt",
                      header=T, sep=",",dec=".",na.strings="/////",
                      colClasses =c(rep("character",2),rep("numeric",6)))

  #Loading the HARMONIE data
  HARMONIE_datum<-t
  HARMONIE_grid<-"fileyyyymmddT123000Z.grd"
  HARMONIE_grid<-gsub("yyyymmdd",t,HARMONIE_grid)
  distshore.grd<-read.asciigrid(paste0(HARMONIE_loc_new,HARMONIE_grid),colname="distshore")
  distshore.grd$distshore=distshore.grd$distshore-273.15

  #DATA IMPORT
  inputdata = inputdata_all[inputdata_all$Datum==HARMONIE_datum,] #The datum of the inputdata equals the RACMO_datum
  inputdata[] <- lapply(inputdata, function(x) type.convert(as.character(x)))
  coordinates(inputdata) = ~RDN_X+RDN_Y #the coordinates of inputdata, compare to headers
  inputdata$Tg_new=inputdata$Tg #mean temperature observation at 1.5m [Celcius]
  inputdata <- inputdata[!is.na(inputdata$Tg),] #removing nan data from the temperature
  #kriging and other interpolations wont work with NA values


  # maps
  # nl.grd <- read.asciigrid("./wn_maskbuffer_001.asc")
  # #Cut NA values from this grid, and make sure the cellsize stays square.
  # gridded(nl.grd)=FALSE;gridded(nl.grd)=TRUE;fullgrid(nl.grd) = TRUE
  # slot(slot(nl.grd, "grid"), "cellsize") <- rep(mean(slot(slot(nl.grd,"grid"), "cellsize")), 2)
  #Cut NA values from this grid, and make sure the cellsize stays square.
  gridded(distshore.grd)=FALSE;gridded(distshore.grd)=TRUE;fullgrid(distshore.grd) = TRUE
  slot(slot(distshore.grd, "grid"), "cellsize") <-rep(mean(slot(slot(distshore.grd, "grid"), "cellsize")), 2)
  
  # over functions
  
  # over Distshore on Var
  distshore.ov=over(inputdata,distshore.grd)
  # Copy the values to Var )
  
  var = inputdata
  
  
  var$distshore=distshore.ov$distshore
  
  #Prepare input  
  field = distshore.grd
  field@data = cbind(field@data, coordinates(field))
  names(field@data) = c("s","x","y")
  #field$log = log(field$s) 
  #for(fieldlog in field$s) {try(print(paste("log of", fieldlog, "=", log(fieldlog))))}
  
  #field$log=fieldlog
  var$x = over(var,field)$x 
  var$y = over(var,field)$y
  var$s = over(var,field)$s
  
  # Remove nodata from dataframe based on missing distshore
  var = var[!is.na(var$distshore),]

  ###############################################################################################
  ############HERE THE INTERPOLATION STARTS######################################################
  ###############################################################################################
  ### Universal kriging (KED) - variogrammodel GAU ###
  
  # Kriging
  
  
  
  
    ked_gau <- autoKrige(Tg_new~distshore, var, distshore.grd, 
                         maxdist=mxdkrige, block=c(input$blocksize,input$blocksize), 
                         model = c(input$variogram), na.action=na.pass, fix.values=c(NA,NA,NA), 
                         miscFitOptions = list(merge.small.bins = TRUE))  #log(distshore)

 


   
    plot(ked_gau)
    
    # plot(mymap.pro,add=TRUE)
    # plot(mymap.pro_lakes,add=TRUE)
   
  
# summary(ked_gau$krige_output$var1.pred)
  })
})
# })
  # # Krige Cross validation
  # ked_gau.cv <- autoKrige.cv(Tg_new~distshore, var, model = c("Exp"),maxdist=mxdkrige,fix.values=c(NA,NA,NA), miscFitOptions = list(merge.small.bins = TRUE))
  # teller <- sum(ked_gau.cv$krige.cv_output$residual^2)
  # noemer <- sum((var$Tg_new-mean(var$Tg_new))^2)
  # ked_gau.r2 <- 1 - teller/noemer
  # ked.zscoremean <- mean(ked_gau.cv$krige.cv_output$zscore)
  # ked.zscore.var <- var(ked_gau.cv$krige.cv_output$zscore)
  # 
  # # FORCE GRID CELLS TO BE SQUARE
  # ked_gau=ked_gau$krige_output
  # slot(slot(ked_gau, "grid"), "cellsize") <- rep(mean(slot(slot(ked_gau, "grid"), "cellsize")), 2)
  # 
  # # Calculate differences at observation points
  # predicted= over (var,ked_gau)
  # var$predicted = predicted$var1.pred
  # var$difference = (var$Tg_new - var$predicted)
  # difmin = min (var$difference,na.rm=TRUE)
  # difmax = max (var$difference,na.rm=TRUE)
  # difmean = mean (var$difference,na.rm=TRUE)
  # difsd = sd (var$difference,na.rm=TRUE)
  # output <- data.frame(var$Stn,var$Locatie,var$RDN_X,var$RDN_Y,var$predicted,var$difference)
  # colnames(output)<-c("Stn","Locatie","RDN_X","RDN_Y","predicted","difference")

  

 