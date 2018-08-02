#'
#'@title IDW interpolation
#'@param groundstations Data from the ground station in spatial format without na values
#'@param distshore.grd masking your output for a certain region, default is the Netherlands
#'@param blocksize idw interpolation variable, with a standard value of 20000
#'@param mxdidw idw interpolation variable, with a standard value of 120000
#'@param variable the variable of the groundstations you want to interpolate
#'@description Function overlays the ground stations and the grid and interpolates using idw.
#'@details after running this function take a look
#'@return returns a list with the kriging interpolation output and a dataframe with the difference between your observations and predicted grid
#'@author Marieke Dirksen
#'@export
interpolation_idw<-function(groundstations,variable,distshore.grd,mxdidw=120000,blocksize=20000){
  ##########################################################################
  #Catching possible problems with the code before continue
  ##########################################################################
  if (!inherits(variable,"character")){
    message("variable name is not a character, returning FALSE")
    return(FALSE)
  }
  
  if (!inherits(groundstations,"SpatialPointsDataFrame")){
    message("groundstations is not in the format of a SpatialPointsDataFrame, returning FALSE")
    return(FALSE)
  }
  
  if(!inherits(distshore.grd,"SpatialGridDataFrame")){
    message("distshore.grd is not in the format of a SpatialGridDataFrame, returning FALSE")
    return(FALSE)
  }
  ##########################################################################
names(groundstations)[names(groundstations)==variable] <- "Tint"

requireNamespace("fields", quietly = TRUE)
requireNamespace("gstat", quietly = TRUE)
requireNamespace("sp", quietly = TRUE)
#Cut NA values from this grid, and make sure the cellsize stays square.
#sp::gridded(mask)=FALSE;sp::gridded(mask)=TRUE;sp::fullgrid(mask) = TRUE
#slot(slot(mask, "grid"), "cellsize") <- rep(mean(slot(slot(mask,"grid"), "cellsize")), 2)

#Cut NA values from this grid, and make sure the cellsize stays square.
#sp::gridded(distshore.grd)=FALSE;sp::gridded(distshore.grd)=TRUE;sp::fullgrid(distshore.grd) = TRUE
#slot(slot(distshore.grd, "grid"), "cellsize") <-rep(mean(slot(slot(distshore.grd, "grid"), "cellsize")), 2)

idw <- gstat::idw(Tint~1, 
                  groundstations, 
                  distshore.grd, 
                  maxdist=mxdidw, 
                  block=c(blocksize,blocksize), 
                  na.action=na.pass
                  )

# FORCE GRID CELLS TO BE SQUARE
slot(slot(idw, "grid"), "cellsize") <- rep(mean(slot(slot(idw, "grid"), "cellsize")), 2)

# IDW Cross Validation

# function following ASDAR page 222 (Bivand, Pebesma, etc)
asdar.idw.cv <- function(input, index) {
  model <- input[-index,]                                    # stations used for interpolation (all but one)
  valid <- input[index,]                                     # station location which will be interpolated
  valid.pr <- gstat::idw(Tint~1, model, valid, maxdist=mxdidw,block=c(blocksize,blocksize))                  # actual idw interpolation
  return(valid.pr$var1.pred)
  
}

# loocv
# for-loop runs through the observation stations
for(stationIndex in 1:dim(groundstations)[1]) {
  print (paste("Station: ", stationIndex))                   # show which obs. station is interpolated right now
  var1.pred <- asdar.idw.cv(input=groundstations, index=stationIndex)   # call function, use Temp and stations
  observed <- groundstations$Tint[stationIndex]               # observation value of station which was interpolated for
  #  append prediction and observation to Temp
  groundstations$var1.pred[stationIndex] <- var1.pred
  groundstations$observed[stationIndex] <- observed
}

groundstations$residue <- groundstations$observed - groundstations$var1.pred

# remove NA's
naIndexes <- !is.na(groundstations$residue)                          # rows NOT containing NA's
residues <- groundstations$residue[naIndexes]
var1.pred <- groundstations$var1.pred[naIndexes]
observed <- groundstations$observed[naIndexes]

# Calculate differences at observation points
predicted= sp::over (groundstations,idw)
groundstations$predicted = predicted$var1.pred
groundstations$difference = (groundstations$observed - groundstations$predicted)
output <- data.frame(groundstations)
#colnames(output)<-c("Stn","Locatie","RDN_X","RDN_Y","observed","predicted","difference")

#idw.cv<-subset(groundstations,select=c("Stn","Locatie","Datum","residue","var1.pred","observed"))

statistical_sum<-get_statistical_summary(prediction = groundstations$var1.pred,observed = groundstations$observed)
# Station output
return(list("spatial"=idw,"cv"= data.frame(groundstations$residue,groundstations$var1.pred,groundstations$observed),"stations"=output,"statistical_summary_cv"=statistical_sum))
}
