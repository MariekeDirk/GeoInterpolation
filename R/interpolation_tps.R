#'
#'@title IDW interpolation
#'@param groundstations Data from the ground station in spatial format without na values
#'@param variable the variable of the groundstations you want to interpolate
#'@param distshore.grd masking your output for a certain region, default is the Netherlands
#'@description Function overlays the ground stations and the grid and interpolates using tps.
#'@details after running this function take a look at \code{get_statistical_summary}
#'@return returns a list with the kriging interpolation output and a dataframe with the difference between your observations and predicted grid
#'@author Marieke Dirksen
#'@export

interpolation_tps<-function(groundstations,variable,distshore.grd){
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

tps <- doTps(Tint~1, groundstations, distshore.grd)  

# TPS Cross validation

tps.cv = crossvalidate(Tint~1, groundstations, func = "doTps", debug.level = 0)



# FORCE GRID CELLS TO BE SQUARE
slot(slot(tps, "grid"), "cellsize") <- rep(mean(slot(slot(tps, "grid"), "cellsize")), 2)

# Calculate differences at observation points
predicted= sp::over (groundstations,tps)
groundstations$predicted = predicted$var1.pred
groundstations$difference = (groundstations$Tint - groundstations$predicted)
output <- data.frame(groundstations)
#colnames(output)<-c("Stn","Locatie","RDN_X","RDN_Y","observed","predicted","difference")

statistical_sum<-get_statistical_summary(prediction = tps.cv$var1.pred,observed = tps.cv$observed)
# Station output
return(list("spatial"=tps,"cv"=tps.cv,"stations"=output,"statistical_summary_cv"=statistical_sum))
}