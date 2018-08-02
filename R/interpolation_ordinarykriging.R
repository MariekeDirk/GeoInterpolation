#'
#'@title Ordinary Kriging interpolation
#'@param groundstations Data from the ground station in spatial format without na values
#'@param variable the variable of the groundstations you want to interpolate
#'@param grid_drift trend grid for the interpolation with same spatial extent as groundstations
#'@param blocksize kriging interpolation variable, with a standard value of 20000
#'@param mxdkrige kriging interpolation variable, with a standard value of Inf
#'@param mod kriging interpolation variable, standard set to "Exp"
#'@description Function overlays the ground stations and the grid and interpolates using autokrige from the \code{\link{autoKrige}}.
#'@details after running this function take a look at \code{get_statistical_summary}
#'@return returns a list with the kriging interpolation output and a dataframe with the difference between your observations and predicted grid
#'@author Marieke Dirksen
#'@export
interpolation_ordinarykriging<-function(groundstations,variable,grid_drift,blocksize=20000,mxdkrige=Inf,mod="Exp"){
  
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
  
  if(!inherits(grid_drift,"SpatialGridDataFrame")){
    message("grid_drift is not in the format of a SpatialGridDataFrame, returning FALSE")
    return(FALSE)
  }
  ##########################################################################
  names(groundstations)[names(groundstations)==variable] <- "Tint"
  
  requireNamespace("fields", quietly = TRUE)
  requireNamespace("gstat", quietly = TRUE)
  requireNamespace("sp", quietly = TRUE)
##############################################################################################
############HERE THE INTERPOLATION STARTS######################################################
###############################################################################################
### Universal kriging (KED) - variogrammodel GAU ###

  ok_sph <- automap::autoKrige(Tint~1, 
                               groundstations, 
                               grid_drift, 
                               maxdist=mxdkrige, 
                               block=c(blocksize,blocksize), 
                               model = c(mod), 
                               na.action=na.pass, 
                               fix.values=c(NA,NA,NA), 
                               miscFitOptions = list(merge.small.bins = TRUE),
                               verbose = FALSE) 
  
  # Krige Cross validation
  ok_sph.cv <- automap::autoKrige.cv(Tint~1, 
                                     groundstations, 
                                     model = c(mod),
                                     maxdist=mxdkrige,
                                     fix.values=c(NA,NA,NA), 
                                     miscFitOptions = list(merge.small.bins = TRUE),
                                     verbose = c(FALSE,FALSE))
  ok.zscoremean <- mean(ok_sph.cv$krige.cv_output$zscore)
  ok.zscore.var <- var(ok_sph.cv$krige.cv_output$zscore)
  
  # FORCE GRID CELLS TO BE SQUARE
  ok_sph=ok_sph$krige_output
  slot(slot(ok_sph, "grid"), "cellsize") <- rep(mean(slot(slot(ok_sph, "grid"), "cellsize")), 2)
  
  # Calculate differences at observation points
  predicted= sp::over (groundstations,ok_sph)
  groundstations$predicted = predicted$var1.pred
  groundstations$difference = (groundstations$Tint - groundstations$predicted)
  output <- data.frame(groundstations)
  #colnames(output)<-c("Stn","Locatie","RDN_X","RDN_Y","predicted","difference")
  
  #statistical_sum<-list("R2"=ok_sph.r2,"diffmin"=ok_sph.difmin)
  statistical_sum.cv<-get_statistical_summary(prediction = ok_sph.cv$krige.cv_output$var1.pred,
                                           observed = ok_sph.cv$krige.cv_output$observed)
  statistical_sum.pred<-get_statistical_summary(prediction = groundstations$predicted,
                                           observed = ok_sph.cv$krige.cv_output$observed)
  
# Station output
return(list("spatial"=ok_sph,
            "cv"=ok_sph.cv,
            "stations"=output,
            "statistical_summary_cv"=statistical_sum.cv,
            "statistical_summary_pred"=statistical_sum.pred))
}
