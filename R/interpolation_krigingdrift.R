#'
#'@title Kriging with External Drift interpolation
#'@param groundstations Data from the ground station in spatial format without na values
#'@param method.overlay Method to overlay the points and the grid, choose between over (1 cell) and extract function (which averages over 4 nearest cells)
#'@param variable the variable of the groundstations you want to interpolate
#'@param grid_drift trend grid for the interpolation with same spatial extent as groundstations
#'@param blocksize kriging interpolation variable, with a standard value of 20000
#'@param mxdkrige kriging interpolation variable, with a standard value of Inf
#'@param mod kriging interpolation variable, standard set to "Exp"
#'@param formula formula used for the kriging interpolation
#'@param conditional.sim boolean, TRUE or FALSE. Standard set to FALSE as this simulation takes more time, nmax=30 and nsim=4. 
#'@description Function overlays the ground stations and the grid and interpolates using autokrige from the \code{\link{autoKrige}}.
#'@details after running this function take a look at \code{get_statistical_summary}
#'@return returns a list with the kriging interpolation output and a dataframe with the difference between your observations and predicted grid
#'@author Marieke Dirksen
#'@export
interpolation_krigingdrift<-function(groundstations,
                                     method.overlay='extract',
                                     formula,
                                     variable,
                                     grid_drift,
                                     blocksize=20000,
                                     mxdkrige=Inf,
                                     mod="Exp",
                                     conditional.sim=FALSE){
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


# over functions
  #names(grid_drift)<-"distshore"
  
  # over Distshore on Var (choose between 2 methods)
  if(method.overlay == 'over'){
  
  distshore.ov = sp::over(groundstations , grid_drift)
  
  } else if(method.overlay == 'extract'){
  
  distshore.ov = extract(stack(grid_drift),
                         groundstations,
                         method = 'bilinear',
                         fun = mean,
                         df = TRUE) # interpolated with the 4 nearest raster cells
  
  } else {message("unknow overlay method") 
          return(FALSE)}
  # Copy the values to Var )

  var = groundstations


  var@data[names(grid_drift)]=distshore.ov[names(grid_drift)]

  #Prepare input  
  field = grid_drift
  field@data = cbind(field@data, coordinates(field))
  names(field@data) = c("s","x","y")
  #field$log = log(field$s) 
#for(fieldlog in field$s) {try(print(paste("log of", fieldlog, "=", log(fieldlog))))}

#field$log=fieldlog
  var$x = sp::over(var,field)$x 
  var$y = sp::over(var,field)$y
  var$s = sp::over(var,field)$s
  
  # Remove nodata from dataframe based on missing distshore
  var = var[complete.cases(var@data[names(grid_drift)]),]
  
###############################################################################################
############HERE THE INTERPOLATION STARTS######################################################
###############################################################################################
### Universal kriging (KED) - variogrammodel GAU ###

# Kriging
ked_gau <- automap::autoKrige(formula=formula, 
                              var, 
                              grid_drift, 
                              maxdist=mxdkrige, 
                              block=c(blocksize,blocksize), 
                              model = c(mod), 
                              na.action=na.pass, 
                              fix.values=c(NA,NA,NA), 
                              miscFitOptions = list(merge.small.bins = TRUE))  #log(distshore)
ked_gau.cv <- automap::autoKrige.cv(formula=formula, 
                                    var, 
                                    model = c(mod),
                                    maxdist = mxdkrige,
                                    fix.values = c(NA,NA,NA), 
                                    miscFitOptions = list(merge.small.bins = TRUE))

  
# Krige Cross validation
ked.zscoremean <- mean(ked_gau.cv$krige.cv_output$zscore)
ked.zscore.var <- var(ked_gau.cv$krige.cv_output$zscore)

# FORCE GRID CELLS TO BE SQUARE
ked_gau=ked_gau$krige_output
slot(slot(ked_gau, "grid"), "cellsize") <- rep(mean(slot(slot(ked_gau, "grid"), "cellsize")), 2)

# Calculate differences at observation points
predicted= sp::over (var,ked_gau)
var$predicted = predicted$var1.pred
var$difference = (var$Tint - var$predicted)
output <- data.frame(var)
#colnames(output)<-c("Stn","Locatie","RDN_X","RDN_Y","observed","predicted","difference")

# statistical_sum<-list("R2"=ked_gau.r2,"diffmin"=ked_gau.difmin)
statistical_sum.cv<-get_statistical_summary(prediction = ked_gau.cv$krige.cv_output$var1.pred,
                                            observed = ked_gau.cv$krige.cv_output$observed)
statistical_sum.pred<-get_statistical_summary(prediction = var$predicted,
                                         observed = ked_gau.cv$krige.cv_output$observed)
if(conditional.sim==TRUE){
  lznr.vgm = variogram(formula, var)
  lznr.fit = fit.variogram(lznr.vgm, model = vgm(mod))
  lzn.condsim<-krige(formula,var,grid_drift,model=lznr.fit,nmax=30,nsim=4)
  
  return(list("spatial"=ked_gau,
              "conditional_simulation"=lzn.condsim,
              "cv"=ked_gau.cv,
              "stations"=output,
              "statistical_summary_cv"=statistical_sum.cv,
              "statistical_summary_pred"=statistical_sum.pred))
}
return(list("spatial"=ked_gau,
            "cv"=ked_gau.cv,
            "stations"=output,
            "statistical_summary_cv"=statistical_sum.cv,
            "statistical_summary_pred"=statistical_sum.pred))
}
