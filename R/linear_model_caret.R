#'
#'@title Linear model Caret
#'@param groundstations Data from the ground station in spatial format without na values
#'@param method.overlay Method to overlay the points and the grid, choose between over (1 cell) and extract function (which averages over 4 nearest cells)
#'@param variable the variable of the groundstations you want to interpolate
#'@param grid_prediction trend grid for the interpolation with same spatial extent as groundstations
#'@param length tune length of the model
#'@param method model caret
#'@param formula formula used for the kriging interpolation
#'@description Function overlays the ground stations and the grid, trains and predicts using the caret package function train.
#'@details after running this function take a look at \code{get_statistical_summary}
#'@return returns a list with the interpolation output and a dataframe with the difference between your observations and predicted grid
#'@author Marieke Dirksen
#'@export
linear_model_caret<-function(groundstations,
                                     method.overlay='extract',
                                     formula,
                                     variable,
                                     grid_prediction,
                                     length=20,
                                     method="lm"
                                     ){
  requireNamespace("raster", quietly = TRUE)
  requireNamespace("fields", quietly = TRUE)
  requireNamespace("gstat", quietly = TRUE)
  requireNamespace("sp", quietly = TRUE)
  requireNamespace("caret",quietly = TRUE)
  
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
  
  if(!inherits(grid_prediction,"SpatialGridDataFrame")){
    message("grid_drift is not in the format of a SpatialGridDataFrame, returning FALSE")
    return(FALSE)
  }
  ##########################################################################
  names(groundstations)[names(groundstations)==variable] <- "Tint"
  

  
  # over functions
  #names(grid_drift)<-"distshore"
  
  # over Distshore on Var (choose between 2 methods)
  if(method.overlay == 'over'){
    
    distshore.ov = sp::over(groundstations , grid_prediction)
    
  } else if(method.overlay == 'extract'){
    
    distshore.ov = extract(stack(grid_prediction),
                           groundstations,
                           method = 'bilinear',
                           fun = mean,
                           df = TRUE) # interpolated with the 4 nearest raster cells
    
  } else {message("unknow overlay method") 
    return(FALSE)}
  # Copy the values to Var )
  
  var = groundstations
  
  
  var@data[names(grid_prediction)]=distshore.ov[names(grid_prediction)]
  
  #Prepare input  
  field = grid_prediction
  field@data = cbind(field@data, coordinates(field))
  names(field@data) = c("s","x","y")
  #field$log = log(field$s) 
  #for(fieldlog in field$s) {try(print(paste("log of", fieldlog, "=", log(fieldlog))))}
  
  #field$log=fieldlog
  var$x = sp::over(var,field)$x 
  var$y = sp::over(var,field)$y
  var$s = sp::over(var,field)$s
  
  # Remove nodata from dataframe based on missing distshore
  var = var[complete.cases(var@data[names(grid_prediction)]),]
  
  ###############################################################################################
  ############HERE THE INTERPOLATION STARTS######################################################
  ###############################################################################################
  # control<-trainControl(method = "repeatedcv",
  #                       repeats = 10,
  #                       number = 2,
  #                       returnData = FALSE) 
  
  mod<-caret::train(form=formula,
               data=data.frame(var),
               method=method,
               tuneLength=length,
               trControl = trainControl(method = "boot_all")
               )
  pred<-raster::predict(object=stack(grid_prediction),model=mod)
  
  pred.point<-extract(pred,groundstations)
  R2.traditional<-caret::R2(pred=pred.point,obs=var$Tint,formula="traditional")
  RMSE.pred<-caret::RMSE(pred=pred.point,obs=var$Tint)
  mod$results$r2_tradiational<-R2.traditional
  mod$results$RMSE_pred<-RMSE.pred
 
 return(list("spatial"=pred,"model"=mod,"statistical_summary"=mod$results))
}
