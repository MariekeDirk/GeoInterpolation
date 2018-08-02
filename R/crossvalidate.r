# Creates an empty crossvalidation result object from 'obj'
#' @title create a object for cross validation
#' @description creating a cross validation object from spatial object
#' @param obj a object 
createCvObjFromSpatialObj = function(obj) {
  cv_obj = obj
  cv_obj$observed = NA
  cv_obj$var1.pred = NA
  cv_obj$var1.var = NA
  cv_obj$residual = NA
  cv_obj$zscore = NA
  return(cv_obj[c("var1.pred","var1.var","observed", "residual", "zscore")])
}

#' This function performs crossvalidation in the style of krige.cv
#'
#' In addition to supporting kriging and idw, it also supports 
#' thin plate splines (through Tps (fields)). 
#' 
#' @param formula A formula to define the dependent and the independent variables, see the documentation of krige.
#' @param data The input data, should be a spatial object which supports coordinates extracting through coordinates()
#' @param func The function which should be used for cross-validation, possibilities are "krige", "idw", "doNearestNeighbor" 
#'              and "doTps".
#' @param ... parameters that are passed on to 'func'
#' @return The function returns a spatial object with the class of 'data'. The format 
#'         in which the cross-validation results are presented is equal to that of krige.cv.
#' @note At this stage only leave-one-out cross-validation is supported. In addition, the formula supports only a limit
#'       syntax, i.e. log(zinc) or sqrt(dist) are not possible in the formula. Instead, create a new column in the object,
#'       e.g. meuse$log_zinc = log(meuse$zinc).
#' @author Paul Hiemstra, \email{p.h.hiemstra@gmail.com}
#' @export
#' @import fields
#' @examples
#' library(automap)
#' library(sp)
#' library(gstat)
#' data(meuse)
#' coordinates(meuse) = ~x+y
#' data(meuse.grid)
#' gridded(meuse.grid) = ~x+y
#' 
#' regres_cv = crossvalidate(zinc~dist, meuse, func = "krige", debug.level = 0)
#' nearestneighbor_cv = crossvalidate(zinc~1, meuse, func = "doNearestNeighbor")
#' idw2_cv = crossvalidate(zinc~1, meuse, func = "idw", debug.level = 0)
#' idw4_cv = crossvalidate(zinc~1, meuse, func = "idw", debug.level = 0, idp = 4)
#' idw05_cv = crossvalidate(zinc~1, meuse, func = "idw", debug.level = 0, idp = 0.5)
#' ked_cv = crossvalidate(zinc~dist, meuse, func = "krige", 
#'         model = autofitVariogram(zinc~dist, meuse, model = "Ste")$var_model, debug.level = 0)
#' tps_cv = crossvalidate(zinc~1, meuse, func = "doTps", debug.level = 0)
#' tpsdist_cv = crossvalidate(zinc~dist, meuse, func = "doTps", debug.level = 0)
#' 
#' compare.cv(regres_cv, idw2_cv, idw4_cv, idw05_cv, tps_cv, tpsdist_cv, ked_cv)
 
crossvalidate = function(formula, data, func = "doTps", ...) {
  pb = txtProgressBar(1, nrow(data), 1, style = 3)
  dependent = as.character(formula)[2]
  cv_obj = createCvObjFromSpatialObj(data)
  for(i in 1:nrow(data)) {
    setTxtProgressBar(pb, i)
    newdata = data[i,]
    cv_data = data[-i,]
    cv_pred = do.call(func, list(formula, cv_data, newdata, ...))
    cv_obj@data[i,"var1.pred"] <- cv_pred$var1.pred
    cv_obj@data[i,"var1.var"] <- cv_pred$var1.var
    cv_obj@data[i,"observed"] <- data[i,][[dependent]]
  }
  cv_obj$residual = cv_obj$observed - cv_obj$var1.pred
  cat("\n")
  return(cv_obj)
}




