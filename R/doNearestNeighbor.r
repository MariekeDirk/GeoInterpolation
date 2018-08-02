#' Performs nearest neighbor interpolation
#' 
#' @param formula A formula to define the dependent and the independent variables, see the documentation of krige. 
#'                Note that nearest neighbor does not support independent variables. Therefor, the formula should always 
#'                have the form dependent~1.
#' @param data The input data, should be a spatial object which supports coordinates extracting through coordinates()
#' @param newdata A spatial object with the prediction locations.
#' @param debug.level standard value set to 1
#' @param ... parameters that are passed on to Tps
#' @note This functions uses idw with 'nmax' set to 1 to perform nearest neighbor interpolation.
#' @return The function returns a spatial object with the same class as \code{newdata} with the prediction and the 
#'          variance (NA in this case). The names of the columns match the outcome of the krige function.
#' @author Paul Hiemstra, \email{p.h.hiemstra@gmail.com}
#' @export
#' @examples
#' library(sp)
#' data(meuse)
#' coordinates(meuse) = ~x+y
#' data(meuse.grid)
#' gridded(meuse.grid) = ~x+y
#' 
#' meuse_nn = doNearestNeighbor(zinc~1, meuse, meuse.grid)
#'

doNearestNeighbor = function(formula, data, newdata, debug.level = 1) {
  requireNamespace("gstat", quietly = TRUE)
  if(debug.level > 0) cat("[using nearest neighbor]\n")
  f = as.character(formula)
  dependent = f[2]
  independent = f[3] ## Meerdere covariates zou moeten kunnen
  if(independent != "1") stop("Nearest neighbor does not support independent variables, please formula to 'dependent~1'.")
  newdata = gstat::idw(formula, data, newdata, nmax = 1, debug.level = 0)
  newdata$var1.stdev = sqrt(newdata$var1.var)
  return(newdata)
}
