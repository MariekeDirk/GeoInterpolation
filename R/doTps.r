#' Provides a krige (automap, gstat) like interface to Tps (fields)
#' 
#' @param formula A formula to define the dependent and the independent variables, see the documentation of krige.
#' @param data The input data, should be a spatial object which supports coordinates extracting through coordinates()
#' @param newdata A spatial object with the prediction locations.
#' @param debug.level standard value set to 1
#' @param addFit logical, whether or not the output contains the fitted Tps model. See the value section below for more details.
#' @param ... parameters that are passed on to Tps
#' @return a spatial object with the same class as \code{newdata} with the prediction and the variance (NA in this case).
#'         The names of the columns match the outcome of the krige function. When \code{addFit} equals \code{TRUE}
#'         the function returns an autoKrige object with two elements: \code{krige_output} with the Tps prediction and variance
#'         and \code{fit} with the fitted Tps model.
#' @author Paul Hiemstra, \email{p.h.hiemstra@gmail.com}
#' @examples
#' library(sp)
#' data(meuse)
#' coordinates(meuse) = ~x+y
#' data(meuse.grid)
#' gridded(meuse.grid) = ~x+y
#' 
#' meuse_tps = doTps(zinc~dist, meuse, meuse.grid)
#' meuse_tps = doTps(zinc~1, meuse, meuse.grid)
#' 
#' summary(meuse_tps$krige_output)
#' meuse_tps$fit
#' @import fields
#' @export
doTps = function(formula, data, newdata, ..., debug.level = 1, addFit = FALSE) {
  requireNamespace("fields", quietly = TRUE)
  if(debug.level > 0) cat("[using thin plate splines (from fields)]\n")
  f = as.character(formula)
  dependent = f[2]
  independent = f[3] ## Meerdere covariates zou moeten kunnen
  if(independent == "1") {
    fit = Tps(x = coordinates(data), Y = data[[dependent]], ...)
    newdata$var1.pred = as.numeric(predict(fit, coordinates(newdata)))
  } else {
    fit = Tps(x = coordinates(data), Y = data[[dependent]], Z = data[[independent]], ...)
    newdata$var1.pred = as.numeric(predict(fit, coordinates(newdata), Z = newdata[[independent]]))
  }
  newdata$var1.var = NA
  if(debug.level > 1) print(fit)
  if(addFit) {
    ret = list(krige_output = newdata, fit = fit)
    class(ret) = c("autoKrige", "list")
  } else {
    ret = newdata
  }
  return(ret)  
}

# # ## Beetje draaien aan lambda
# library(automap)
# data(meuse)
# coordinates(meuse) = ~x+y
# data(meuse.grid)
# gridded(meuse.grid) = ~x+y
# 
# meuse_tps1 = doTps(zinc~dist, meuse, meuse.grid)
# meuse_tps2 = doTps(zinc~1, meuse, meuse.grid)
# 
# lambda_range = seq(1e-6, 5e-3, length = 25)
# meuse_lambda = lapply(lambda_range, function(x) doTps(zinc~1, meuse, meuse.grid, lambda = x, addFit = TRUE))
# plotautoKrigeMaps(meuse_lambda, labels = round(lambda_range, digits = 6))


