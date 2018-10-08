#'
#'@title Statistics from interpolation
#'@param observed observation from your groundstations
#'@param prediction prediction at the location of the groundstations (either from the cv or prediction)
#'@description makes a statistical summary list from the output of interpolation kriging
#'@details look at \code{interpolation_kriging}
#'@author Marieke Dirksen
#'@export
get_statistical_summary<-function(observed,prediction){
requireNamespace("ModelMetrics")
n <- sum(complete.cases(prediction))
#r2
teller<-sum((observed-prediction)^2)
noemer<-sum((observed-mean(observed))^2)
# r2<-1-teller/noemer
r2<- 1 - (sum((observed - prediction)^2, na.rm = TRUE)/((n - 1) * var(observed, na.rm = TRUE)))

#rmse
RMSE<-sqrt(n*(sum(prediction)-sum(observed))^2)
RMSEsd<-RMSE/sqrt(1/length(noemer)*sum(noemer))

#ME
ME<-n*(sum(prediction)-sum(observed))
MEmean<-ME/mean(observed)

#MAE
MAE<-mae(observed,prediction)

return(list("R2"=r2,"RMSE"=RMSE,"RMSEsd"=RMSEsd,"ME"=ME,"MEmean"=MEmean,"MAE"=MAE))
}
