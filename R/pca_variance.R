#'
#'\code{calculates variance from the sandard deviation of a principle component analysis}
#'@title pca variance
#'@param sdev standard deviation of the PCA
#'@author Marieke Dirksen
#'@export

pca_variance<-function(sdev){
  proportion_variance<-sdev^2/sum(sdev^2)*100
  cumulative_proportion_variance<-cumsum(proportion_variance)
  return(list("proportion_variance"=proportion_variance,"cumulative_proportion_variance"=cumulative_proportion_variance))
}
