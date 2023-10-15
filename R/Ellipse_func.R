#' Function used in app. Returns an ellipse element with isovalue defined by the 
#' log posteriori value for "Unknown breed"
#' 
#' @param log_unknown A posteriori value (product of prior x likelihood) for unknown 
#' breed at the log scale.
#' @param mu Expected value for the CB in question.
#' @param Sigma Covariance matrix for the CB in question
#' @param log_prior Prior probability for CB in question at log scale. 

#' @import rgl

#' @return a ellpise3d object
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @export

Ellipse_func <- function(log_unknown,mu,Sigma,log_prior)
{
  mm <- length(mu)
  Z_val <- seq(0,50*mu[1]/sqrt(Sigma[1,1]/mm),length.out = 5000)
  h_vec <- (dnorm(Z_val,log=TRUE)+log_prior)
  Z_val <- Z_val[which.min((h_vec-log_unknown)^2)]
  return(ellipse3d( x = Sigma,centre = mu, t = Z_val))
}
  
  
