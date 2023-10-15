#' Function for calculating Kullback Leibler divergence between real and predicted 
#' CB combinations

#' @param Real: Vector of length m (number of possible PB), with proportions of each 
#' PB as quarters, i.e. assuming grandparents to be PB
#' @param Predicted: Vector of length m (number of possible PB), with predicted (soft)
#' proportions for each PB. The order of PB's in Real and Predicted must be the same.
#' @param pred_min: Elements <0 or >1 in Predicted are inflated to 0 + pred_min or truncated 
#' to 1 - pred_min.

#' @return a scalar. The Kullback Leibler divergense assuming that 4 x Real is 
#' multinomal distributed, with true probability parameter Real, and predicted probability
#' parameter Predicted.
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' To come..



#' @export
KL_dist_CBpred_func <- function(Real,Predicted,pred_min = 10^(-10))
  {
  if(round(sum(Real))==1){Real <- round(Real*4)}
  Predicted <- pmin(pmax(Predicted,pred_min),1-pred_min)
  return(dmultinom(Real,prob = Real/4,log=FALSE)*(
    dmultinom(Real,prob = Real/4,log=TRUE)-
      dmultinom(Real,prob = Predicted,log=TRUE)))
}
  
  
  
