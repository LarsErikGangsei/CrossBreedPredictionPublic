#' Function used in app. Returns the prior distribution as, where the support 
#' for Unknown breed and informative prior might be manipiulated by the input
#' 
#' @param delta_i scalar in interval [0 1]. delta_i = 0 is no weight to informative 
#' prior, whereas delta_i = 1 is the informative prior.
#' @param delta_UK scalar in interval [-1 1]. delta_UK = -1 is zero prior 
#' probability for unknown breed, whereas  delta_UK = 1 is prior probability 1 for
#' unknown breed.
#' @param Train Either 'P+' or 'P-', i.e. which training set to use. 

#' @return a named vector (possible breedcombinations) with prior probabilities.
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @export

Prior_func <- function(delta_i=0,delta_UK=0,Train = 'P+')
{
  mm <- dim(Prior[[Train]])[1]
  delta_UK <- (1/mm)*(1+ifelse(delta_UK<0,delta_UK,delta_UK*(mm-1)))  
  flat <- c(rep((1-delta_UK)/(mm-1),mm-1),delta_UK)
  infPB <- c(Prior[[Train]]$value[1:(mm-1)]*(1-delta_UK),delta_UK)
  res <- flat*(1-delta_i) + infPB*delta_i
  names(res) <- Prior[[Train]]$BreedComb
  return(res)
}


