#' Function used in app. Returns (hard) predictions for the different individuals
#' 
#' @param PriorDist named vector with prior probabilities for different
#' breedcombinations. Length m
#' @param LogLike matrix of size n x m with likelihoods for the n individuals to
#' be classified crossed over the m possible breed combinations. Column names
#' as four letter abbrivations defing the possible breeds. 

#' @return A vector of length n containing four letter abbrivations for hard
#' predictions of breed combinations.
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @export

Pred_func <- function(PriorDist,LogLike)
{
  nn <- dim(LogLike)[1]
  mm <- dim(LogLike)[2]
  return(colnames(LogLike)[apply(LogLike+matrix(log(PriorDist),nn,mm,byrow=TRUE),
                                          1,which.max)])
}




