#' Function to go from a "soft" to hard prediction for crossbreed pigs.

#' @param Posteriors: Matrix (or numeric vector), with posterior probabilities for different 
#' crossbreed combinations defined b colomnnames (or names if vector).
#' @param PBs: Vector of letters defining which PB's that should be included as 
#' possible PB grandparents. Of the form  "c('D','H','L','P','W')". Only capital letters 
#' 'D','H','L','P','W' are allowed. Default value is c('D','H','L','P','W'). PBs must
#' include all PB's defined in names of "Posterior" and possibly more.

#' @return A matrix of dimentions dim(Posterior)[1] x length(PBs) defining soft breed prdictions
#' based on the posterior given as input. Unknown breed not included.
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' To come


#' @export
PosteriorToSoft_func <- function(Posteriors,PBs = c('D','H','L','P','W'))
{
  if(class(Posteriors)[1] == 'numeric'){Posteriors <- matrix(Posteriors,1,length(Posteriors),
                                                 dimnames=list(NULL,names(Posteriors)))}
  
  PBs <- sort(PBs)
  B_mat <- BreedToMat(colnames(Posteriors),Breeds = PBs)
  res <- array(B_mat,dim = c(dim(B_mat),dim(Posteriors)[1]))
  for(ii in 1:dim(B_mat)[2]){res[,ii,] <- res[,ii,]*t(Posteriors)}
  res <- t(apply(res,3,colSums))
  colnames(res) <- PBs
  return(res)
}
            