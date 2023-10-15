#' Function for redefining breed combinations as 4 letter abbreviations (example 'DDLW') 
#' to numerical vectors (\eqn{\Delta}'s)

#' @param Bvec: a vector of length n, each string represents a letter abbrivation for 
#' on individual.
#' @param Breeds: The (m number of) breeds to be included in \eqn{\Delta}'s, either as one string element
#' ("DHLPW" which is the default) or as a vector defining the breeds ("c('D','H','L','P','W')").

#' @return A matrix of size n x m.
#' 
#' @author Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' BreedToMat(c('DDLW','HHLW'))
#' BreedToMat(c('DLDW','HHLW'))
#' BreedToMat(c('DLDW','HHLW'),'abDH')



#' @export

BreedToMat <- function(Bvec,Breeds='DHLPW')
{
  if(length(Breeds)==1){Breeds <- strsplit(Breeds,split='')[[1]]}
  Breeds <- sort(unique(Breeds))
  res <- matrix(NA,length(Bvec),length(Breeds),dimnames = list(Bvec,Breeds))
  
  
  for(bb in Breeds)
  {
    res[,which(Breeds==bb)] <- sapply(lapply(sapply(as.list(Bvec),strsplit,
                                  split=''),is.element,bb),sum)/4
  }
  return(res)
}
  
