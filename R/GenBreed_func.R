#' Function for generalising breedcombinations usee in Vinje et.al.
#' 
#' @param bb: Breed combination abbreviation. string of 4 characters, i.e. 
#' 'DDLL' etc.
#' @param KeepBreed: (vector) with one letter abbreviations for breeds to 
#' keep in output.

#' @return Four letter abbreviation, where breeds not part of "KeepBreed" are 
#' simplified to in order "X", "Y", "L" and "K".
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' GenBreed_func('DDLL')
#' GenBreed_func('DDLL',KeepBreed = 'D')
#' 
#' @export
#' 

GenBreed_func <- function(bb,KeepBreed = 'P')
{
  xx <- table(strsplit(bb,''))
  t_names <- names(xx)
  xx <- as.numeric(xx)
  names(xx) <- t_names
  xx <- sort(xx,decreasing = TRUE)
  res <- rep(KeepBreed,max(c(xx[is.element(names(xx),KeepBreed)],0)))
  if(length(res)>0){xx <- xx[-which(names(xx)==KeepBreed)]}
  if(length(xx)>0){for(ii in 1:length(xx)){res <- c(res,
                                          rep(c('X','Y','L','K')[ii],xx[ii]))}}
  return(paste(res,collapse=''))
}
