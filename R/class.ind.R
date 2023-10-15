#' Function for making an indicator matrix based on vector of factors/ levels

#' @param x Vector of length n, which might be transformed to factor by as.factor(x)

#' @return A matrix of size n x m, where m is the number of levels in x (as.factor(x)). 
#' The matrix has column names as given by levels(as.factor(x)), and contains values 0 or 1 
#'
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' class.ind(as.factor(rep(1:4,each = 5)))


#' @export

class.ind <- function(x)
{
  if(class(x)=='character'){x <- as.factor(x)}
  c_names <- levels(x)
  x <- model.matrix(~x-1)
  mostattributes(x) <- list(dim=dim(x), dimnames=list(NULL, 1:dim(x)[2]))
  colnames(x) <- c_names
  x
}
