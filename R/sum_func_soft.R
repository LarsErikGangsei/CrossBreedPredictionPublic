#' Function used for function "xtable" in Vinje et.al.....
#' 
#' @param x numerical vector
#' @param digits passed direcetely to "format(...,digits = digits)"
#' @param scientific passed direcetely to "format(...,scientific = digits)"

#' @return a string with "mean(x) +/- sd(x)"
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' sum_func_soft(rnorm(10))
#' sum_func_soft(rnorm(100))
#' sum_func_soft(rnorm(10^5))
#' 
#' @export
#' 
sum_func_soft <- function(x,digits = 2,scientific = 15)
  {paste(format(mean(x),digits = 2,scientific = 15),
                            '$\\pm$',format(sd(x),digits = 2,scientific = 15))}
  

