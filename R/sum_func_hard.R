#' Function used for function "xtable" in Vinje et.al.....
#' 
#' @param x logical vector
#' @param digits passed direcetely to "format(...,digits = digits)"

#' @return a string with "percentage of x is TRUE (n = length(x))"
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' sum_func_hard(rnorm(10)<0)
#' sum_func_hard(rnorm(100)<0)
#' sum_func_hard(rnorm(10^5)<0)
#' 
#' @export
#' 
sum_func_hard <- function(x){paste(sum(x),' (',
                                   format(100*sum(x)/length(x),
                                          digits = 2),'\\%)',sep='')}

