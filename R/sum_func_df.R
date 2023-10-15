#' Function used for function "xtable" in Vinje et.al.....
#' 
#' @param df data frame whose elements are either numerical or logical
#' @param return_order vector defining order of returned elements

#' @return a vector of length dim(df)[2] containing mean +/- standard deviations
#' for numerical elements and the sum and %-age in() of "TRUE"'s for logical elements
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' Dist_mat <- Dist_eval_func(RF_Sim_predictions$`TrainP+`,rownames(RF_Sim_predictions$`TrainP+`))
#' sum_func_df(Dist_mat)
#' 
#' @export
#' 
sum_func_df <- function(df,return_order = c(1,3,2,4))
{
  res <- rep(NA,dim(df)[2])
  for(jj in 1:dim(df)[2])
  {if(class(df[,jj])[1]=='numeric'){res[jj] <- sum_func_soft(df[,jj])
    }else{res[jj] <- sum_func_hard(df[,jj])}}
  names(res) <- names(df)
  res <- res[return_order]
  return(res)
}
