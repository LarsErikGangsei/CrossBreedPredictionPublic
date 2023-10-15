#' Function for evaluating Kullback Leibler distances and square losses from a
#' matrix where rows contain soft predictions and a vector defining the true 
#' breeds. Used for ecvaluation of simulated data in Vinje et.al.....
#' 
#' @param Soft matrix of size n x q where each row represent a soft prediction of 
#' an individual.
#' @param TrueBreed vector of length n containing the true breed combinations as four letter abbrivations.
#' @param PBs: Vector of letters defining which PB's that should be included as 
#' possible PB grandparents. Of the form  "c('D','H','L','P','W')". Only capital letters 
#' 'D','H','L','P','W' are allowed. Default value is c('D','H','L','P','W'). PBs must
#' include all PB's defined in names of "Soft" and possibly more.

#' @return A data frame of size n x 4 with the four elements:\cr
#' -KL_dist: Kullback leibler distances between soft and true predications.\cr
#' -Sq_loss: Square losses between soft and true predications\cr
#' -KL_hardPred: Hard prediction as the crossbreed combination with smallest KL distance to soft prediction\cr
#' -Sq_hardPred Hard prediction as the crossbreed combination with smallest squared loss to soft prediction\\cr
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' Dist_eval_func(RF_Sim_predictions$`TrainP+`,rownames(RF_Sim_predictions$`TrainP+`))
#' @export

Dist_eval_func <- function(Soft,TrueBreed,PBs = c('D','H','L','P','W'))
{
  message('Process is ongoing, might be slow (minutes) for n = 7000')
  PBs <- sort(PBs)
  colnames(Soft) <- substr(colnames(Soft),1,1)
  nn <- dim(Soft)[1]
  mm <- length(PBs)
  Soft <- cbind(Soft,matrix(0,nn,mm-dim(Soft)[2],dimnames = list(NULL,
                                        PBs[!is.element(PBs,colnames(Soft))])))
  Soft <- Soft[,order(colnames(Soft))] 
  TBreeds <- BreedToMat(TrueBreed,Breeds = PBs)
  res <- data.frame(KL_dist = mapply(KL_dist_CBpred_func,
                                     split(TBreeds,1:nrow(TBreeds)),
                                     split(Soft,1:nrow(Soft)),
                                     pred_min = 10^(-10)),
                    Sq_loss = mapply(function(x,y){(sum((x-y)^2))},
                                     split(TBreeds,1:nrow(TBreeds)),
                                     split(Soft,1:nrow(Soft))),
                    KL_hardPred = SoftToHard_func(Soft,method='KL_dist',PBs = PBs)==TrueBreed,
                    Sq_hardPred = SoftToHard_func(Soft,method='Sq_loss',PBs = PBs)==TrueBreed)
  return(res)
}



sum_func_df <- function(df)
{
  sum_func_soft <- function(x){paste(format(mean(x),digits = 2,scientific = 15),
                          '$\\pm$',format(sd(x),digits = 2,scientific = 15))}
  
  sum_func_hard <- function(x){paste(sum(x),' (',
                                     format(100*sum(x)/length(x),
                                            digits = 2),'\\%)',sep='')}
  res <- c(sum_func_soft(df[,1]),sum_func_hard(df[,3]),
           sum_func_soft(df[,2]),sum_func_hard(df[,4]))
  names(res) <- names(df)
  return(res)
}

