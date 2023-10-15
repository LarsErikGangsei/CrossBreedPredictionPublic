#' Function to go from a "soft" to hard prediction for crossbreed pigs.

#' @param Soft: Matrix (or numeric vector), with soft predictions. Each row represents
#' one individual. Colnames (or names if vector) defines PB's with softbreeds.
#' @param method: Eiher 'Sq_loss' or'KL_dist', defining the loss function to be used.
#' @param PBs: Vector of letters defining which PB's that should be included as 
#' possible PB grandparents. Of the form  "c('D','H','L','P','W')". Only capital letters 
#' 'D','H','L','P','W' are allowed. Default value is c('D','H','L','P','W'). PBs must
#' include all PB's defined in "Soft" and possibly more.
#' @param pred_min: Passed to "KL_dist_CBpred_func()". Elements <0 or >1 in 
#' probability vector to be evaluated for Kullbach-Leibler divergenses are inflated to 
#' 0 + pred_min or truncated to 1 - pred_min.

#' @return A vector of length equal to dim(Soft)[1] with four letter abbreviations, containg 
#' "hard predictions" for cross breed combinations.
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' To come


#' @export
SoftToHard_func <- function(Soft,method = c('Sq_loss','KL_dist'),
                            PBs = c('D','H','L','W','P'),pred_min=10^(-10))
  {
  if(class(Soft)[1] == 'numeric'){Soft <- matrix(Soft,1,length(Soft),
                                          dimnames=list(NULL,names(Soft)))}
  
  colnames(Soft) <- substr(colnames(Soft),1,1)
  
  nn <- dim(Soft)[1]
  mm <- length(PBs)
  
  Soft <- cbind(Soft,matrix(0,nn,mm-dim(Soft)[2],dimnames = list(NULL,
                                PBs[!is.element(PBs,colnames(Soft))])))
  Soft <- Soft[,order(colnames(Soft))] 
  PBs <- sort(PBs)
  
  ## Possible prediction based on PBs
  #BreedCombMat <- comboGeneral(PBs,4,repetition = TRUE)
  #BreedCombs <- sapply(split(BreedCombMat,f=1:dim(BreedCombMat)[1]),
  #                     paste,collapse = '')
  BreedCombs <-  sort(unique(apply(t(apply(as.matrix(expand.grid(rep(list(PBs),4))),1,sort)),
                                   1,paste,collapse = '')))
  nn_comb <- length(BreedCombs)
  
  Real_list <- split(BreedToMat(BreedCombs,
                                Breeds = paste(PBs,collapse ='')),
                     f = 1:length(BreedCombs))
  names(Real_list) <- BreedCombs
  
  
  if(method=='Sq_loss')
  {
    res <- sapply(split(Soft,f=1:nn),
                  function(x,y)
                  {return(names(y)[
                    which.min(sapply(y,function(yy)
                    {return(sum((yy-x)^2))}))])},
                  y = Real_list)
  }
  if(method=='KL_dist')
  { 
   res <- sapply(split(Soft,f=1:nn),
                     function(x,y)
                     {return(names(y)[
                       which.min(sapply(y,KL_dist_CBpred_func,
                            Predicted=x,pred_min=pred_min))])},
                     y = Real_list)
  }
  return(res)
}
  
  
  
