#' Function for calculating parameters \eqn{\mu_i}'s (mean vector) and \eqn{\Sigma_i}'s in the 
#' likelihood functions used for PLS-QDA

#' @param Train: Either 'TrainP+' (default), or 'TrainP-', indicating if training data including or
#' omiting PB pietrain is to be used. 
#' @param alpha0: Deafault value is 73.58105, i.e. the free parameter \eqn{\alpha_0} used 
#' in calculation of \eqn{V(\theta)}.


#' @return A list with the possible breedcombinations from input PBs. Named with 4 letter abbreviations.
#' 
#' @author Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' Default_alpha <- LikelihoodParameters_func()
#' Small_alpha <- LikelihoodParameters_func(alpha0=2)
#' Default_alpha$DDLW
#' Small_alpha$DDLW


#' @export

LikelihoodParameters_func <- function(Train = 'TrainP+',alpha0 = 73.58105)
  {
  
    if(Train =='TrainP+'){PBs <- c('D','H','L','P','W')
    }else{PBs <- c('D','H','L','W')}
  #BreedCombMat <- comboGeneral(sort(PBs),4,repetition = TRUE)
  #nn_comb <- dim(BreedCombMat)[1]
  #BreedCombs <- sapply(split(BreedCombMat,f=1:nn_comb),paste,collapse = '')
  BreedCombs <-  sort(unique(apply(t(apply(as.matrix(expand.grid(rep(list(PBs),4))),1,sort)),
                                   1,paste,collapse = '')))
  nn_comb <- length(BreedCombs)
  res <- vector('list',nn_comb)
  names(res) <- BreedCombs
  
 for(bb in strrep(PBs,4))
    {
      XX <- Mod_pls[[Train]]$scores[Mod_pls[[Train]]$Breed==bb,]
      res[[bb]] <- list(mu = colMeans(XX),cov = var(XX))
    }
  
  
    PBPar <- res[strrep(sort(PBs),4)]
    muPluss <- sapply(PBPar,function(x) x$mu)
    mm <- dim(muPluss)[1]
    qq <- dim(muPluss)[2]

    for(bb in setdiff(BreedCombs,strrep(sort(PBs),4)))
    {
        Delta <- as.vector(BreedToMat(bb,paste(sort(PBs),collapse='')))
        varTheta <- (diag(Delta) - as.matrix(Delta)%*%t(Delta))/(alpha0+1)
        res[[bb]] <- list(mu = as.vector(muPluss%*%as.matrix(Delta)),
                    cov = matrix(rowSums(mapply('*',lapply(PBPar,
                              function(x) x$cov),split(Delta^2+diag(varTheta),1:qq))),mm,mm)+
                      muPluss%*%varTheta%*%t(muPluss))
    }
      
 return(res)
}
  
  
  
