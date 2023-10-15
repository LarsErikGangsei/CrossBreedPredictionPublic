#' Function for predicting cross breed combination based on DNA data (GeneSeek 50K (Illumina) SNP chip). from files (.ped or .txt)

#' @param DNA: One of the following: \cr 
#' - A list with elementsts: 1) Scores: PLS-scores, 2) RF_pred: soft predictions from Random Forest
#' and 3) PLSR_pred: soft predictions from PLSR. 
#' - matrix (n x m) with 0-1-2 coding of SNP's to be evaluated\cr
#' - data.frame (n x 2m) with "A", "C", "G", "T" coding of SNP's to be evaluated\cr
#' - a path to a txt - file with ped data possible to read with "read_ped_func()"
#' @param Train: Either 'TrainP+' (default), or 'TrainP-', indicating if training data including or
#' omitting PB pietrain is to be used. 
#' @param alpha0: Deafault value is 73.58105, i.e. the free parameter \eqn{\alpha_0} used 
#' in calculation of \eqn{V(\theta)}.
#' @param Unkn_LogLike: the log likelihood for the uniform distribution for unknown breed in PLS-QDA. 
#' Defaults value is NULL, for which the value is calculated based on the m dimentional
#' space spanned bu the scores in the PLS-model for PB's.
#' @param PriorDist: "NULL" (default) for which a totally flat prior is applied, 
#' 'informative' for which the informative prior in Vinje et.al.#' is applied, 
#' or a scalar >0 and <1 defining the prior probability for class "Unknown" 
#' and a flat prior is applied to 
#' the rest of the classes, 
#' @param pred_min: Parameter to be passed to "KL_dist_CBpred_func()". Default to 10^(-10).
#' 
#' @import mvtnorm
#' 
#' @return a list bla bla
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' To come


#' @export

PredictCB_func <- function(DNA=NULL,Predictions = NULL, Train = 'TrainP+',
                           alpha0 = 73.58105,Unkn_LogLike=NULL,PriorDist = NULL,
                          pred_min = 10^(-10))
{
  if(!is.list(DNA))
  {
  # DNA to 0-1-2 coding
  if(class(DNA)[1]=='character'){DNA <- read_ped_data(DNA)}
  if(dim(DNA)[2]==(2*dim(PigBreedPrediction_map)[1])){DNA <- pedTo012_func(DNA)}
    message('Data loaded and prediction is ongoing')
    Scores <- predict(Mod_pls[[Train]],type='scores',newdata = DNA,
                      comps = 1:Mod_pls[[Train]]$ncomp)
    RF_pred <- predict(Classifier_RF[[Train]],newdata = DNA, type = "prob")
    PLSR_pred <- predict(Mod_pls[[Train]], newdata = DNA,
                           type = "response",comps = 1:Mod_pls[[Train]]$ncomp)
    ID <- rownames(DNA)
  }else{
    Scores <-  DNA$Scores
    RF_pred <- DNA$RF_pred
    PLSR_pred <- DNA$PLSR_pred
    ID <- rownames(Scores)
  }
    
  # Define dimentions, breeds to be evaluated and number of PLS components
  nn_new <- dim(Scores)[1]
  mm <- dim(Scores)[2]
  
  # Likelihoods for PLS-QDA
  T_likelihood <- LikelihoodParameters_func(Train,alpha0)
  
  nn_comb <- length(T_likelihood)
  
  # Log - Likelihood for uniform distribution for unknown breed
  if(is.null(Unkn_LogLike))
  {Unkn_LogLike <- -sum(log(apply(apply(Mod_pls[[Train]]$scores,2,range),2,diff)))}
  
  
  # Construct the posterior distribution
  Posterior_distribution <- vector('list',length=nn_comb+1)
  names(Posterior_distribution) <- c(names(T_likelihood),'Unknown')
  
  # Get the prior distribution
  if(is.null(PriorDist)){
    Prior_dist <- split(rep(1,nn_comb+1)/(nn_comb+1),
                        f=names(Posterior_distribution))
  }else{
    if(PriorDist == 'informative')
    {if(Train == 'TrainP+'){Prior_dist <- split(Prior$`P+`$value,f=Prior$`P+`$BreedComb)
    }else{Prior_dist <- split(Prior$`P-`$value,f=Prior$`P-`$BreedComb)}
    }else{Prior_dist <- split(c(rep(1-PriorDist,nn_comb)/nn_comb,PriorDist),
                          f=names(Posterior_distribution))
      }
  }
  
  for(bb in names(T_likelihood))
  {Posterior_distribution[[bb]] <- (dmvnorm(Scores,mean = T_likelihood[[bb]]$mu,
                                                 sigma = T_likelihood[[bb]]$cov,
                                           log=TRUE)+log(Prior_dist[[bb]]))}
  
  Posterior_distribution$Unknown <- rep(Unkn_LogLike+log(Prior_dist$Unknown),nn_new)
  
  Log_posteriors <- matrix(unlist(Posterior_distribution),
                           nn_new,nn_comb+1,byrow=FALSE)
  
  Log_posteriors <- Log_posteriors - matrix(apply(Log_posteriors,1,max),
                                            nn_new,nn_comb+1,byrow=FALSE)
  
  Posteriors <- exp(Log_posteriors)
  Posteriors <- Posteriors/matrix(rowSums(Posteriors),
                                  nn_new,nn_comb+1,byrow=FALSE)
  colnames(Posteriors) <- names(Posterior_distribution)
  
 
  
  res <- data.frame(ID = ID,
                  RF = I(RF_pred),
                  PLSR = I(PLSR_pred),
                  PLSQDA = I(PosteriorToSoft_func(Posteriors,
                             PBs = intersect(c('D','H','L','P','W'),
                                  unique(strsplit(paste(colnames(Posteriors),
                                                        collapse=''),'')[[1]])))),
                  PLSQDA_Posteriors = I(Posteriors),
                  PLSScores = I(Scores))
  
 
  return(res)
  
}
