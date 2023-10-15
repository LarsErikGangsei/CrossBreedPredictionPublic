#' A function for fitting PLS, PCA and Random forest models based on PB-data
#' 
#' @param PB_data A data frame containing SNP-data for n = 4014 pure breed pigs. PB_data has elements:\cr
#' - Breed (4 letter abbrivation for real breed) 
#' - ID. Identification of the different individuals. 
#' - XX: Matrix of size (4014 x 23070) containing 0-1-2 coding for the 23070 SNp's. 
#' The column names are the SNP-names. Row names as breed combinations. 
#' 
#' @import pls
#' @import randomForest

#' @return Three objects Mod_pls, Mod_pca and Classififier_RF, all list with two elements:
#' "TrainP+" and "TrainP-", containing models where Pietrain data is used ("TrainP+") or
#' omitted ("TrainP-"). \cr
#' PLS- and PCA- models are fitted by functions plsr() and pcr() from 
#' the "pls" package.\cr 
#' The random forest model is fitted via the randomForest() in 
#' the "randomForest" package. In random forest the weights argument is used in order to
#' increase the weight of Hampshire observations.\cr
#' The three model objects saved in the "CrossBreedPrediction" 
#' package are the same objects as fitted by Fit_Models_func(PB_data), with slight
#' modifications (i.e. raw data are deleted, and a new element "Breed" is added.)
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' Data <- Load_012data_func()
#' attach(Data)
#' Fit_Models_func(Models$PB_data)


#' @export
## Set path to data to be included
Fit_Models_func <- function(PB_data)
{
  PB_data$YY <- I(class.ind(PB_data$Breed))
  # PLS
  Mod_pls <- list(plsr(YY~XX, ncomp=4,data = PB_data),
                    plsr(YY~XX, ncomp=3,data = PB_data[PB_data$Breed!='PPPP',]))
    
  names(Mod_pls) <- c('TrainP+','TrainP-')
  
  # PCA
  Mod_pca <- list(pcr(YY~XX, ncomp=4,data = PB_data),
                    pcr(YY~XX, ncomp=3,data = PB_data[PB_data$Breed!='PPPP',]))
  names(Mod_pca) <- c('TrainP+','TrainP-')
    
  # Random forest
  RF_weightsP <- ((dim(PB_data$YY)[1]/dim(PB_data$YY)[2])*
                      PB_data$YY%*%as.matrix(1/colSums(PB_data$YY))) 
  RF_weightsM <-  (RF_weightsP[PB_data$Breed!='PPPP']/
                       mean(RF_weightsP[PB_data$Breed!='PPPP']))
  
  Classifier_RF <- list(randomForest(x = PB_data$XX,
                                       y = factor(PB_data$Breed),
                                       ntree = 500,
                                       weights = RF_weightsP),
                          randomForest(x = PB_data$XX[PB_data$Breed!='PPPP',],
                                       y = factor(PB_data$Breed[PB_data$Breed!='PPPP']),
                                       ntree = 500,
                                       weights = RF_weightsM))
    
  names(Classifier_RF) <- c('TrainP+','TrainP-')
  return(list(Mod_pls = Mod_pls,Mod_pca = Mod_pca,Classifier_RF=Classifier_RF))
}
  
  
