#' Function used in app. Returns colorblends based on CB input as four letter 
#' abbrivations
#' 
#' @param xx vector of length n with crossbreeds defined by four letter abbrivations.
#' @param rgbw four letter string defining which purebreeds that should be 
#' represented by colors red, green, blue and white respectively.
#' @param lim_white scalar. Defines the amount of "grey" in the white color
#' @param blackBreed One letter abbreviation for which all breedcombinations
#' containing this breed will get black color.

#' @return A vector of length n containing color definitions as returned by rgb()
#' 
#' @author Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @export

BreedToColor <- function(xx,rgbw = 'DLWH',lim_white=0.9,blackBreed = 'P')
{
  BredMat <- BreedToMat(xx,rgbw)[,order(order(strsplit(rgbw,split='')[[1]]))]
  if(length(xx)==1){BredMat <- t(as.matrix(BredMat))}
  #print(BredMat[1,])
  RGBMat <- (BredMat[,1:3]*matrix(1-BredMat[,4],length(xx),3,byrow=FALSE)+
               matrix(BredMat[,4],length(xx),3,byrow=FALSE))
  RGBMat[RGBMat>1] <- 1
  RGBMat[RGBMat<0] <- 0
  RGBMat[rowSums(RGBMat)==3,] <- lim_white
  RGBMat[grep(blackBreed,xx),] <- 0
  return(apply(RGBMat,1,function(x){rgb(x[1],x[2],x[3])}))
}



