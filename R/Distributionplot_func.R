#' A function for making score plot comparing PLS and PCA scores in Vinje et al.
#' 
#' @param image_path (optional). Path for saving figure as png-format. If NULL 
#' (default), windows() is called and figure is shown in new windows.
#' @param TrueBreed Vector of length n with True Breeds as four letter abbreviations.
#' @param SoftPreds data.frame with k (typically 3) elements with soft predictions
#' of the individuals defined in "TrueBreed"
#' @param BreedNames in full not one letter abbrivations
#' @param MethodNames names for the k methods (in full) represented in "SoftPreds"
#' @param dens_adj vector of length k passed to "density(...,adjust = dens_adj[k])"

#' @return A plot (or png file) with four panels. See Vinje et.al.... for further description.
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' Distributionplot_func(TrueBreed = rownames(RF_Sim_predictions$`TrainP+`),
#' SoftPreds = data.frame(I(RF_Sim_predictions$`TrainP+`)),
#' I(PLSR_Sim_predictions$`TrainP+`)),MethodNames = c("Random Forest","PLSR"))
#' @export

Distributionplot_func <- function(image_path = NULL,TrueBreed,SoftPreds,
                                  BreedNames = list(
                                    Others = c("DDDD","LLLL","PPPP","WWWW"),
                                    Hampshire = "HHHH"),
                                  MethodNames = c("Random Forest","Admixture","PLSR","PLS-QDA"),
                                  dens_adj = c(1,10,1,200),width = 17,height = 22)
{
  if(!is.null(image_path))
    {
    png(filename = image_path,
      width = width, height = height, units = "cm", pointsize = 12,
      res = 1200)}
  
  if(is.null(BreedNames)){BreedNames <- unique(unlist(strsplit(
    paste(TrueBreed,collapse=''),'')))}
  if(is.null(MethodNames)){MethodNames <- names(SoftPreds)}
  
  Real <- BreedToMat(Bvec = TrueBreed,Breeds = paste(unique(unlist(strsplit(
    paste(TrueBreed,collapse=''),''))),collapse = ''))
  nn <- length(BreedNames)
  mm <- length(MethodNames)
  
  layout(mat = rbind(1:(nn+1),matrix((nn+2):((mm+1)*(nn+1)),ncol=nn+1,
                             nrow=mm,byrow=FALSE)),
         heights = c(0.05,rep(0.95/(mm+1),mm+1)),
         widths = c(0.1,rep(0.9/nn,nn)))
  
  par(mar = rep(0,4))
  for(kk in 1:(nn+1))
  {plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    legend(x = "center",inset = 0,
           legend = c( " ", names(BreedNames))[kk], 
           horiz = TRUE,bty='n',cex=1.5)
  }
  
  for(kk in 0:nn)
  {
  for(rr in 1:mm)
  {
      if(kk==0)
      {
        par(mar = rep(0,4))
        plot(1, type = "n", axes=FALSE, xlab="", ylab="")
        text(x =1,y=1,adj=0.5,labels = MethodNames[rr], 
             cex=1.5,srt=90)
        par(mar = c(8,4,1,2)/4+0.1)
      }else{
        cidx <- is.element(colnames(SoftPreds[[rr]]),BreedNames[[kk]])
        for(dd in (0:4)/4)
        {
          Pvec <- SoftPreds[[rr]][,cidx]
          Pvec <- Pvec[Real[,cidx]==dd]
          #if(rr<(mm+1)){Pvec <- SoftPreds[[kk]][Real[,rr]==dd,rr]
          #}else{Pvec <- SoftPreds[[kk]][Real==dd]}
          if(dd ==0){
            if(length(Pvec)>1){
            plot(density(Pvec,adjust = dens_adj[rr],n = 1024,
                         from = 0,to = 0.25),xlim = c(0,1),lwd = 2,
                 main = '',#c('Random Forest','PLSR','PLS-DA')[kk],
                 xlab = ifelse(rr==mm,expression(Delta),''),
                 ylim = c(0,50),axes=FALSE)}
            box()
            if(rr==mm){axis(1,at = (0:4)/4)}
            if(kk==1){axis(2,at = seq(0,50,by=2))}
            abline(v=(0:4)/4,lwd=2,lty=1,
                   col = c('black','red','blue','green','magenta'))
          }else{
            if(length(Pvec)>1){
              points(density(Pvec,adjust = dens_adj[rr],n = 2048),type = 'l',lwd = 2,
                     col = 'black')
              points(density(Pvec,adjust = dens_adj[rr],n = 2048),
                     lwd = 2,lty = dd*4,type = 'l',
                   col = c('red','blue','green','magenta')[dd*4])
            }
          }
        }
      }
    }
  }
  
if(!is.null(image_path)){dev.off()}
}
  
  
