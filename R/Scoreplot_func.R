#' A function for making score plot comparing PLS and PCA scores in Vinje et al.
#' 
#' @param image_path (optional). Path for saving figure as png-format. If NULL 
#' (default), windos() is called and figure is shown in new windows.
#' 

#' @return A plot (or png file) with four panels. See Vinje et.al.... for further description.
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' Scoreplot_func()
#' @export

Scoreplot_func <- function(image_path = NULL)
{
cvec <- sapply(split(class.ind(Mod_pls$`TrainP+`$Breed) %*% t(matrix(c(1,0,1,1,0,
                                               1,0,0,0,1,
                                               0,0,1,0,1),3,5,byrow=TRUE)),
                     f = 1:length(Mod_pls$`TrainP+`$Breed)),
               function(x) rgb(x[1],x[2],x[3]))

pchvec <- as.numeric(as.factor(Mod_pls$`TrainP+`$Breed))+20

if(!is.null(image_path))
  {png(filename = image_path,
      width = 12, height = 12, units = "cm", pointsize = 12,
      res = 1200)}else{windows()}

layout(mat = matrix(c(1,1,2:5),ncol=2,nrow=3,byrow=TRUE),
       heights = c(0.1,0.45,0.45))

par(mar = rep(0,4))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "center",inset = 0,
       legend = c("Pietrain", "Duroc", "Landrace","Large white", "Hampshire"), 
       pt.bg=c(rgb(1,0,0),rgb(1,1,0),rgb(1,0,1),rgb(0,1,1),rgb(0,0,0)), 
       pt.cex = 2, pch = c(4,1,3,5,2)+20,
       pt.lwd=0, cex=1.2, horiz = TRUE)
par(mar = c(5,4,4,2)/2+0.1)

# Multiplied with -1 for scores in order to illustrate similarity for scores 
# 1 and 3 in PCA and PLS.
plot(-Mod_pca$`TrainP+`$scores[,1],Mod_pca$`TrainP+`$scores[,2],bg=cvec,
     main = 'PCA - Scores 1 and 2',xlab = 'Score 1',ylab = 'Score 2',cex=1.5,
     pch = pchvec)
plot(-Mod_pca$`TrainP+`$scores[,3],-Mod_pca$`TrainP+`$scores[,4],bg=cvec,
     main = 'PCA - Scores 3 and 4',xlab = 'Score 3',ylab = 'Score 4',cex=1.5,
     pch = pchvec)


plot(Mod_pls$`TrainP+`$scores[,1],Mod_pls$`TrainP+`$scores[,2],bg=cvec,
     main = 'PLS - Scores 1 and 2',xlab = 'Score 1',ylab = 'Score 2',cex=1.5,
     pch = pchvec)
plot(Mod_pls$`TrainP+`$scores[,3],Mod_pls$`TrainP+`$scores[,4],bg=cvec,
     main = 'PLS - Scores 3 and 4',xlab = 'Score 3',ylab = 'Score 4',cex=1.5,
     pch = pchvec)

if(!is.null(image_path))
{dev.off()}
}
  
  
