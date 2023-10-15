#' Function for reading DNA data (GeneSeek 50K (Illumina) SNP chip) from files (.ped or .txt)

#' @param ped: A data frame of  size n x 2m, where n is the number of rows (individuals) 
#' in the ped file, and m is the number of SNP's in the map file (23 070 in the PigBreedPrediction_map)
#' @param na_val: Value to put in SNP's with missing data. Default is NA, might be set to 0.
#' @import dplyr

#' @return matrix size n x m, where n is the number of rows (individuals) 
#' in the ped file, and m is the number of SNP's in the map file 
#' (23 070 in the PigBreedPrediction_map), use 
#' head(PigBreedPrediction_map) to view first entries of map file. Column names 
#' according to SNP-names and Rownames according to ID
#' 
#' @author  Hilde Brustad, Hilde Vinje & Lars Erik Gangsei
#' @references Vinje,H.......
#' 
#' @examples
#' ped <- read_ped_data('data/Ped_example.txt',ID_column = 'ID',nrows = -1)
#' ped012 <- pedTo012_func(ped,na_val = 0)



#' @export
pedTo012_func <- function(ped,na_val = NA)
  {
  message('The process is ongoing')
  pedlist <- lapply(split(t(as.matrix(ped)),
                   f = rep(PigBreedPrediction_map$snp,
                           each=2))[PigBreedPrediction_map$snp],
                   function(x){matrix(x,2,length(x)/2,byrow=FALSE)})
  
  SumMat <- 2-sapply(mapply("==",pedlist,split(PigBreedPrediction_map$dom,
                            1:dim(PigBreedPrediction_map)[1]),
                           SIMPLIFY=FALSE),colSums)
  
  SumMat[sapply(lapply(lapply(pedlist,is.element,set = c('A','C','G','T')),
                       function(x){matrix(x,2,length(x)/2,
                            byrow=FALSE)}),colSums)<2] <- na_val
  rownames(SumMat) <- rownames(ped)
  colnames(SumMat) <- PigBreedPrediction_map$snp
  return(SumMat)
}
  
  
  
