#' Function for reading DNA data (GeneSeek 50K (Illumina) SNP chip) from files (.ped or .txt)

#' @param ped_path Path to "ped" file. The ped file must have column names in line 
#' with snp names in PigBreedPrediction_map
#' @param ID_column (optional) Column names in ped file used as ID. 
#' @param nrows (optional) pased to "read.table()". integer: the maximum number of rows to 
#' read in. Negative and other invalid values are ignored. If -1 (default) all rows
#' are read in.
#' @import dplyr

#' @return A data frame of  size n x 2m, where n is the number of rows (individuals) 
#' in the ped file, and m is the number of SNP's in the map file 
#' (23 070 in the PigBreedPrediction_map), use 
#' head(PigBreedPrediction_map) to view first entries of map file
#' 
#' @author  Hilde Brustad, Hilde Vinje & Lars Erik Gangsei
#' @references Vinje,H.......
#' 
#' @examples
#' ped <- read_ped_data('data/Ped_example.txt',ID_column = 'ID',nrows = -1)
#' ped[,1:10]



#' @export

## Functions from Hilde Brustad
read_ped_data <- function(ped_path,ID_column = NULL,nrows=-1)
  {
  message('Reading data from txt file is time consuming')
  ped <- read.table(ped_path,header = TRUE,
                    check.names = FALSE,nrows=nrows)
  names(ped) <- gsub('_a|_b','',names(ped))
  names(ped)[substr(names(ped),1,1) =='X'] <- substr(names(ped),2,
                                            100)[substr(names(ped),1,1) =='X']
  names(ped) <- gsub('_a|_b','',names(ped))
  names(ped)[substr(names(ped),1,1) =='X'] <- substr(names(ped),2,
                                          100)[substr(names(ped),1,1) =='X']
  if(!is.null(ID_column)){row.names(ped) <- ped[,ID_column]}
  ped <- ped[,-which(!is.element(names(ped),PigBreedPrediction_map$snp))]
  #names(ped) <- paste(names(ped),rep(c('','.1'),dim(ped)[2]/2),sep='')
  #ped <- ped[,paste(rep(PigBreedPrediction_map$snp,each=2),
  #            rep(c('','.1'),dim(PigBreedPrediction_map)[1]/2),sep='')]
  ped <- ped[,rep(PigBreedPrediction_map$snp,each=2)]
  names(ped) <- rep(PigBreedPrediction_map$snp,each=2)
  return(ped)
}
  
  
  
