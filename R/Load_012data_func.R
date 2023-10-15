#' A function for loading data in 012 format. User will need to have access to
#' a folder with files "PB_data.RData", "Real_data.RData" and "Simdata.RData".

#' @return A list with three data frames containing DNA data from n different individuals: 
#' "PB_data" (n = 4014), "Simdata" (n = 7000) and "Real_data" (n = 1013), all with elements:\cr
#' - Breed (4 letter abbrivation for real breed, simulated breed. "Unknown for Real_data) 
#' - ID (or "AnimaliaDNA" for real data). Identification of the different individuals. 
#' - XX: Matrix of size (n x 23070) containing 0-1-2 coding for the 23070 SNp's. 
#' The column names are the SNP-names. Rownames as breed combinations for Simdata and
#' PB-data, and "AnimaliaDNA" for Real_data 
#' 
#' @author  Lars Erik Gangsei & Hilde Vinje
#' @references Vinje,H.......
#' 
#' @examples
#' Load_012data_func()


#' @export
## Set path to data to be included
Load_012data_func <- function()
{
if(Sys.getenv('USERNAME')=='hildvi'){
  Data_path <- file.path('C:','Users',Sys.getenv('USERNAME'),
                         'Norwegian University of Life Sciences',
                         'Lars Erik Gangsei - DNA_gris')
}else{
  if(is.element(Sys.getenv('USERNAME'),c('aoleg','laga')))
  {
    Data_path <- file.path('C:','Users',Sys.getenv('USERNAME'),
                           'Onedrive - Norwegian University of Life Sciences',
                           'Z-Drive','NMBU','DNA_gris')
  }else{
    Data_path <- choose.dir('',caption = 'Choose folder "DNA_gris" containing 
                            files "PB_data.RData", "Real_data.RData" and "Simdata.RData"')
  }
}


load(file = file.path(Data_path,'PB_data.RData'))
PB_data$XX[is.na(PB_data$XX)] <- 0
rownames(PB_data$XX) <- PB_data$Breed 

load(file = file.path(Data_path,'Real_data.RData'))
Real_data$XX[is.na(Real_data$XX)] <- 0
rownames(Real_data$XX) <- Real_data$AnimaliaDNA 

load(file.path(Data_path,'Simdata.RData'))
Simdata$XX[is.na(Simdata$XX)] <- 0
rownames(Simdata$XX) <- Simdata$Breed

return(list(PB_data=PB_data, Real_data = Real_data, Simdata=Simdata))
}
  
  
