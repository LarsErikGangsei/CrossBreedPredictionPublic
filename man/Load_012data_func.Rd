% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Load_012data_func.R
\name{Load_012data_func}
\alias{Load_012data_func}
\title{A function for loading data in 012 format. User will need to have access to
a folder with files "PB_data.RData", "Real_data.RData" and "Simdata.RData".}
\usage{
Load_012data_func()
}
\value{
A list with three data frames containing DNA data from n different individuals: 
"PB_data" (n = 4014), "Simdata" (n = 7000) and "Real_data" (n = 1013), all with elements:\cr
- Breed (4 letter abbrivation for real breed, simulated breed. "Unknown for Real_data) 
- ID (or "AnimaliaDNA" for real data). Identification of the different individuals. 
- XX: Matrix of size (n x 23070) containing 0-1-2 coding for the 23070 SNp's. 
The column names are the SNP-names. Rownames as breed combinations for Simdata and
PB-data, and "AnimaliaDNA" for Real_data
}
\description{
A function for loading data in 012 format. User will need to have access to
a folder with files "PB_data.RData", "Real_data.RData" and "Simdata.RData".
}
\examples{
Load_012data_func()
}
\references{
Vinje,H.......
}
\author{
Hilde Vinje & Lars Erik Gangsei
}
