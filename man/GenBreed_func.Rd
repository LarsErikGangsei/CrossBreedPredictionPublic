% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GenBreed_func.R
\name{GenBreed_func}
\alias{GenBreed_func}
\title{Function for generalising breedcombinations usee in Vinje et.al.}
\usage{
GenBreed_func(bb, KeepBreed = "P")
}
\arguments{
\item{bb:}{Breed combination abbreviation. string of 4 characters, i.e. 
'DDLL' etc.}

\item{KeepBreed:}{(vector) with one letter abbreviations for breeds to 
keep in output.}
}
\value{
Four letter abbreviation, where breeds not part of "KeepBreed" are 
simplified to in order "X", "Y", "L" and "K".
}
\description{
Function for generalising breedcombinations usee in Vinje et.al.
}
\examples{
GenBreed_func('DDLL')
GenBreed_func('DDLL',KeepBreed = 'D')

}
\references{
Vinje,H.......
}
\author{
Hilde Vinje & Lars Erik Gangsei
}
