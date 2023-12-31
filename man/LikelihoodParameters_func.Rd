% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LikelihoodParameters_func.R
\name{LikelihoodParameters_func}
\alias{LikelihoodParameters_func}
\title{Function for calculating parameters \eqn{\mu_i}'s (mean vector) and \eqn{\Sigma_i}'s in the 
likelihood functions used for PLS-QDA}
\usage{
LikelihoodParameters_func(Train = "TrainP+", alpha0 = 73.58105)
}
\arguments{
\item{Train:}{Either 'TrainP+' (default), or 'TrainP-', indicating if training data including or
omiting PB pietrain is to be used.}

\item{alpha0:}{Deafault value is 73.58105, i.e. the free parameter \eqn{\alpha_0} used 
in calculation of \eqn{V(\theta)}.}
}
\value{
A list with the possible breedcombinations from input PBs. Named with 4 letter abbreviations.
}
\description{
Function for calculating parameters \eqn{\mu_i}'s (mean vector) and \eqn{\Sigma_i}'s in the 
likelihood functions used for PLS-QDA
}
\examples{
Default_alpha <- LikelihoodParameters_func()
Small_alpha <- LikelihoodParameters_func(alpha0=2)
Default_alpha$DDLW
Small_alpha$DDLW
}
\references{
Vinje,H.......
}
\author{
Hilde Vinje & Lars Erik Gangsei
}
