#' Genetic map used in the PigBreedPrediction project.
#' based on the map for a GeneSeek 50K (Illumina) SNP chip
#'
#'
#' @docType data
#'
#' @usage data(PigBreedPrediction_map)
#'
#' @format Data frame of size 23070 x 5, with the 5 columns:\cr
#' - chrm: Chromosome number, integer 1-18\cr
#' - snp: SNP name, character\cr
#' - unknown: integer 0 (not in use)\cr
#' - bp: base position of SNP at chromosome\cr
#' - dom: dominent base at SNP in the PB animals used in the PigBreedPrediction project\cr
#'
#' @keywords datasets, genetic map
#'
#' @references Vinje et.al. 2023
#'
#'
#' @examples
#' head(PigBreedPrediction_map)
#' 
"PigBreedPrediction_map"