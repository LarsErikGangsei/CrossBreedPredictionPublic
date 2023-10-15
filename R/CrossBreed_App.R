#' App to show CrossBreeds
#' @export
CrossBreed_App <- function() 
{
  shiny::runApp(appDir = system.file('appdir', package='CrossBreedPrediction'))
}