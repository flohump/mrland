#' @title convertFAOPlantedForests2006
#' @description Fills the countries not covered by the questionnaire with 0 (no data).
#' @param x magpie object from readFAOPlantedForests2006
#' @return magpie object, all countries
#' @author Florian Humpenoeder
convertFAOPlantedForests2006 <- function(x) {
  return(toolCountryFill(x, fill = 0, verbosity = 2))
}
