#' @title convertMoFuSS
#' @description Countries without MoFuSS simulation get no harvest and no fNRB value.
#' @param x magpie object from readMoFuSS
#' @return magpie object with all countries
#' @author Florian Humpenoeder
#' @seealso [madrat::readSource()]
convertMoFuSS <- function(x) {
  return(toolCountryFill(x, fill = 0, verbosity = 2))
}
