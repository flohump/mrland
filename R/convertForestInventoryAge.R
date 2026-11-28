#' @title convertForestInventoryAge
#' @description Fills countries without a value with 0.
#' @param x magpie object from [readForestInventoryAge()]
#' @param subtype subtype of the source, not used
#' @return magpie object on the common country list
#' @author Florian Humpenoeder
convertForestInventoryAge <- function(x, subtype = "FIN") {
  return(toolCountryFill(x, fill = 0, verbosity = 2))
}
