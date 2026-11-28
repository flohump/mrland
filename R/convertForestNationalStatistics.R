#' @title convertForestNationalStatistics
#' @description Fills countries and years without a value with 0. Subtype "ForestEurope2020": the roundwood and
#' the fellings of the EU-28 as a whole are given to each member state as `removals_eu28` and `fellings_eu28`.
#' @param x magpie object from [readForestNationalStatistics()]
#' @param subtype "ForestEurope2020" or "FP48"
#' @return magpie object on the common country list
#' @author Florian Humpenoeder
convertForestNationalStatistics <- function(x, subtype = "ForestEurope2020") {
  if (subtype == "ForestEurope2020") {
    members <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL",
                 "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
    eu28 <- x["EU28", , ]
    x <- add_columns(x["EU28", , , invert = TRUE], c("removals_eu28", "fellings_eu28"), dim = 3.1, fill = NA)
    for (variable in c("removals", "fellings")) {
      for (iso in members) {
        x[iso, , paste0(variable, "_eu28")] <- as.vector(eu28[, , variable])
      }
    }
  }
  x[is.na(x)] <- 0
  return(toolCountryFill(x, fill = 0, verbosity = 2))
}
