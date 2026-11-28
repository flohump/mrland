#' @title calcSelectiveLogging
#' @description Selective logging in natural forest by country: log volume extracted per logged ha (`extract`, m3
#' per ha) and carbon lost per m3 extracted (`cfac`, tC per m3). Brazil is the only country with values: 15 m3 per
#' ha (log volume of the Imazon mill surveys of 1998 and 2004, Lentini et al. 2005, over the logged area of Asner
#' et al. 2005) and 1.36 tC per m3 (Pearson et al. 2017, Carbon Balance and Management 12:3, Table 2). Countries
#' without a value get 0 and carry no weight. Weight: natural-forest industrial roundwood production
#' ([calcNaturalForestShare()]), so a country with logging values needs a natural-forest share there.
#' @return List of magpie objects with results on country level, weight, unit and description.
#' @author Florian Humpenoeder
#' @seealso [calcNaturalForestShare()]
#' @examples
#' \dontrun{
#' calcOutput("SelectiveLogging", round = 2)
#' }
calcSelectiveLogging <- function() {
  share <- calcOutput("NaturalForestShare", aggregate = FALSE, supplementary = TRUE)
  wood <- share$x * share$weight
  x <- new.magpie(getItems(wood, 1), NULL, c("extract", "cfac"), fill = 0)
  x["BRA", , "extract"] <- 15
  x["BRA", , "cfac"] <- 1.36
  return(list(x = x, weight = wood * (x > 0) + 1e-10, min = 0, unit = "m3 per ha (extract), tC per m3 (cfac)",
              description = "Selective logging: extracted volume per logged ha and carbon lost per m3 extracted"))
}
