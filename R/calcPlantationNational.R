#' @title calcPlantationNational
#' @description Rotation and stand yield at the rotation of timber plantations from national statistics by
#' country and Koeppen-Geiger class. The stand yield is the mean annual increment times the rotation. Brazil is
#' the only country with values: eucalyptus in the tropical classes and pine in the temperate classes Cfa and Cfb,
#' with the mean annual increment of 2023 (33.7 and 30.8 m3 per ha per yr with bark, IBA Relatorio Anual 2024,
#' p. 43 and 44) and the rotation of the FAO 2006 planted forests study (Del Lungo et al., Table 6a, South
#' America: eucalyptus 7 to 15 yr, pines 21 to 31 yr, on 5-year steps 10 and 25 yr). Classes and countries without
#' a value get 0 and carry no weight, so that the countries with a value stand for their region.
#' @param indicator "rotation" (yr) or "yield" (m3 per ha)
#' @return List of magpie objects with results on country level, weight, unit and description.
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("PlantationNational", indicator = "yield", round = 1)
#' }
calcPlantationNational <- function(indicator = "rotation") {
  stopifnot(indicator %in% c("rotation", "yield"))
  clcl <- c("Af", "Am", "As", "Aw", "BSh", "BSk", "BWh", "BWk", "Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb",
            "Cwc", "Dfa", "Dfb", "Dfc", "Dfd", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd", "EF", "ET")
  tropical <- c("Af", "Am", "As", "Aw")
  temperate <- c("Cfa", "Cfb")
  area <- setYears(collapseNames(readSource("FRA2025", "forest_area")[, "y2020", "plantationForest"]), NULL)
  rotation <- new.magpie(getItems(area, 1), NULL, clcl, fill = 0)
  increment <- rotation
  rotation["BRA", , tropical] <- 10
  rotation["BRA", , temperate] <- 25
  increment["BRA", , tropical] <- 33.7
  increment["BRA", , temperate] <- 30.8
  x <- if (indicator == "yield") rotation * increment else rotation
  what <- c(rotation = "rotation", yield = "stand yield at the rotation")[[indicator]]
  return(list(x = x, weight = area * (x > 0) + 1e-10, min = 0,
              unit = c(rotation = "yr", yield = "m3 per ha")[[indicator]],
              description = paste("Plantation", what, "from national statistics by Koeppen class (0 = no value)")))
}
