#' @title calcPlantationRemovals
#' @description Industrial roundwood removals of timber plantations per ha of plantation estate: the removals of
#' 2012 by country (FAO working paper FP/48, source ForestNationalStatistics) over the FRA 2025 plantation forest
#' area of 2012. For the United States the area is that of the planted stands of the South, the estate the removals
#' of FP/48 refer to (18.9 Mha of forest land, US forest inventory FIA, evaluation group 2010). Countries without
#' removals or without that area have no value and carry no weight.
#' @return List of magpie objects with results on country level, weight, unit and description.
#' @author Florian Humpenoeder
#' @seealso [readForestNationalStatistics()]
#' @examples
#' \dontrun{
#' calcOutput("PlantationRemovals", round = 2)
#' }
calcPlantationRemovals <- function() {
  stated <- c(USA = 18.9)                                                                          # Mha
  removals <- setYears(collapseNames(readSource("ForestNationalStatistics", "FP48")), NULL) / 1e6   # Mm3 per yr
  area <- collapseNames(readSource("FRA2025", "forest_area")[, , "plantationForest"])
  area <- setYears(time_interpolate(area, 2012)[, 2012, ], NULL)
  area[names(stated), , ] <- stated
  x <- removals / area
  x[area == 0] <- 0
  return(list(x = x, weight = area * (x > 0) + 1e-10, min = 0, unit = "m3 per ha per yr",
              description = "Industrial roundwood removals of timber plantations per ha of plantation estate, 2012"))
}
