#' @title calcPlantationAreaFRA
#' @description
#' Plantation forest area 1995 to 2025 in 5-year steps (FRA 2025, interpolated between the FRA years).
#' @return List of magpie objects with results on country level, weight, unit and description.
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("PlantationAreaFRA", aggregate = TRUE)
#' }
calcPlantationAreaFRA <- function() {
  fraArea <- collapseNames(readSource("FRA2025", "forest_area")[, , "plantationForest"])
  out <- time_interpolate(fraArea, seq(1995, 2025, 5))
  return(list(x = out,
              weight = NULL,
              unit = "Mha",
              description = "Plantation forest area, FRA 2025, interpolated to 5-year steps"))
}
