#' @title calcPlantedForest
#' @description
#' Calculates the share of plantations in planted forest
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link[mrfaocore]{calcFAOmassbalance_pre}}
#' @examples
#' \dontrun{
#' calcOutput("PlantedForest")
#' }
#' @importFrom magclass getNames<- as.magpie time_interpolate
#' @export

calcPlantedForest <- function() {
  ## Read land area FRA source
  forestArea <- readSource("FRA2025", "forest_area")
  ## MAgPIE base year 1995, interpolated from the FRA years 1990 and 2000
  forestArea <- time_interpolate(forestArea, 1995)
  plantedShare <- setNames(round(forestArea[, , "plantationForest"], 3) / round(forestArea[, , "plantedForest"], 3),
                           NULL)
  plantedShare[is.na(plantedShare)] <- 0
  out <- setYears(plantedShare, NULL)

  ## Weight
  weight <- setYears(setNames(round(forestArea[, , "plantedForest"], 3), NULL), NULL)

  return(list(x = out,
              weight = weight,
              min = 0,
              unit = "share",
              description = "Calculates the share of plantation forest in planted forest"))

}
