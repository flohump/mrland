#' @title calcPlantedHarvestAge
#' @description
#' Harvest age of planted forest by pool and Koeppen-Geiger climate class from national sources, in years
#' (0 = no value). Timber plantations (item "plantation"): the rotation of [calcPlantationNational()].
#' Other planted forest (item "other_planted", FRA planted forest that is not plantation forest): Germany, Finland
#' and Sweden 120 years in the cool temperate, boreal and polar classes, the harvest age of a uniform estate with
#' the mean stand age and the share older than 100 years of their inventories (Bundeswaldinventur 2012; Korhonen
#' et al. 2024; SLU 2021 to 2025); Japan 80 years, also in the warm temperate classes, a lower bound from the forest
#' registers of 2007 to 2022 (Forestry Agency of Japan). Countries without a value carry no weight.
#' Weight: area of the pool (plantations 2020, other planted forest 1995; FRA 2025).
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Florian Humpenoeder
#' @seealso [calcPlantationAgeClass()]
#' @examples
#' \dontrun{
#' calcOutput("PlantedHarvestAge", aggregate = TRUE, round = 0)
#' }
calcPlantedHarvestAge <- function() {
  plantation <- calcOutput("PlantationNational", indicator = "rotation", aggregate = FALSE, supplementary = TRUE)
  clcl <- getItems(plantation$x, 3)
  warm <- c("Cfa", "Csa", "Cwa")
  cool <- setdiff(grep("^[CDE]", clcl, value = TRUE), warm)

  fraArea1995 <- setYears(time_interpolate(readSource("FRA2025", "forest_area"), 1995), NULL)
  otherPlanted <- collapseNames(fraArea1995[, , "plantedForest"]) - collapseNames(fraArea1995[, , "plantationForest"])
  otherPlanted[otherPlanted < 0] <- 0
  x <- new.magpie(getItems(otherPlanted, 1), NULL, clcl, fill = 0)
  x[c("DEU", "FIN", "SWE"), , cool] <- 120
  x["JPN", , c(warm, cool)] <- 80
  weight <- otherPlanted * (x > 0) + 1e-10

  bind <- function(plant, other) {
    return(mbind(add_dimension(plant, dim = 3.1, add = "pool", nm = "plantation"),
                 add_dimension(other, dim = 3.1, add = "pool", nm = "other_planted")))
  }
  return(list(x = bind(plantation$x, x),
              weight = bind(plantation$weight, weight),
              unit = "yr",
              min = 0,
              description = paste("Harvest age of timber plantations and of other planted forest by Koeppen-Geiger",
                                  "class from national sources (0 = no value)")))
}
