#' @title calcForestManagedShare
#' @description Share of the forest area that is available for wood supply, by country. Where State of Europe's
#' Forests 2020 reports the forest available for wood supply in 2020, the share is that area over the forest area of
#' 2020 (annex Table 4). Two countries have a value from other statistics: the United States (timberland, 208 of
#' 310 Mha of forest land in 2017, Oswalt et al. 2019, GTR WO-97) and Japan (FRA 2025 reports no production
#' designation but all forest under a management plan). Elsewhere it is the FRA 2025 share of the forest area with
#' a production designation in 2020. A country without a value gets 0 and no weight. Weight: FRA 2025 forest area
#' in 2020 of the countries with a value.
#' @return List of magpie objects with results on country level, weight, unit and description.
#' @author Florian Humpenoeder
#' @seealso [readForestNationalStatistics()]
#' @examples
#' \dontrun{
#' calcOutput("ForestManagedShare", round = 2)
#' }
calcForestManagedShare <- function() {
  statistics <- c(USA = 208 / 310, JPN = 1)
  area <- setYears(collapseNames(readSource("FRA2025", "forest_area")[, "y2020", "forestArea"]), NULL)
  designated <- setYears(collapseNames(readSource("FRA2025", "management")[, "y2020", "prod"]), NULL)
  countries <- getItems(area, 1)
  soef <- collapseNames(readSource("ForestNationalStatistics", "ForestEurope2020")[countries, 2020, ])
  inventory <- soef[, , "faws"] / (soef[, , "forest_area"] + 1e-10)
  inventory[names(statistics), , ] <- statistics

  share <- designated[countries, , ] / area
  share[is.na(share) | area == 0] <- 0
  share[share > 1] <- 1
  share[inventory > 0] <- inventory[inventory > 0]
  hasValue <- share > 0
  return(list(x = share, weight = area * hasValue + 1e-10, min = 0, max = 1, unit = "1",
              description = paste("Share of the forest area available for wood supply (forest inventories), else",
                                  "share with a production designation in 2020 (FRA 2025)")))
}
