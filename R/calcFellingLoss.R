#' @title calcFellingLoss
#' @description Share of the felled stem volume over bark that stays in the forest, by country, for industrial
#' roundwood ("wood"); 0 for wood fuel ("woodfuel"). The share is 1 minus the roundwood removals under bark over
#' the fellings over bark, with the removals brought over bark by a bark ratio of 0.87 (State of Europe's Forests
#' 2020). Member states of the EU-28 get the ratio of the EU-28 as a whole (Table 3.2-2, mean of 2013 to 2017). The
#' other countries that report fellings and removals for 2015 get their pooled ratio. Two countries have a value
#' from the literature: the United States (logging residue over fellings of growing stock, Oswalt et al. 2019,
#' Forest Resources of the United States 2017, GTR WO-97, Table 41) and Canada (merchantable waste of the harvested
#' volume on the Crown land of British Columbia 2004 to 2008, Forest Practices Board 2010, FPB/IRC/170). A country
#' without a value counts as 0 (no loss) and keeps its weight. Weight: industrial roundwood production (FAOSTAT
#' forestry, mean 2013 to 2017).
#' @return List of magpie objects with results on country level, weight, unit and description.
#' @author Florian Humpenoeder
#' @seealso [readForestNationalStatistics()]
#' @examples
#' \dontrun{
#' calcOutput("FellingLoss", round = 3)
#' }
calcFellingLoss <- function() {
  barkRatio <- 0.87
  literature <- c(USA = 1126441 / 12776206, CAN = 0.043)
  soef <- setYears(readSource("ForestNationalStatistics", "ForestEurope2020")[, 2015, ], NULL)
  member <- as.vector(soef[, , "fellings_eu28"] > 0)
  other <- !member & as.vector(soef[, , "fellings"] > 0 & soef[, , "removals"] > 0)
  loss <- new.magpie(getItems(soef, 1), NULL, NULL, fill = 0)
  loss[member, , ] <- 1 - soef[member, , "removals_eu28"] / soef[member, , "fellings_eu28"] / barkRatio
  loss[other, , ] <- 1 - sum(soef[other, , "removals"]) / sum(soef[other, , "fellings"]) / barkRatio
  loss[names(literature), , ] <- literature

  fao <- readSource("FAO_online", "ForestProdTrade2026")
  item <- grep("\\|Industrial roundwood$", getNames(fao, dim = 1), value = TRUE)
  production <- dimSums(fao[, 2013:2017, paste0(item, ".production")], dim = c(2, 3)) / 5
  production <- production[getItems(loss, 1), , ]
  x <- mbind(setNames(loss, "wood"), setNames(0 * loss, "woodfuel"))
  weight <- mbind(setNames(production, "wood"), setNames(production, "woodfuel"))
  return(list(x = x, weight = weight + 1e-10, min = 0, max = 1, unit = "1",
              description = paste("Share of the felled stem volume over bark left in the forest, industrial",
                                  "roundwood (wood); wood fuel 0")))
}
