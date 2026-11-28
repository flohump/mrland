#' @title calcNaturalForestShare
#' @description Share of natural forest in the industrial roundwood production of a country, from the FAOSTAT
#' production by species group (mean 2015 to 2019) under one assumption per country (mapping
#' naturalForestRoundwoodRule.csv). The countries of Latin America and the Caribbean have a rule. "total": all
#' industrial roundwood is natural-forest wood. "none": all of it is plantation wood (Chile and Uruguay, pine and
#' eucalyptus). "nonconiferous": the non-coniferous part (Argentina, coniferous wood counts as plantation pine).
#' "sawlogs_nonconiferous": the non-coniferous sawlogs and veneer logs (Brazil, coniferous wood and non-coniferous
#' pulpwood count as plantation wood). Countries without a rule get 0 and carry no weight. Weight: FAO industrial
#' roundwood production, mean 2015 to 2019.
#' @return List of magpie objects with results on country level, weight, unit and description.
#' @author Florian Humpenoeder
#' @seealso [calcSelectiveLogging()]
#' @examples
#' \dontrun{
#' calcOutput("NaturalForestShare", round = 3)
#' }
calcNaturalForestShare <- function() {
  rule <- toolGetMapping("naturalForestRoundwoodRule.csv", type = "regional", where = "mrland")
  items <- c(irw = "1865|Industrial roundwood", nonconiferous = "1867|Industrial roundwood, non-coniferous",
             sawlogs = "1604|Sawlogs and veneer logs, non-coniferous")
  fao <- readSource("FAO_online", "ForestProdTrade2026")
  wood <- dimSums(collapseNames(fao[, 2015:2019, "production"][, , items]), dim = 2) / 5
  getNames(wood) <- names(items)
  irw <- collapseNames(wood[, , "irw"])
  stopifnot(rule$Rule %in% c("total", "none", "nonconiferous", "sawlogs_nonconiferous"),
            !anyDuplicated(rule$CountryCode))
  natural <- 0 * irw
  natural[rule$CountryCode[rule$Rule == "total"], , ] <- 1
  part <- rule$CountryCode[rule$Rule == "nonconiferous"]
  natural[part, , ] <- collapseNames(wood[part, , "nonconiferous"]) / irw[part, , ]
  part <- rule$CountryCode[rule$Rule == "sawlogs_nonconiferous"]
  natural[part, , ] <- collapseNames(wood[part, , "sawlogs"]) / irw[part, , ]
  given <- 0 * irw
  given[rule$CountryCode, , ] <- 1
  stopifnot(natural <= 1)
  return(list(x = natural, weight = irw * given + 1e-10, min = 0, max = 1, unit = "1",
              description = "Share of natural forest in industrial roundwood production (0 where not given)"))
}
