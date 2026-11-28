#' @title calcForestAgeInventory
#' @description Age structure of the forest by country in 1995 as shares of the country's forest in 15 ten-year
#' age classes (class15: 140 yr and older). Sources: national inventories by stand age for USA, Canada, China,
#' Japan (natural forest), Germany, Finland, Sweden and Norway ([readForestInventoryAge()]); for other European
#' countries the even-aged forest by development phase of State of Europe's Forests 2020, annex Table 11
#' ([readForestNationalStatistics()]), with the phase limits at 20 and 90 percent of a 100-yr rotation.
#' Bins are moved back to 1995 by the years since 1995; the area that regrew since then is placed in the classes
#' from 60 yr on (Canada 80 yr), in proportion to the moved bins.
#' @return magpie object, country x 1995 x age class: share of the country's forest in each class, summing to the
#' share with a target (0 for countries without data). Not meant for aggregation.
#' @author Florian Humpenoeder
#' @seealso [calcForestIncrementRel()]
#' @examples
#' \dontrun{
#' calcOutput("ForestAgeInventory", aggregate = FALSE)
#' }
calcForestAgeInventory <- function() {
  rotation <- 100                                     # recommended rotation of the SoEF development phases (yr)
  harvestAge <- c(CAN = 80, other = 60)               # lower age of the classes that take the regrown land (yr)
  classLower <- seq(0, 140, 10)
  classUpper <- c(seq(10, 140, 10), Inf)

  # class shares in 1995 of bins (lower, upper in yr, area) observed in year; an open bin is spread up to class15
  classShares1995 <- function(lower, upper, area, year, iso) {
    upperClosed <- ifelse(is.finite(upper), upper, pmax(lower + 10, 150 + year - 1995))
    young <- area * pmin(1, pmax(0, (year - 1995 - lower) / (upperClosed - lower)))     # grown up since 1995
    lower1995 <- pmax(lower - (year - 1995), 0)
    upper1995 <- upperClosed - (year - 1995)
    keep <- upper1995 > lower1995
    classArea <- vapply(seq_along(classLower), function(ageClass) {
      overlap <- pmax(0, pmin(upper1995, classUpper[ageClass]) - pmax(lower1995, classLower[ageClass]))
      sum(((area - young) * overlap / (upper1995 - lower1995))[keep])
    }, 0)
    regrowAge <- if (iso == "CAN") harvestAge[["CAN"]] else harvestAge[["other"]]
    regrowWeight <- classArea * (classLower >= regrowAge)
    regrowWeight <- if (sum(regrowWeight) > 0) regrowWeight / sum(regrowWeight) else c(rep(0, 14), 1)
    classArea <- classArea + sum(young) * regrowWeight
    return(classArea / sum(classArea))
  }
  # bins of a reader object (one country, one year): bin "a<from>_<to>", lower edges to whole decades
  bins <- function(x, variable) {
    binArea <- collapseNames(x[, , variable])
    binLower <- as.numeric(sub("^a([0-9]+)_.*$", "\\1", getNames(binArea)))
    binUpper <- as.numeric(sub("^a[0-9]+_", "", getNames(binArea)))
    return(list(iso = getItems(x, 1), year = getYears(x, as.integer = TRUE), covered = 1,
                lower = floor(binLower / 10) * 10,
                upper = ifelse(is.finite(binUpper), ceiling(binUpper / 10) * 10, Inf),
                area = as.vector(binArea)))
  }

  inventory <- function(subtype) readSource("ForestInventoryAge", subtype, convert = FALSE)
  targets <- lapply(c("USA", "CAN", "DEU", "FIN", "SWE", "NOR"), function(subtype) {
    bins(inventory(subtype), "area")
  })
  targets <- c(targets, list(bins(inventory("JPNhandbook"), "natural_area")))
  china <- toolChinaAgeGroups(inventory("CHN"))
  targets <- c(targets, list(list(iso = "CHN", year = 2016, covered = 1, lower = china$lower, upper = china$upper,
                                  area = china$area)))

  soef <- readSource("ForestNationalStatistics", "ForestEurope2020")[, 2015, ]
  phases <- c("regeneration", "intermediate", "mature")
  for (iso in setdiff(getItems(soef, 1), vapply(targets, function(target) target$iso, ""))) {
    phaseArea <- as.vector(soef[iso, , phases])
    forest <- as.vector(soef[iso, , "forest_area"])
    if (anyNA(phaseArea) || is.na(forest) || sum(phaseArea) <= 0 || forest <= 0) {
      next
    }
    targets <- c(targets, list(list(iso = iso, year = 2015, covered = min(1, sum(phaseArea) / forest),
                                    lower = c(0, 0.2, 0.9) * rotation, upper = c(0.2, 0.9, Inf) * rotation,
                                    area = phaseArea)))
  }

  isos <- vapply(targets, function(target) target$iso, "")
  stopifnot(!anyDuplicated(isos))
  x <- new.magpie(isos, 1995, paste0("class", 1:15), fill = 0)
  for (target in targets) {
    x[target$iso, , ] <- target$covered * classShares1995(target$lower, target$upper, target$area, target$year,
                                                          target$iso)
  }
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  return(list(x = x,
              weight = NULL,
              unit = "1",
              min = 0,
              max = 1,
              description = paste("Share of the country's forest in 15 age classes in 1995 from national forest",
                                  "inventories and the SoEF 2020 development phases")))
}
