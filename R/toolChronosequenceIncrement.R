#' @title toolChronosequenceIncrement
#' @description Relative net increment by stand age class for an inventory that reports area and growing stock
#' by age group but no increment by age (China, [readForestInventoryAge()] subtype "CHN"). It is derived, not
#' measured.
#'
#' The stock per ha of the five age groups is placed at the mean ages of the groups (the mid of each age range; the
#' over-mature group at 1.5 times its lower limit), with the age limits averaged over the forest types by area. The
#' line through these points and the origin gives the net stock change of a stand by age. A loss rate proportional
#' to the stock is added so that the area-weighted gross increment equals the increment the inventory reports for
#' the whole forest, and the mortality rate (mortality over the mean standing stock of two inventories) is
#' subtracted again. The area of an age group is spread evenly over its age range.
#' @param x magpie object of [readForestInventoryAge()], subtype "CHN"
#' @return magpie object, country x (variable . bin): `area` (Mha) and `increment_rel` (net of mortality, 1 per yr)
#' for 20-year classes up to 140 years
#' @author Florian Humpenoeder
toolChronosequenceIncrement <- function(x) {
  ages <- toolChinaAgeGroups(x)
  area <- setNames(ages$area, ages$group)
  stockPerHa <- ages$stock * 1000 / area                                    # Gm3 over Mha
  lower <- ages$lower
  upper <- ages$upper
  overMature <- 1.5 * lower[5]
  nodes <- c(0, (lower[1:4] + upper[1:4]) / 2, overMature)
  stockAt <- function(age) {                                                # last slope continued beyond the last node
    segment <- pmin(pmax(findInterval(age, nodes, left.open = TRUE) + 1, 2), length(nodes))
    nodeStock <- c(0, stockPerHa)
    nodeStock[segment - 1] + (nodeStock[segment] - nodeStock[segment - 1]) * (age - nodes[segment - 1]) /
      (nodes[segment] - nodes[segment - 1])
  }
  slope <- function(from, to) (stockAt(to) - stockAt(from)) / (to - from)
  lossRate <- (as.vector(x[, , "gross_increment"]) - sum(area * slope(lower, upper)) / sum(area)) /
    as.vector(x[, , "stock_per_ha"])
  mortality <- as.vector(x[, , "mortality"]) / mean(as.vector(x[, , "standing_stock"]))

  from <- seq(0, 120, 20)
  binEnd <- from + 20
  classArea <- vapply(seq_along(from), function(ageClass) {
    sum(area * pmax(0, pmin(binEnd[ageClass], upper) - pmax(from[ageClass], lower)) / (upper - lower))
  }, 0)
  classStock <- vapply(seq_along(from), function(ageClass) {
    mean(stockAt(from[ageClass] + (binEnd[ageClass] - from[ageClass]) * (seq_len(2000) - 0.5) / 2000))
  }, 0)
  net <- slope(from, binEnd) + (lossRate - mortality) * classStock
  bins <- paste0("a", c(0, from[-1] + 1), "_", binEnd)
  out <- data.frame(iso = getItems(x, 1), variable = rep(c("area", "increment_rel"), each = length(bins)), bin = bins,
                    value = c(classArea, net / classStock))
  return(as.magpie(out, spatial = 1, datacol = 4))
}
