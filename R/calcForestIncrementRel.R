#' @title calcForestIncrementRel
#' @description
#' Relative net increment of managed stands (net annual increment over growing stock, per year) by 5-year age
#' class, from the national forest inventories of the source ForestInventoryAge: Germany, Finland, Sweden, Norway
#' and, derived from stocks by age group, China ([toolChronosequenceIncrement()]).
#'
#' Per country: the relative gross increment of an inventory class (increment over growing stock) times 1 minus the
#' share of natural losses in the gross increment (Avitabile et al. 2023; China's increment is net already),
#' placed at the mid age of the class (an open top class counts as wide as the class below it; classes that start
#' at 140 years or later are not used), log-linear between the mid ages and constant below the first and above the
#' last. Countries without an inventory get no value and no weight, so the countries with an inventory stand for
#' their region and a region without one is 0. Weight: forest area of the inventory class, moved to the age
#' classes in the same way as the increment.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Florian Humpenoeder
#' @seealso [readForestInventoryAge()], [toolChronosequenceIncrement()]
#' @examples
#' \dontrun{
#' calcOutput("ForestIncrementRel", aggregate = TRUE, round = 5)
#' }
calcForestIncrementRel <- function() {
  inventories <- lapply(c("DEU", "FIN", "SWE", "NOR"), function(subtype) {
    dimSums(readSource("ForestInventoryAge", subtype, convert = FALSE), dim = 2)
  })
  inventories <- c(inventories, list(toolChronosequenceIncrement(readSource("ForestInventoryAge", "CHN",
                                                                            convert = FALSE))))
  losses <- dimSums(readSource("ForestInventoryAge", "Avitabile2023"), dim = 2)
  lossShare <- collapseNames(losses[, , "natural_losses"] / (losses[, , "gross_increment"] + 1e-10))
  age <- c(seq(2.5, 302.5, 5), 302.5)

  x <- new.magpie(getItems(lossShare, 1), NULL, toolForestAgeClasses(), fill = 0)
  weight <- x
  for (inv in inventories) {
    iso <- getItems(inv, 1)
    bins <- getItems(inv, "bin")
    from <- as.numeric(sub("^a([0-9]+)_.*$", "\\1", bins))
    binEnd <- as.numeric(sub("^.*_", "", bins))
    value <- function(variable) as.vector(inv[, , paste(variable, bins, sep = ".")])
    used <- which(value("area") > 0 & from < 140)
    used <- used[order(from[used])]
    upper <- binEnd[used]
    open <- which(is.infinite(upper))
    upper[open] <- from[used][open] + (upper - from[used])[open - 1]
    mid <- (from[used] + upper) / 2
    if ("increment_rel" %in% getItems(inv, "variable")) {
      net <- value("increment_rel")[used]
    } else {
      net <- (value("increment") / value("growing_stock"))[used] * (1 - as.vector(lossShare[iso, , ]))
    }
    x[iso, , ] <- exp(stats::approx(mid, log(net), age, rule = 2)$y)
    weight[iso, , ] <- exp(stats::approx(mid, log(value("area")[used]), age, rule = 2)$y)
  }

  return(list(x = x,
              weight = weight + 1e-10,
              unit = "1 per yr",
              min = 0,
              description = paste("Relative net annual increment of managed stands (increment over growing stock)",
                                  "by age class from national forest inventories, 0 where no inventory")))
}
