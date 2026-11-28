#' @title toolChinaAgeGroups
#' @description Area, growing stock and age range of the five age groups of China's inventory
#' ([readForestInventoryAge()], subtype "CHN"): the age limits averaged over the forest types by area, the
#' over-mature group from its lower limit L to 2 x 1.5 L - L.
#' @param x magpie object of [readForestInventoryAge()], subtype "CHN"
#' @return data frame with the group, its lower and upper age (yr), its area (Mha) and its growing stock (Gm3)
#' @author Florian Humpenoeder
toolChinaAgeGroups <- function(x) {
  itemValues <- function(variable) {
    values <- collapseNames(x[, , variable])
    setNames(as.vector(values), getNames(values))
  }
  groups <- c("young", "middle_aged", "near_mature", "mature", "over_mature")
  schemeArea <- itemValues("scheme_area")
  limit <- vapply(groups[1:4], function(group) {
    sum(itemValues(paste0("limit_", group))[names(schemeArea)] * schemeArea) / sum(schemeArea)
  }, 0)
  return(data.frame(group = groups, lower = c(0, limit), upper = c(limit, 2 * 1.5 * limit[[4]] - limit[[4]]),
                    area = as.vector(itemValues("area")[groups]), stock = as.vector(itemValues("stock")[groups])))
}
