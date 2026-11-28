#' @title toolForestAgeClasses
#' @description Names of the 5-year forest age classes: ac0 to ac300 and the open class acx.
#' @return character vector
#' @author Florian Humpenoeder
toolForestAgeClasses <- function() {
  return(c(paste0("ac", seq(0, 300, 5)), "acx"))
}
