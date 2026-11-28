#' @title calcEndUseTimber
#' @description
#' Calculates the demand of timber from historical FAO data (including intermediate products).
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link[mrfaocore]{calcFAOmassbalance_pre}}
#' @examples
#'
#' \dontrun{
#' calcOutput("EndUseTimber")
#' }
#' @export

calcEndUseTimber <- function() {

  faoTimber <- collapseNames(calcOutput("TimberDemand", aggregate = FALSE)[, , "domestic_supply"])

  faoTimber <- faoTimber[, c(paste0("y", seq(from = 1965, to = 2020, by = 5))), ]

  getNames(faoTimber) <- gsub(x = getNames(faoTimber), pattern = " ", replacement = "_")
  getNames(faoTimber) <- gsub(x = getNames(faoTimber), pattern = "-", replacement = "_")
  getNames(faoTimber) <- tolower(getNames(faoTimber))

  out <- faoTimber

  return(list(x = out, weight = NULL,
              unit = "mio m3/yr",
              description = "historical timber domestic demand"))
}
