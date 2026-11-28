#' @title calcWoodfuelNonforestShare
#' @description Renewable share of wood fuel (1 - fNRB) of the decade 2020 to 2030 from the MoFuSS simulations,
#' weighted by the MoFuSS wood fuel harvest. Countries without MoFuSS simulation carry no weight; a region without
#' any gets 0.
#' @return magpie object with the renewable share by country
#' @author Florian Humpenoeder
#' @seealso [readMoFuSS()]
#' @examples
#' \dontrun{
#' calcOutput("WoodfuelNonforestShare")
#' }
calcWoodfuelNonforestShare <- function() {
  x <- readSource("MoFuSS")[, "y2030", ]
  share <- setYears(1 - x[, , "fNRB"] / 100, NULL)
  harvest <- setYears(x[, , "harvest"], NULL)
  share[harvest == 0] <- 0
  return(list(x = setNames(share, NULL),
              weight = setNames(harvest, NULL) + 10^-10,
              unit = "1",
              description = "Renewable share of wood fuel (1 - fNRB, MoFuSS 2020 to 2030, harvest-weighted)"))
}
