#' @title readMoFuSS
#' @description Read the MoFuSS country summary: fraction of non-renewable biomass (fNRB, percent) and wood fuel
#' harvest (tonnes), means of the decades 2020 to 2030, 2030 to 2040 and 2040 to 2050.
#' @return magpie object with countries, the decades as years (2030, 2040, 2050) and the variables fNRB and harvest
#' @author Florian Humpenoeder
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' readSource("MoFuSS")
#' }
readMoFuSS <- function() {
  countrySummary <- utils::read.csv("summary_adm0.csv")
  decades <- c(y2030 = "2020_2030", y2040 = "2030_2040", y2050 = "2040_2050")
  x <- new.magpie(countrySummary$GID_0, names(decades), c("fNRB", "harvest"), sets = c("iso", "t", "variable"))
  for (year in names(decades)) {
    x[, year, "fNRB"] <- countrySummary[[paste0("fNRB_", decades[[year]], "_mean")]]
    x[, year, "harvest"] <- countrySummary[[paste0("Harv_", decades[[year]], "_mean")]]
  }
  return(x)
}
