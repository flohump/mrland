#' @title readTrost
#' @description Read in soil organic carbon stock change factors for irrigation
#'
#' Based on:
#' Trost, B., Prochnow, A., Drastig, A., Meyer-Aurich, A., Ellmer, F., et al. (2013):
#' Irrigation, soil organic carbon and N2O emissions. A review. Agronomy for
#' Sustainable Development 33(4), 733-749. https://doi.org/10.1007/s13593-013-0134-0
#'
#' The factors give the soil organic carbon stock under irrigated agriculture
#' relative to rainfed conditions for the aridity classes arid, semi_arid and
#' humid. Emde et al. (2021, Global Change Biology 27, 3898-3910,
#' https://doi.org/10.1111/gcb.15680) reports similar values, see README in the
#' source folder.
#'
#' @param file Name of the source csv file. The file must be ";"-separated,
#' comment lines must start with "*", the (unnamed) first column must hold the
#' aridity class, the remaining columns the factors.
#'
#' @return magpie object containing soil organic carbon stock change factors for
#' irrigation (dimension 1: aridity classes, dimension 2: factor name) (1)
#' @author Kristine Karstens
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("Trost", convert = FALSE)
#' }
#'

readTrost <- function(file = "f_irrigation_soc_raw.csv") {

  data <- read.csv(file, sep = ";", stringsAsFactors = FALSE, comment.char = "*")
  out <- as.magpie(data)

  return(out)
}
