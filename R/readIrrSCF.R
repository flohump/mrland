#' @title readIrrSCF
#' @description Read soil organic carbon stock change factors for irrigation
#'   (F_IRR), relative to rainfed conditions. The subtype selects the data
#'   source.
#'
#' Subtypes:
#' \describe{
#'   \item{Trost}{Trost et al. (2013), Irrigation, soil organic carbon and N2O
#'     emissions. A review. Agronomy for Sustainable Development 33(4), 733-749.
#'     https://doi.org/10.1007/s13593-013-0134-0}
#' }
#' An \code{Emde} subtype (Emde et al. 2021, IPCC NGGIA 2019 refinement) can be
#' added once its table is prepared.
#'
#' @param subtype data source (e.g. \code{Trost})
#' @return magpie object with a single GLO cell and the irrigation stock change
#' factor per aridity class (arid, semi_arid, humid) (1)
#' @author Kristine Karstens
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("IrrSCF", "Trost", convert = FALSE)
#' }
#'
readIrrSCF <- function(subtype) {

  # Trost et al. 2013 is the current source; add "Emde" -> its file here later.
  files <- c("Trost" = "f_irrigation_soc_raw.csv")
  file  <- toolSubtypeSelect(subtype, files)

  data <- read.csv(file, sep = ";", stringsAsFactors = FALSE, comment.char = "*")
  out  <- new.magpie(names = data[, 1], sets = c("region", "years", "data"))
  out[, , ] <- data[, 2]

  return(out)
}
