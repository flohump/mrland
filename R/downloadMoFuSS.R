#' @title downloadMoFuSS
#' @description Download the country summary of the MoFuSS global wood fuel simulations 2010 to 2050: wood fuel
#' harvest, non-renewable biomass and the fraction of non-renewable biomass (fNRB), the values the UNFCCC
#' Methodological Panel uses for default fNRB values.
#' @author Florian Humpenoeder
#' @seealso [madrat::downloadSource()]
downloadMoFuSS <- function() {
  url <- "https://zenodo.org/api/records/18865904/files/summary_adm0.csv/content"
  utils::download.file(url, destfile = "summary_adm0.csv", mode = "wb")
  return(list(url = url,
              doi = "10.5281/zenodo.18865904",
              title = "MoFuSS global simulations 1km 2010-2050",
              author = "Adrian Ghilardi, Robert Bailis",
              version = "2025-12-20",
              release_date = "2025-12-20",
              description = paste("Country summary (adm0) of the MoFuSS wood fuel simulations:",
                                  "harvest, non-renewable biomass, fNRB"),
              license = "Creative Commons Attribution 4.0 International",
              unit = "harvest in tonnes, fNRB in percent"))
}
