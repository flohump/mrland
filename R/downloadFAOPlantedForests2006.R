#' @title downloadFAOPlantedForests2006
#' @description Download the FAO Global planted forests thematic study (Del Lungo, Ball and Carle 2006,
#' Planted Forests and Trees Working Paper FP/38E), the questionnaire survey of 61 countries for 2005.
#' @author Florian Humpenoeder
#' @seealso [madrat::downloadSource()]
downloadFAOPlantedForests2006 <- function() {
  url <- "https://openknowledge.fao.org/3/bl366e/bl366e.pdf"
  utils::download.file(url, destfile = "FAO_2006_Planted_Forests_Thematic_Study_WP38.pdf", mode = "wb")
  toolPdfText("FAO_2006_Planted_Forests_Thematic_Study_WP38.pdf")   # reading then needs no PDF software
  return(list(url = url,
              doi = NULL,
              title = "Global planted forests thematic study: results and analysis",
              author = "A. Del Lungo, J. Ball, J. Carle",
              version = "2006",
              release_date = "2006-12-01",
              description = "Planted Forests and Trees Working Paper FP/38E, FAO Rome, handle 20.500.14283/bl366e",
              license = "FAO open access",
              unit = "1000 ha"))
}
