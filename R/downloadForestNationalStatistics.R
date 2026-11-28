#' @title downloadForestNationalStatistics
#' @description Download forestry statistics by country. Subtype "ForestEurope2020": State of Europe's Forests
#' 2020 (FOREST EUROPE), the report with its annex tables by country. Subtype "FP48": FAO Planted Forests and Trees
#' Working Paper FP/48/E (Juergensen, Kollert and Lebedys 2014), industrial roundwood from planted forests in 2012.
#' The text of each PDF is written next to it ([toolPdfText()]).
#' @param subtype "ForestEurope2020" or "FP48"
#' @author Florian Humpenoeder
#' @seealso [madrat::downloadSource()], [readForestNationalStatistics()]
downloadForestNationalStatistics <- function(subtype = "ForestEurope2020") {
  meta <- switch(subtype,
                 ForestEurope2020 = list(url = "https://foresteurope.org/wp-content/uploads/2016/08/SoEF_2020.pdf",
                                         file = "SoEF_2020.pdf",
                                         title = "State of Europe's Forests 2020",
                                         author = "FOREST EUROPE",
                                         version = "2020",
                                         release_date = "2020",
                                         description = paste("Report of the Ministerial Conference on the",
                                                             "Protection of Forests in Europe with annex tables",
                                                             "of the pan-European indicators by country"),
                                         license = "(c) Ministerial Conference on the Protection of Forests in Europe",
                                         unit = "1000 ha, 1000 m3"),
                 FP48 = list(url = "https://www.fao.org/3/a-i3384e.pdf",
                             file = "FAO_2014_FP48_roundwood_planted_forests.pdf",
                             title = "Assessment of industrial roundwood production from planted forests",
                             author = "C. Juergensen, W. Kollert, A. Lebedys",
                             version = "2014",
                             release_date = "2014",
                             description = "FAO Planted Forests and Trees Working Paper FP/48/E, Rome",
                             license = "(c) FAO, use and reproduction encouraged with acknowledgement",
                             unit = "m3"),
                 stop("unknown subtype: ", subtype))
  withr::local_options(timeout = 3600)
  utils::download.file(meta$url, destfile = meta$file, mode = "wb")
  toolPdfText(meta$file)
  return(meta[names(meta) != "file"])
}
