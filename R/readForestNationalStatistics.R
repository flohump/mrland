#' @title readForestNationalStatistics
#' @description Forestry statistics by country, read from the tables of the published reports (text of the PDF
#' files, [toolPdfText()]).
#'
#' Subtype "ForestEurope2020" (State of Europe's Forests 2020): forest area 2020 and forest available for wood
#' supply 1990 to 2020 (annex Table 4, 1000 ha), fellings over bark (annex Table 19) and total roundwood removals
#' (annex Table 20) in forest available for wood supply, 1990 to 2015 (1000 m3). The item "EU28" holds the
#' roundwood under bark and the fellings over bark of the EU-28 (Table 3.2-2, mean of 2013 to 2017, given as 2015).
#' Annex Table 11: forest area and even-aged forest in 2015, and the even-aged forest by development phase
#' (`regeneration`, `intermediate`, `mature`, `unspecified`, 1000 ha).
#'
#' Subtype "FP48" (FAO working paper FP/48/E, Table 9): industrial roundwood production in plantations in 2012 (m3).
#' @param subtype "ForestEurope2020" or "FP48"
#' @return magpie object, country x year x variable; NA where the table has no value
#' @author Florian Humpenoeder
#' @seealso [madrat::readSource()], [downloadForestNationalStatistics()]
readForestNationalStatistics <- function(subtype = "ForestEurope2020") {
  number <- function(cell) as.numeric(gsub(" ", "", sub("^-$", NA, cell)))

  if (subtype == "FP48") {
    txt <- toolPdfText("FAO_2014_FP48_roundwood_planted_forests.pdf")
    lines <- strsplit(txt[grepl("\n\\s*1\\. Brazil ", txt)], "\n")[[1]]
    entry <- "[0-9]+\\.\\s*([^0-9]+?)\\s{2,}([0-9][0-9 ]*[0-9])(?=\\s+[0-9]+\\.\\s*[^0-9 ]|\\s*$)"
    found <- regmatches(lines, gregexpr(entry, lines, perl = TRUE))
    found <- do.call(rbind, regmatches(unlist(found), regexec(entry, unlist(found), perl = TRUE)))
    total <- number(sub("^\\s*TOTAL\\s+", "", grep("^\\s*TOTAL\\s", lines, value = TRUE)))
    value <- number(found[, 3])
    stopifnot(nrow(found) == 78, abs(sum(value) - total) <= 1)   # the printed total is 1 m3 above the sum
    countryCodes <- c("tanzania, ur" = "TZA", "iran, i.r" = "IRN", "papua n. guinea" = "PNG",
                      "trinidad/tobago" = "TTO", "lao pdrs" = "LAO", "uk" = "GBR")
    long <- data.frame(iso = toolCountry2isocode(trimws(found[, 2]), mapping = countryCodes), year = 2012,
                       variable = "plantation_removals", value = value)
  } else if (subtype == "ForestEurope2020") {
    txt <- toolPdfText("SoEF_2020.pdf")
    # country rows of the annex table whose title is a whole line of its page
    annexTable <- function(title, variables, years) {
      page <- txt[vapply(strsplit(txt, "\n"), function(candidateLines) {
        any(trimws(gsub("[[:cntrl:]]", "", candidateLines)) == title)
      }, TRUE)]
      stopifnot(length(page) == 1)
      rowRe <- "^\\s*([A-Z][A-Za-z .,'()-]*?[a-z)])\\s{2,}(\\S.*\\S)\\s*$"
      pageLines <- strsplit(page, "\n")[[1]]
      rows <- regmatches(pageLines, regexec(rowRe, pageLines, perl = TRUE))
      rows <- rows[lengths(rows) == 3]
      cells <- lapply(rows, function(row) strsplit(row[3], "\\s{2,}")[[1]])
      isData <- vapply(cells, function(rowCells) all(grepl("^-?[0-9 ]+(\\.[0-9]+)?$|^-$", rowCells)), TRUE)
      stopifnot(sum(isData) == 46, length(unique(lengths(cells[isData]))) == 1,     # the report has 46 countries
                lengths(cells[isData])[1] >= length(years))
      country <- sub("\\s*\\(.*\\)$", "", vapply(rows[isData], function(row) row[2], ""))
      data.frame(iso = rep(toolCountry2isocode(country), each = length(years)), year = years,
                 variable = variables,
                 value = unlist(lapply(cells[isData], function(rowCells) number(rowCells[seq_along(years)]))))
    }
    fawsYears <- c(1990, 2000, 2005, 2010, 2015, 2020)
    long <- rbind(
      annexTable("Table 4: Ind. 1.1 Change in area of forest available for wood supply, 1990-2020",
                 c("forest_area", rep("faws", 6)), c(2020, fawsYears)),
      annexTable("Table 19: Ind. 3.1 Increment in forest available for wood supply, 1990-2015 (Cont.)",
                 "fellings", c(NA, 1990, 2000, 2010, 2015)),
      annexTable("Table 20: Ind. 3.2 Volume and value of total roundwood removals, 1990-2015",
                 "removals", c(NA, 1990, 2000, 2010, 2015)),
      annexTable("Table 11: Ind. 1.3 Age-class distribution in all even-aged forest stands, 2015",
                 c("forest_area", "even_aged", "regeneration", "intermediate", "mature", "unspecified"),
                 rep(2015, 6))
    )
    long <- long[!is.na(long$year), ]
    regionRe <- "^\\s*EU-28\\s+([0-9 ]+?)\\s{2,}([0-9 ]+?)\\s{2,}[0-9.]+\\s*$"
    page <- txt[grepl("\n\\s*Table 3.2-2: Proportion of reported roundwood and fellings, by region, 2015\\s*\n", txt)]
    pageLines <- strsplit(page, "\n")[[1]]
    eu28 <- regmatches(pageLines, regexec(regionRe, pageLines, perl = TRUE))
    eu28 <- eu28[lengths(eu28) == 3]
    stopifnot(length(page) == 1, length(eu28) == 1)
    long <- rbind(long, data.frame(iso = "EU28", year = 2015, variable = c("removals", "fellings"),
                                   value = number(eu28[[1]][2:3])))
  } else {
    stop("unknown subtype: ", subtype)
  }
  stopifnot(!anyNA(long$iso), !anyDuplicated(long[, 1:3]))
  return(as.magpie(long, spatial = 1, temporal = 2, datacol = 4))
}
