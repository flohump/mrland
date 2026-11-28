#' @title readFAOPlantedForests2006
#' @description Planted forest area by age class, country and category from Table 7 of the FAO Global planted
#' forests thematic study (Del Lungo et al. 2006), the questionnaire responses of 30 countries for 2005, main
#' species summed. Age classes 0-5, 5-10, 10-20, 20-30, 30-40, 40-50, 50-70, 70-90, 90-100 and over 100 years.
#' @return magpie object, country x category (productive or protective plantation or semi-natural) x age bin, 1000 ha
#' @author Florian Humpenoeder
#' @seealso [madrat::readSource()]
readFAOPlantedForests2006 <- function() {
  txt <- toolPdfText("FAO_2006_Planted_Forests_Thematic_Study_WP38.pdf")
  pages <- grep("Table 7 - Age class distributions by country, forest category and species\\s+\\([0-9]+ of 29\\)", txt)
  lines <- unlist(strsplit(txt[pages], "\n"))
  bins <- c("a0_5", "a5_10", "a10_20", "a20_30", "a30_40", "a40_50", "a50_70", "a70_90", "a90_100", "a100plus")
  rowPattern <- "^\\s{10,}(\\S.*?\\S)\\s{2,}((?:[0-9,]+\\.[0-9]\\s+){9}[0-9,]+\\.[0-9])\\s*$"
  categoryPattern <- "^\\s+(Productive|Protective) (Plantation|Semi-Natural Planted) Forests\\s*$"
  countryPattern <- "^ {20,40}([A-Z][A-Za-z ,'()-]+?)\\s*$"
  country <- NA
  category <- NA
  out <- NULL
  for (line in lines) {
    if (grepl(categoryPattern, line, perl = TRUE)) {
      fields <- regmatches(line, regexec(categoryPattern, line, perl = TRUE))[[1]]
      category <- paste0(tolower(fields[2]), "_", ifelse(fields[3] == "Plantation", "plantation", "semi_natural"))
    } else if (grepl(rowPattern, line, perl = TRUE)) {
      fields <- regmatches(line, regexec(rowPattern, line, perl = TRUE))[[1]]
      values <- as.numeric(gsub(",", "", strsplit(trimws(fields[3]), "\\s+")[[1]]))
      out <- rbind(out, data.frame(country = country, category = category, t(setNames(values, bins))))
    } else if (grepl(countryPattern, line, perl = TRUE) && !grepl("Annex|Table|Age Class", line)) {
      country <- trimws(regmatches(line, regexec(countryPattern, line, perl = TRUE))[[1]][2])
      category <- NA
    }
  }
  out <- out[!is.na(out$country) & !is.na(out$category), ]
  out$country <- toolCountry2isocode(out$country)
  agg <- stats::aggregate(out[, bins], by = out[, c("country", "category")], FUN = sum)
  long <- data.frame(country = rep(agg$country, length(bins)), category = rep(agg$category, length(bins)),
                     bin = rep(bins, each = nrow(agg)), value = unlist(agg[, bins]))
  x <- as.magpie(long, spatial = 1, temporal = NULL, datacol = 4)
  x[is.na(x)] <- 0
  return(x)
}
