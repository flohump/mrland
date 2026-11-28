#' @title readForestInventoryAge
#' @description National forest inventory values by stand age class, read from the published tables.
#' "FIN" (13th inventory 2019 to 2023, productive forest, Tables 50, 51, 64 of Korhonen et al. 2024), "SWE"
#' (productive forest land 2020, growth 2015 incl. formally protected areas, on the classes of the growth table),
#' "DEU" (fourth inventory 2022) and "NOR" (2021, forest land used for forestry, 10-year classes merged to 20 years):
#' forest area (`area`, Mha), growing stock (`growing_stock`, m3 per ha), gross annual increment (`increment`, m3
#' per ha per yr). "JPN": planted forest area by 5-year class, 2007 (`planted_area`, ha); "JPNhandbook": natural
#' forest area by 5-year class, 2012 (`natural_area`, ha). "USA" (FIADB, 48 conterminous states, 2021 to 2023) and
#' "CAN" (NFI table 5.0, treed forest land, 2017): forest area by 20-year class (`area`, Mha). "Avitabile2023":
#' gross annual increment and natural losses of European countries, 2015 (m3 per ha per yr). "CHN": 9th inventory
#' (2014 to 2018), natural arbor forest, as transcribed. "DEU" (bwi.info result pages), "NOR"
#' (landsskog.nibio.no) and "CHN" are placed in the source folder by hand, each with a NOTE.md.
#' @param subtype "FIN", "SWE", "DEU", "NOR", "JPN", "JPNhandbook", "USA", "CAN", "Avitabile2023" or "CHN"
#' @return magpie object, country x inventory year x (variable . bin), bin "a<from>_<to>" in years (to = Inf for
#' an open class), "all", or the item of the transcription (CHN)
#' @author Florian Humpenoeder
#' @seealso [madrat::readSource()], [downloadForestInventoryAge()]
readForestInventoryAge <- function(subtype = "FIN") {
  number <- function(x) suppressWarnings(as.numeric(x))
  long <- function(iso, year, variable, from, binEnd, value) {
    data.frame(iso = iso, year = year, variable = variable, bin = paste0("a", from, "_", binEnd), value = value)
  }
  sheet <- function(file, name) {
    as.matrix(readxl::read_excel(file, sheet = name, col_names = FALSE, .name_repair = "minimal"))
  }

  if (subtype == "FIN") {
    file <- "silvafennica_24045_S2.xlsx"
    classes <- c("1-20", "21-40", "41-60", "61-80", "81-100", "101-120", "121-140", "141-160", ">160")
    from <- seq(1, 161, 20)
    binEnd <- c(seq(20, 160, 20), Inf)
    # whole-country row of all species in the block "Productive forest, Total": its last row labelled "Total"
    blockTotal <- function(name) {
      tableCells <- sheet(file, name)
      start <- which(tableCells[, 1] == "Productive forest, Total")
      row <- max(which(tableCells[, 2] == "Total"))
      stopifnot(length(start) == 1, row > start)
      number(tableCells[row, match(classes, tableCells[start + 2, ])])
    }
    table64 <- sheet(file, "Table 64")
    header <- which(apply(table64 == "1-20", 1, any, na.rm = TRUE))
    increment <- number(table64[which(table64[, 1] == "Whole country"), match(classes, table64[header, ])])
    out <- rbind(long("FIN", 2021, "area", from, binEnd, blockTotal("Table 50") / 1e4),          # km2 to Mha
                 long("FIN", 2021, "growing_stock", from, binEnd, blockTotal("Table 51")),
                 long("FIN", 2021, "increment", from, binEnd, increment))
  } else if (subtype == "SWE") {
    # PC-Axis file as an array over its stub and heading variables; the whole country has the county code 000
    readPcAxis <- function(file) {
      txt <- paste0("\n", paste(readLines(file, warn = FALSE, encoding = "latin1"), collapse = "\n"))
      txt <- iconv(txt, "ISO-8859-15", "UTF-8")
      statement <- function(key) {
        rest <- substring(txt, regexpr(paste0("\n", key, "="), txt, fixed = TRUE) + nchar(key) + 2)
        substring(rest, 1, regexpr(";", rest, fixed = TRUE) - 1)
      }
      quoted <- function(text) gsub("\"", "", regmatches(text, gregexpr("\"[^\"]*\"", text))[[1]])
      variables <- c(quoted(statement("STUB")), quoted(statement("HEADING")))
      values <- lapply(variables, function(variable) quoted(statement(paste0("VALUES(\"", variable, "\")"))))
      values[[2]] <- quoted(statement(paste0("CODES(\"", variables[2], "\")")))
      data <- number(gsub("\"", "", strsplit(trimws(statement("DATA")), "\\s+")[[1]]))
      stopifnot(length(data) == prod(lengths(values)), "000" %in% values[[2]])
      aperm(array(data, rev(lengths(values)), rev(values)), rev(seq_along(values)))
    }
    area <- readPcAxis("PS_Areal_alderklasser_tab_a.px")
    area <- area["2020", "000", grep("1000 ha", dimnames(area)[[3]], fixed = TRUE), ]
    stock <- readPcAxis("PS_Virkesf_per_hektar_aldersklasser_tab.px")
    stock <- stock["2020", "000", 1, ]
    growth <- readPcAxis("PS_Tillvaxt_alder_tab.px")
    growth <- growth["2015", "000", 1, grep("^Inkl", dimnames(growth)[[4]]), ]   # incl. formally protected areas
    classes <- c("0-", "21-", "41-", "61-", "81-", "101-", "121-")        # age classes of the growth table
    group <- c("0-" = "0-", "3-" = "0-", "11-" = "0-", "21-" = "21-", "31-" = "21-", "41-" = "41-", "61-" = "61-",
               "81-" = "81-", "101-" = "101-", "121-" = "121-", "141-" = "121-", "161-" = "121-")
    fine <- area[names(group)]
    fineStock <- stock[sub("161-", "141-", names(group), fixed = TRUE)]   # the top stock class is 141 and older
    groupArea <- tapply(fine, group, sum)[classes]
    groupStock <- tapply(fine * fineStock, group, sum)[classes] / groupArea
    from <- seq(1, 121, 20)
    binEnd <- c(seq(20, 120, 20), Inf)
    out <- rbind(long("SWE", 2020, "area", from, binEnd, as.vector(groupArea) / 1000),            # 1000 ha to Mha
                 long("SWE", 2020, "growing_stock", from, binEnd, as.vector(groupStock)),
                 long("SWE", 2020, "increment", from, binEnd, as.vector(growth[classes])))
  } else if (subtype == "DEU") {
    # values of the nine age classes and of all classes in the table row that starts with the label and a unit
    tableRow <- function(file, label) {
      txt <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = " ")
      txt <- gsub("<[^>]+>", " ", gsub("=\"[^\"]*\"", "", txt))               # attribute values hold ">"
      rowRe <- paste0(label, "\\s*\\[[^\\]]*\\]((\\s+[-0-9.,]+){10})")
      row <- regmatches(txt, regexpr(rowRe, txt, perl = TRUE))
      stopifnot(length(row) == 1)
      cells <- strsplit(trimws(sub("^.*\\]", "", row)), "\\s+")[[1]]
      number(sub(",", ".", gsub(".", "", cells, fixed = TRUE), fixed = TRUE))
    }
    area <- tableRow("BWI2022_3.06_area.html", "alle Baumarten")
    stopifnot(abs(sum(area[1:9]) - area[10]) < 10)                            # classes add up to the total, ha
    from <- seq(1, 161, 20)
    binEnd <- c(seq(20, 160, 20), Inf)
    stock <- tableRow("BWI2022_3.06_stock.html", "alle Baumarten")
    increment <- tableRow("BWI2022_5.05_increment.html", "Deutschland \\(alle L.nder\\)")
    out <- rbind(long("DEU", 2022, "area", from, binEnd, area[1:9] / 1e6),
                 long("DEU", 2022, "growing_stock", from, binEnd, stock[1:9]),
                 long("DEU", 2022, "increment", from, binEnd, increment[1:9]))
  } else if (subtype == "NOR") {
    nibio <- utils::read.csv("landsskog_app_ref2021_skogbruksmark_aldersklasse10.csv", comment.char = "#")
    lower <- number(sub("^\\D*([0-9]+).*$", "\\1", nibio$age_class))        # "[20,30)" and ">=160"
    stopifnot(nrow(nibio) == 17, lower == seq(0, 160, 10))
    group <- lower %/% 20 * 20
    mean20 <- function(perHa) {
      as.vector(tapply(nibio$area_ha * perHa, group, sum) / tapply(nibio$area_ha, group, sum))
    }
    from <- seq(0, 160, 20)
    binEnd <- c(seq(19, 159, 20), Inf)
    out <- rbind(long("NOR", 2021, "area", from, binEnd, as.vector(tapply(nibio$area_ha, group, sum)) / 1e6),
                 long("NOR", 2021, "growing_stock", from, binEnd, mean20(nibio$vol_ub_m3ha)),
                 long("NOR", 2021, "increment", from, binEnd, mean20(nibio$incr_ub_m3ha_yr)))
  } else if (subtype == "JPN") {
    speciesTable <- sheet("jyusyu_ha.xls", 1)                              # planted forest, all species
    row <- which(trimws(speciesTable[, 2]) == intToUtf8(35336))            # the row of the national total
    area <- number(speciesTable[row, 3:21])
    stopifnot(length(row) == 1, abs(sum(area) - number(speciesTable[row, 22])) < 1)
    out <- long("JPN", 2007, "planted_area", seq(1, 91, 5), c(seq(5, 90, 5), Inf), area)
  } else if (subtype == "JPNhandbook") {
    # table 6 (1), row of all species ("total") in two blocks: age classes 1 to 10, and 11 to 19 with the sum
    page <- grep(intToUtf8(c(22825, 28982, 26519, 40802, 32026, 21029, 38754, 31309)),
                 toolPdfText("youran_mokuzi2017-8.pdf"), value = TRUE)
    lines <- strsplit(page, "\n")[[1]]
    lines <- lines[seq_len(grep(intToUtf8(c(22825, 28982, 26519, 40802, 32026, 21029, 33988, 31309)), lines) - 1)]
    rows <- grep(paste0("^\\s*", intToUtf8(c(32207, 32, 25968))), lines, value = TRUE)
    values <- number(gsub(",", "", unlist(regmatches(rows, gregexpr("[0-9][0-9,]*", rows)))))
    stopifnot(length(page) == 1, length(rows) == 2, length(values) == 20, abs(sum(values[1:19]) - values[20]) < 1)
    out <- long("JPN", 2012, "natural_area", seq(1, 91, 5), c(seq(5, 90, 5), Inf), values[1:19])
  } else if (subtype == "USA") {
    estimates <- jsonlite::fromJSON("fiadb_forest_area_stand_age.json")$estimates
    age <- regmatches(estimates$GRP1, regexec("([0-9]+)(-([0-9]+)|\\+) years", estimates$GRP1))
    known <- lengths(age) > 0                                                # all but "Not available"
    lowerAge <- number(vapply(age[known], function(ageMatch) ageMatch[2], ""))
    upperAge <- number(vapply(age[known], function(ageMatch) ageMatch[4], ""))
    upperAge[is.na(upperAge)] <- Inf
    stopifnot(sum(!known) <= 1, all(diff(lowerAge) > 0))
    out <- long("USA", 2022, "area", lowerAge, upperAge,
                estimates$ESTIMATE[known] * 0.40468564224 / 1e6)            # acres to Mha
  } else if (subtype == "CAN") {
    ageTable <- utils::read.csv("ca_t5_forage20_area_en.csv", skip = 1, check.names = FALSE)
    classes <- c(paste0(seq(1, 181, 20), "-", seq(20, 200, 20)), "201+")
    treed <- ageTable[startsWith(ageTable[[1]], "Treed"), classes]
    stopifnot(nrow(treed) == 3)
    out <- long("CAN", 2017, "area", c(seq(1, 181, 20), 201), c(seq(20, 200, 20), Inf),
                colSums(treed) / 1000)                                       # 1000 ha to Mha
  } else if (subtype == "Avitabile2023") {
    incrementSheet <- sheet("Biomass_and_Increment_STATISTICS.xlsx", "INCREMENT 2015")
    stopifnot(incrementSheet[3, 8:9] == c("GAI", "ANL"), grepl("ha/yr", incrementSheet[4, 8]))
    national <- incrementSheet[which(incrementSheet[, 2] == incrementSheet[, 3]), ]
    out <- data.frame(iso = rep(toolCountry2isocode(national[, 1]), 2), year = 2015,
                      variable = rep(c("gross_increment", "natural_losses"), each = nrow(national)), bin = "all",
                      value = number(c(national[, 8], national[, 9])))
  } else if (subtype == "CHN") {
    transcription <- utils::read.csv("china_nfi9_natural_forest.csv")
    out <- data.frame(iso = "CHN", year = 2016, variable = transcription$variable, bin = transcription$item,
                      value = transcription$value)
  } else {
    stop("unknown subtype: ", subtype)
  }
  stopifnot(!anyNA(out$iso), !anyNA(out$value), !anyDuplicated(out[, 1:4]))
  return(as.magpie(out, spatial = 1, temporal = 2, datacol = 5))
}
