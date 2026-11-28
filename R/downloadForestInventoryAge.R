#' @title downloadForestInventoryAge
#' @description Download national forest inventory tables by stand age class. Subtypes: "FIN" (Korhonen et al.
#' 2024, 13th national forest inventory of Finland, supplementary tables), "SWE" (SLU, official statistics of the
#' Swedish national forest inventory, tables 3.2a, 3.21 and 3.31c as they stand in the database on the day of the
#' download), "JPN" (Forestry Agency of Japan, state of forest resources of 31 March 2007, area by tree species
#' and age class), "JPNhandbook" (Forestry Agency of Japan, forestry statistics handbook 2017, chapter 1, with
#' the natural forest area by age class of 31 March 2012; its text is written next to it, [toolPdfText()]),
#' "USA" (USDA Forest Service, FIADB-API: area of forest land by 20-year stand age class, 48 conterminous
#' states, evaluations 2021 to 2023), "CAN" (Canada's National Forest Inventory, table 5.0: area of forest land
#' by forest type and age class) and "Avitabile2023" (Avitabile et al., harmonised statistics of forest biomass
#' and increment in Europe). The subtypes "DEU", "NOR" and "CHN" have no file link and are placed in the source
#' folder by hand, see [readForestInventoryAge()].
#' @param subtype "FIN", "SWE", "JPN", "JPNhandbook", "USA", "CAN" or "Avitabile2023"
#' @author Florian Humpenoeder
#' @seealso [madrat::downloadSource()], [readForestInventoryAge()]
downloadForestInventoryAge <- function(subtype = "FIN") {
  slu <- "https://skogsstatistik.slu.se/Resources/PX/Databases/OffStat/ProduktivSkogsmark/"
  # FIADB evaluation of each of the 48 conterminous states (state code and year)
  fiaEvaluations <- paste(c("12022", "42022", "52022", "62021", "82022", "92022", "102022", "122022", "132022",
                            "162022", "172022", "182022", "192023", "202022", "212022", "222023", "232022", "242022",
                            "252022", "262022", "272022", "282022", "292022", "302022", "312022", "322022", "332022",
                            "342022", "352022", "362022", "372022", "382022", "392022", "402022", "412022", "422022",
                            "442022", "452022", "462022", "472022", "482022", "492022", "502022", "512022", "532022",
                            "542022", "552022", "562022"), collapse = ",")
  meta <- switch(subtype,
                 FIN = list(url = "https://www.silvafennica.fi/supp_files/article24045/S2.xlsx",
                            file = "silvafennica_24045_S2.xlsx",
                            doi = "10.14214/sf.24045",
                            title = "Forests of Finland 2019-2023 and their development 1921-2023, supplement S2",
                            author = "K. T. Korhonen et al.",
                            version = "2024",
                            release_date = "2024-11-06",
                            description = "Result tables of the 13th national forest inventory of Finland",
                            license = "CC BY-SA 4.0",
                            unit = "km2, m3 per ha, m3 per ha per yr"),
                 SWE = list(url = paste0(slu, c("Areal/PS_Areal_alderklasser_tab_a.px",
                                                "Virkesforrad/PS_Virkesf_per_hektar_aldersklasser_tab.px",
                                                "Tillvaxt/PS_Tillvaxt_alder_tab.px")),
                            file = c("PS_Areal_alderklasser_tab_a.px", "PS_Virkesf_per_hektar_aldersklasser_tab.px",
                                     "PS_Tillvaxt_alder_tab.px"),
                            title = "Forest statistics, official statistics of Sweden: productive forest land",
                            author = "Swedish University of Agricultural Sciences (SLU), national forest inventory",
                            version = paste("database of", Sys.Date()),
                            release_date = as.character(Sys.Date()),
                            description = paste("Tables 3.2a (area), 3.21 (growing stock per ha) and 3.31c (annual",
                                                "growth per ha) by age class"),
                            license = "not stated (official statistics of Sweden)",
                            unit = "1000 ha, m3sk per ha, m3sk per ha per yr"),
                 JPN = list(url = "https://www.rinya.maff.go.jp/j/keikaku/genkyou/h19/other/jyusyu_ha.xls",
                            file = "jyusyu_ha.xls",
                            title = "State of forest resources of 31 March 2007: area by tree species and age class",
                            author = "Forestry Agency of Japan",
                            version = "2007",
                            release_date = "2007-03-31",
                            description = "Forest under forest plans, stocked land, planted and natural forest",
                            license = "Public Data License (version 1.0) of the Government of Japan",
                            unit = "ha"),
                 JPNhandbook = list(url = paste0("https://www.rinya.maff.go.jp/j/kikaku/toukei/attach/pdf/",
                                                 "youran_mokuzi2017-8.pdf"),
                                    file = "youran_mokuzi2017-8.pdf",
                                    title = paste("Forestry statistics handbook 2017, chapter 1: national economy",
                                                  "and forest resources"),
                                    author = "Forestry Agency of Japan",
                                    version = "2017",
                                    release_date = "2017",
                                    description = paste("Table 6: area and growing stock of natural forest by",
                                                        "age class, 31 March 2012"),
                                    license = "Public Data License (version 1.0) of the Government of Japan",
                                    unit = "ha"),
                 USA = list(url = paste0("https://apps.fs.usda.gov/fiadb-api/fullreport?snum=2&rselected=",
                                         utils::URLencode("Stand age 20 yr classes (0 to 500 plus)",
                                                          reserved = TRUE),
                                         "&cselected=None&outputFormat=NJSON&wc=", fiaEvaluations),
                            file = "fiadb_forest_area_stand_age.json",
                            title = "Area of forest land by stand age class, 48 conterminous states",
                            author = "USDA Forest Service, Forest Inventory and Analysis Program",
                            version = "FIADB evaluations 2021 to 2023",
                            release_date = as.character(Sys.Date()),
                            description = paste("FIADB-API fullreport, estimate 2 (area of forest land) by",
                                                "20-year stand age class"),
                            license = "see https://www.fia.fs.usda.gov",
                            unit = "acres"),
                 CAN = list(url = paste0("https://nfi.nfis.org/resources/general/summaries/t1/en/CA/csv/",
                                         "ca_t5_forage20_area_en.csv"),
                            file = "ca_t5_forage20_area_en.csv",
                            title = "Table 5.0: area of forest land by forest type and age class in Canada",
                            author = "Canada's National Forest Inventory",
                            version = "remeasured 2007 to 2017 and projected to 2017",
                            release_date = as.character(Sys.Date()),
                            description = "Area of forest land by forest type and 20-year age class",
                            license = "see https://nfi.nfis.org",
                            unit = "1000 ha"),
                 Avitabile2023 = list(url = "https://ndownloader.figshare.com/files/43678089",
                                      file = "Biomass_and_Increment_STATISTICS.xlsx",
                                      doi = "10.6084/m9.figshare.23932899.v4",
                                      title = "Biomass and Increment Statistics (Excel)",
                                      author = "V. Avitabile et al.",
                                      version = "4",
                                      release_date = "2023-12-15",
                                      description = paste("Harmonised statistics of forest biomass and increment in",
                                                          "Europe by country, doi:10.1038/s41597-023-02868-8"),
                                      license = "CC BY 4.0",
                                      unit = "see file"),
                 stop("subtype ", subtype, " has no file link: place its files in the source folder by hand, see ",
                      "?readForestInventoryAge"))
  withr::local_options(timeout = 3600)
  for (i in seq_along(meta$url)) {
    utils::download.file(meta$url[i], destfile = meta$file[i], mode = "wb")
  }
  if (subtype == "JPNhandbook") {
    toolPdfText(meta$file)
  }
  meta$url <- meta$url[1]
  return(meta[names(meta) != "file"])
}
