#' @title calcSoilStockChangeFactors
#' @description calculates and merges information on stock change factors
#'
#' @seealso
#' [readIPCC()]
#' [readTrost()]
#'
#' @param factor Which stock change factor to calculate: \code{landuse} (F_LU,
#' default), \code{tillage} (F_MG from IPCC 2019 Table 5.5), \code{input} (F_I
#' from IPCC 2019 Table 5.5), \code{fallow} (F_LU for fallow land, derived from
#' the IPCC 2019 land use factors) or \code{irrigation} (Trost et al. 2013).
#'
#' @examples
#' \dontrun{
#' calcOutput("SoilStockChangeFactors")
#' calcOutput("SoilStockChangeFactors", factor = "tillage")
#' calcOutput("SoilStockChangeFactors", factor = "input")
#' calcOutput("SoilStockChangeFactors", factor = "fallow")
#' calcOutput("SoilStockChangeFactors", factor = "irrigation")
#' }
#'
#' @return MAgPIE object of yields
#' @author Kristine Karstens

calcSoilStockChangeFactors <- function(factor = "landuse") {

  if (factor %in% c("tillage", "input")) {

    subtype <- if (factor == "tillage") "SCF_MG2019" else "SCF_I2019"
    raw <- readSource("IPCC", subtype = subtype, convert = FALSE)
    # raw arrives as GLO x "climateclass.factor", reshape into climate x factor
    values <- setNames(vapply(getNames(raw), function(n) as.numeric(raw[1, , n]), numeric(1)),
                       getNames(raw))
    classes <- sort(unique(sub("\\.[^.]*$", "", getNames(raw))))
    factors <- sort(unique(sub(".*\\.", "", getNames(raw))))
    out <- new.magpie(classes, NULL, names = factors, fill = 0)
    out[, , ] <- matrix(values[outer(classes, factors, paste, sep = ".")],
                        nrow = length(classes))
    return(list(x = out,
                unit = "1",
                description = if (factor == "tillage")
                                "Soil carbon stock change factors for tillage (IPCC 2019, Table 5.5)"
                              else
                                "Soil carbon stock change factors for input management (IPCC 2019, Table 5.5)"))

  } else if (factor == "fallow") {

    raw <- readSource("IPCC", subtype = "SCF_LU2019", convert = FALSE)
    # raw arrives as GLO x "climateclass.landusetype"
    values <- setNames(vapply(getNames(raw), function(n) as.numeric(raw[1, , n]), numeric(1)),
                       getNames(raw))
    classes <- sort(unique(sub("\\.[^.]*$", "", getNames(raw))))
    out <- new.magpie(classes, NULL, names = "fallow", fill = NA)
    for (cl in classes) {
      # assume bare fallow in dry regions (long-term cultivated value) and
      # vegetated fallow in moist regions (set-aside value)
      lu <- if (grepl("_dry$", cl)) "Long_term_cultivated" else "Set_aside_under20yrs"
      out[cl, , "fallow"] <- values[paste(cl, lu, sep = ".")]
    }
    return(list(x = out,
                unit = "1",
                description = paste("Soil carbon stock change factor for fallow land",
                                    "(IPCC 2019, Table 5.5; bare fallow in dry regions,",
                                    "vegetated fallow in moist regions)")))

  } else if (factor == "irrigation") {

    trost <- readSource("Trost", convert = FALSE)
    # trost arrives as GLO x aridity class (single data column)
    vals  <- setNames(vapply(getNames(trost), function(n) as.numeric(trost[1, , n]), numeric(1)),
                      getNames(trost))
    dry   <- as.numeric(vals["semi_arid"])
    moist <- as.numeric(vals["humid"])

    w   <- findset("w")
    kcr <- findset("kcr")
    out <- new.magpie(findset("climate59_2019"), NULL,
                      names = as.vector(outer(w, kcr, paste, sep = ".")), fill = 1)
    dryClasses   <- grep("_dry", getCells(out), value = TRUE)
    moistClasses <- grep("_moist", getCells(out), value = TRUE)
    irrigated    <- paste("irrigated", kcr, sep = ".")
    out[dryClasses, , irrigated]   <- dry
    out[moistClasses, , irrigated] <- moist
    # higher soil carbon of paddy rice is handled via the land use factor F_LU
    out[, , "irrigated.rice_pro"] <- 1
    return(list(x = out,
                unit = "1",
                description = paste("Soil carbon stock change factors for irrigation",
                                    "(Trost et al. 2013, Agronomy for Sustainable",
                                    "Development 33, 733-749)")))

  }

  scfLu2019 <- readSource("IPCC", subtype = "SCF_LU2019", convert = FALSE)

  kcr <- findset("kcr")
  lu  <- getNames(scfLu2019, dim = 2)
  lu2kcr <- new.magpie(names = as.vector(outer(lu, kcr, paste, sep = ".")),
                       fill  = 0, sets = c("regions", "t", "LU.kcr"))

  annCrop <- "Long_term_cultivated"
  perTree <- "Perennial_Tree_Crop"
  kcrInAnnCrop <- setdiff(kcr, c("betr", "begr", "oilpalm", "others", "rice_pro"))
  # sugar cane would be an perennial, but due to mostly highly managed systems (with peplanting every 2-5 years)
  # or traditional systems with burning soc stocks might be closer to annual SOC change factors
  # than to perennials (like trees)
  kcrInPerTree  <- c("betr", "begr", "oilpalm")
  kcrInMix      <- c("others", "rice_pro")
  lu2kcr[, , annCrop][, , kcrInAnnCrop] <- 1
  lu2kcr[, , perTree][, , kcrInPerTree] <- 1

  othersPerShare <- calcOutput("LUH2MAgPIE", aggregate = FALSE,
                               share = "LUHofMAG")[, "y1995", "others"]
  noShare        <- which(round(dimSums(othersPerShare, dim = 3.1)) != 1)
  othersPerShare <- dimSums(othersPerShare[, , c("c3per", "c4per")], dim = 3.1)
  othersPerShare[noShare] <- 0.62 # default assumption 62% perennials in others
  # (as 62% of total others production is perennial)
  riceNonFlood      <- calcOutput("Ricearea", share = TRUE, aggregate = FALSE)[, "y1995", ]

  lu2kcrIso      <- magpie_expand(lu2kcr, riceNonFlood)
  lu2kcrIso[, , "others"][, , perTree]        <- othersPerShare
  lu2kcrIso[, , "others"][, , annCrop]        <- 1 - othersPerShare

  # Due to mismatch in LUH data rice share for non flooded is disabled and only paddy rice assumed # nolint
  # lu2kcrIso[, , "rice_pro"][, , annCrop]      <- riceNonFlood                                    # nolint
  # lu2kcrIso[, , "rice_pro"][, , "Paddy_rice"] <- 1 - riceNonFlood                                # nolint
  lu2kcrIso[, , "rice_pro"][, , annCrop]      <- 0
  lu2kcrIso[, , "rice_pro"][, , "Paddy_rice"] <- 1

  cShare <- collapseDim(dimSums(scfLu2019 * lu2kcrIso, dim = 3.2))
  weight <- collapseDim(calcOutput("Croparea", aggregate = FALSE, physical = TRUE)[, "y1995", ])
  weight[, , setdiff(kcr, kcrInMix)] <- 1

  return(list(x            = cShare,
              weight       = weight + 1e-10,
              min          = 0,
              unit         = "tC per tC",
              description  = "Stock change factors for first 30 cm of the soil profile"))
}
