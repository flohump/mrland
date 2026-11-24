#' @title calcSoilStockChangeFactors
#' @description calculates and merges information on stock change factors
#'
#' @seealso
#' [readIPCC()]
#' [readIrrSCF()]
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

  # stock change factors are dimensionless ratios (carbon after / before)
  unit <- "tC per tC"

  if (factor %in% c("tillage", "input")) {

    subtype <- ifelse(factor == "tillage", "SCF_MG2019", "SCF_I2019")
    # The converted IPCC source is already a climate x factor table: its cells
    # are the IPCC climate classes and its items are the management factors.
    out <- readSource("IPCC", subtype = subtype, convert = FALSE)

    weight      <- NULL
    description <- ifelse(factor == "tillage",
                          "Soil carbon stock change factors for tillage (IPCC 2019, Table 5.5)", 
                          "Soil carbon stock change factors for input management (IPCC 2019, Table 5.5)")

  } else if (factor == "fallow") {

    # Fallow land has no dedicated IPCC column, so its stock change factor is
    # derived from the land-use (F_LU), tillage (F_MG) and input (F_I) factors.
    # Each source is a smartmag: a single GLO cell whose items are
    # "<climate class>.<category>", so values are selected by the combined name.
    lu  <- readSource("IPCC", subtype = "SCF_LU2019", convert = FALSE)  # F_LU
    mg  <- readSource("IPCC", subtype = "SCF_MG2019", convert = FALSE)  # F_MG (tillage)
    inp <- readSource("IPCC", subtype = "SCF_I2019", convert = FALSE)   # F_I  (input)
    classes <- getNames(lu, dim = 1)

    # Assumption for fallow management (IPCC 2019, Table 5.5):
    #   - dry regions   -> bare fallow: long-term cultivated cropland combined
    #                      with full tillage and low input management
    #   - moist regions -> vegetated fallow: the set-aside land-use factor
    # The moisture regime is encoded in the climate class name (*_dry / *_moist).
    dry <- grepl("_dry$", classes)
    annCrop  <- "Long_term_cultivated"
    setAside <- "Set_aside_under20yrs"
    out          <- collapseNames(lu[, , setAside])
    out[, , dry] <- (lu[, , annCrop] * mg[, , "full_tillage"] * inp[, , "low_input"])[, , dry]

    weight      <- NULL
    description <- paste("Soil carbon stock change factor for fallow land",
                         "(IPCC 2019, Table 5.5; bare fallow under full",
                         "tillage and low input in dry regions, vegetated",
                         "set-aside in moist regions)")

  } else if (factor == "irrigation") {

    # Irrigation stock change factor from Trost et al. 2013: relative soil carbon
    # under irrigation vs rainfed, given per aridity class (arid/semi_arid/humid).
    trost <- readSource("IrrSCF", subtype = "Trost", convert = FALSE)

    # Map the Trost aridity classes onto the six IPCC climate classes with
    # toolAggregate (incidence matrix: rows = Trost classes, columns = climate
    # classes). Dry classes take the semi-arid factor (arid and semi_arid are
    # both 1.17), moist classes the humid factor (1.02).
    climate <- c("temperate_dry", "temperate_moist", "tropical_dry",
                 "tropical_moist", "boreal_dry", "boreal_moist")
    rel     <- matrix(0, nrow = 3, ncol = 6,
                      dimnames = list(c("arid", "semi_arid", "humid"), climate))
    rel["semi_arid", c("temperate_dry", "tropical_dry", "boreal_dry")]       <- 1
    rel["humid",     c("temperate_moist", "tropical_moist", "boreal_moist")] <- 1
    cf      <- toolAggregate(trost, rel = rel, dim = 3)

    # Default factor 1 (no irrigation effect); only the irrigated crops take the
    # mapped Trost factor. w is not a MAgPIE set, so the two water modes are
    # hard-coded. The data dimension carries the names in climate.w.kcr order.
    w   <- c("rainfed", "irrigated")
    kcr <- findset("kcr")
    nm  <- do.call(paste, c(expand.grid(climate, w, kcr, stringsAsFactors = FALSE),
                            list(sep = ".")))
    out <- new.magpie("GLO", NULL, names = nm, fill = 1)
    for (clim in climate) {
      out[, , paste(clim, "irrigated", kcr, sep = ".")] <- as.numeric(cf[, , clim])
    }
    # higher soil carbon of paddy rice is handled via the land use factor F_LU
    out[, , paste(climate, "irrigated", "rice_pro", sep = ".")] <- 1

    weight      <- NULL           
    description <- paste("Soil carbon stock change factors for irrigation",
                                    "(Trost et al. 2013, Agronomy for Sustainable",
                                    "Development 33, 733-749)")

  } else if (factor == "landuse") {

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

    out    <- collapseDim(dimSums(scfLu2019 * lu2kcrIso, dim = 3.2))
    weight <- collapseDim(calcOutput("Croparea", aggregate = FALSE, physical = TRUE)[, "y1995", ])
    weight[, , setdiff(kcr, kcrInMix)] <- 1
    weight <- weight + 1e-10
    description <- "Stock change factors for first 30 cm of the soil profile"

  } else {
    stop("Unknown factor type specified!")
  }

  return(list(x            = out,
              weight       = weight,
              min          = 0,
              unit         = unit,
              description  = description))
}
