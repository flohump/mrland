#' @title calcPlantationAgeClass
#' @description
#' Age structure of the planted forest estate in 1995 as area shares by 5-year age class, from the FAO 2006
#' planted-forest survey (Table 7, 2005, taken as 1995; reported age bins spread evenly over their classes).
#' Plantations (`pool = "plantation"`): the plantation categories; countries without data get zero shares, so
#' regional shares sum to the reporting share of the estate. Weight: plantation area 1995.
#' Other planted forest (`pool = "other_planted"`, FRA planted forest that is not plantation forest): the
#' semi-natural categories plus the plantation categories for the reported plantation area above the FRA 2025
#' plantation forest; a country reports when this area is at least a tenth of its other planted area. Countries
#' with the national survey of planted forest by age class (source ForestInventoryAge, subtype JPN) take the
#' earliest survey, moved back to 1990 and split over the two neighbouring whole numbers of classes; classes
#' planted after 1990 drop out. Countries without data get zero shares and zero weight, so a region without
#' reporters is zero. Weight: other planted area 1995 of the reporters.
#' @param pool "plantation", "other_planted" or both (one item per pool)
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Florian Humpenoeder
#' @seealso [readFAOPlantedForests2006()], [readForestInventoryAge()]
#' @examples
#' \dontrun{
#' calcOutput("PlantationAgeClass", aggregate = TRUE)
#' }
calcPlantationAgeClass <- function(pool = c("plantation", "other_planted")) {
  if (length(pool) > 1) {
    parts <- lapply(pool, function(poolName) {
      calcOutput("PlantationAgeClass", pool = poolName, aggregate = FALSE, supplementary = TRUE)
    })
    bind <- function(what) {
      return(mbind(lapply(seq_along(parts), function(index) {
        add_dimension(parts[[index]][[what]], dim = 3.1, add = "pool", nm = pool[index])
      })))
    }
    return(list(x = bind("x"),
                weight = bind("weight"),
                unit = "share",
                description = "Age structure of the planted forest estates in 1995 by pool (see the pools)"))
  }
  obs <- readSource("FAOPlantedForests2006")
  plant <- dimSums(obs[, , c("productive_plantation", "protective_plantation")], dim = 3.1)
  fraArea <- readSource("FRA2025", "forest_area")
  fraArea1995 <- setYears(time_interpolate(fraArea, 1995), NULL)
  plantation95 <- collapseNames(fraArea1995[, , "plantationForest"])

  if (pool == "plantation") {
    x <- plant
    weight <- plantation95
    hasSurvey <- FALSE
  } else if (pool == "other_planted") {
    semi <- dimSums(obs[, , c("productive_semi_natural", "protective_semi_natural")], dim = 3.1)
    excess <- dimSums(plant, dim = 3) - 1000 * plantation95                           # 1000 ha
    excess[excess < 0] <- 0
    x <- semi + plant / (dimSums(plant, dim = 3) + 1e-10) * excess
    weight <- 1000 * (collapseNames(fraArea1995[, , "plantedForest"]) - plantation95)  # 1000 ha
    weight[weight < 0] <- 0
    survey <- collapseNames(readSource("ForestInventoryAge", "JPN")[, , "planted_area"])
    hasSurvey <- as.vector(dimSums(survey, dim = c(2, 3)) > 0 & weight > 0)
    noObs <- as.vector(weight <= 0 | dimSums(x, dim = 3) < 0.1 * weight) & !hasSurvey
    x[noObs, , ] <- 0
    weight[noObs, , ] <- 0
  } else {
    stop("unknown pool: ", pool)
  }

  binClasses <- list(a0_5 = "ac0", a5_10 = "ac5", a10_20 = c("ac10", "ac15"), a20_30 = c("ac20", "ac25"),
                     a30_40 = c("ac30", "ac35"), a40_50 = c("ac40", "ac45"), a50_70 = paste0("ac", seq(50, 65, 5)),
                     a70_90 = paste0("ac", seq(70, 85, 5)), a90_100 = c("ac90", "ac95"), a100plus = "acx")
  out <- new.magpie(getItems(x, 1), NULL, toolForestAgeClasses(), fill = 0)
  for (bin in names(binClasses)) {
    out[, , binClasses[[bin]]] <- x[, , bin] / length(binClasses[[bin]])
  }

  for (iso in getItems(out, 1)[hasSurvey]) {
    year <- getYears(survey)[as.vector(dimSums(survey[iso, , ], dim = 3) > 0)][1]
    area <- as.vector(survey[iso, year, ])
    upperAge <- as.numeric(sub("^a([0-9]+)_.*$", "\\1", getItems(survey, 3))) + 4
    stopifnot(upperAge %% 5 == 0)
    shift <- (as.numeric(sub("y", "", year)) - 1990) / 5
    out[iso, , ] <- 0
    for (classes in unique(c(floor(shift), ceiling(shift)))) {
      age <- upperAge - 5 * classes
      targetClasses <- paste0("ac", age[age >= 5])
      out[iso, , targetClasses] <- out[iso, , targetClasses] + (1 - abs(shift - classes)) * area[age >= 5]
    }
  }
  out <- out / (dimSums(out, dim = 3) + 1e-10)

  return(list(x = out,
              weight = weight + 1e-10,
              unit = "share",
              description = if (pool == "plantation") {
                "Age structure of the plantation forest estate in 1995 (FAO 2006 survey, zero where not reported)"
              } else {
                paste("Age structure of the other planted forest estate in 1995 (FAO 2006 survey, national survey",
                      "moved back to 1990 where available, zero where not reported)")
              }))
}
