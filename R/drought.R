#' Calculate drought factor
#'
#' Drought factor based on KBDI/SDI, days since rain, and rainfall amount.
#' Capped at 10.
#'
#' @param kbdi Keetch-Byram Drought Index or Soil Dryness Index.
#' @param days_since_rain Number of days since last rain.
#' @param rain_amount Amount of rain (mm).
#' @return Drought factor (0-10).
#' @export
drought_factor <- function(kbdi, days_since_rain, rain_amount) {
  df <- 0.191 * (kbdi + 104) * (days_since_rain + 1)^1.5 /
    (3.52 * (days_since_rain + 1)^1.5 + rain_amount - 1)
  pmin(df, 10)
}
