#' Calculate dew point from temperature and relative humidity
#'
#' @param temp Air temperature (degrees C).
#' @param rh Relative humidity (%).
#' @return Dew point temperature (degrees C).
#' @references Magnus formula with coefficients a = 17.2694, b = 237.7.
#' @export
dew_point <- function(temp, rh) {
  a <- 17.2694
  b <- 237.7
  gamma <- a * temp / (b + temp) + log(rh / 100)
  b * gamma / (a - gamma)
}

#' Calculate relative humidity from temperature and dew point
#'
#' @param temp Air temperature (degrees C).
#' @param dew_pt Dew point temperature (degrees C).
#' @return Relative humidity (%).
#' @export
rh_from_dewpoint <- function(temp, dew_pt) {
  100 * (((112 - 0.1 * temp + dew_pt)) / (112 + 0.9 * temp))^8
}

#' Calculate RH and dew point from dry and wet bulb temperatures
#'
#' Uses the psychrometric equation at standard pressure (101.3 kPa).
#'
#' @param temp Dry bulb temperature (degrees C).
#' @param wet_bulb Wet bulb temperature (degrees C).
#' @return A list with components \code{rh} (%) and \code{dew_pt} (degrees C).
#' @export
rh_from_wetbulb <- function(temp, wet_bulb) {
  es <- exp((16.78 * temp - 116.9) / (temp + 237.3))
  e  <- exp((16.78 * wet_bulb - 116.9) / (wet_bulb + 237.3)) -
    (0.00066 * (1 + 0.00115 * wet_bulb)) * 101.3 * (temp - wet_bulb)
  rh <- 100 * e / es
  dp <- dew_point(temp, rh)
  list(rh = rh, dew_pt = dp)
}

#' Convert wind direction text to degrees
#'
#' Converts compass bearing abbreviations (N, NNE, NE, etc.) to degrees.
#' Numeric values are returned unchanged.
#'
#' @param wind_dir Character or numeric wind direction.
#' @return Numeric wind direction in degrees from true north.
#' @export
wind_dir_to_degrees <- function(wind_dir) {
  lookup <- c(
    N = 360, NNE = 22.5, NE = 45, ENE = 67.5,
    E = 90, ESE = 112.5, SE = 135, SSE = 157.5,
    S = 180, SSW = 202.5, SW = 225, WSW = 247.5,
    W = 270, WNW = 292.5, NW = 315, NNW = 337.5
  )
  vapply(wind_dir, function(wd) {
    if (is.numeric(wd)) return(wd)
    wd_upper <- toupper(trimws(as.character(wd)))
    val <- suppressWarnings(as.numeric(wd_upper))
    if (!is.na(val)) return(val)
    match_val <- lookup[wd_upper]
    if (is.na(match_val)) return(NA_real_)
    unname(match_val)
  }, numeric(1), USE.NAMES = FALSE)
}

#' Calculate the general direction of fire spread
#'
#' Fire spreads in the opposite direction to the wind.
#'
#' @param wind_dir_deg Wind direction in degrees from true north.
#' @return Direction of fire spread in degrees.
#' @export
fire_spread_direction <- function(wind_dir_deg) {
  ifelse(wind_dir_deg < 180, wind_dir_deg + 180, wind_dir_deg - 180)
}

#' Estimate surface fine fuel moisture content (McArthur)
#'
#' @param temp Air temperature (degrees C).
#' @param rh Relative humidity (%).
#' @return Fine fuel moisture content (%).
#' @export
fuel_moisture_mcarthur <- function(temp, rh) {
  3.033808 * exp(rh * (-0.00033169 * temp + 0.02638614))
}
