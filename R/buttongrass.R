#' Buttongrass Fuel Load
#'
#' @param age Buttongrass age since last fire (years, default 20).
#' @param cover Foliage projective cover (%, 0 if unknown).
#' @param productivity Site productivity code (1 = low, 2 = medium).
#' @return Fuel load (t/ha).
#' @export
buttongrass_fuel_load <- function(age = 20, cover = 0, productivity = 2) {
  a <- ifelse(age == 0, 20, age)
  if (cover > 0) {
    ifelse(productivity == 1,
           0.629 * (a * cover)^0.38,
           0.305 * (a * cover)^0.58)
  } else {
    ifelse(productivity == 1,
           11.73 * (1 - exp(-0.106 * a)),
           44.61 * (1 - exp(-0.041 * a)))
  }
}

#' Buttongrass Fire Behaviour (Marsden-Smedley 2003)
#'
#' @param temp Air temperature (degrees C).
#' @param rh Relative humidity (%).
#' @param wind_speed Wind speed at 10 m (km/h).
#' @param rain_amount Amount of recent rain (mm, default 0).
#' @param hours_since_rain Hours since rain stopped (default 0).
#' @param age Buttongrass age (years, default 20).
#' @param cover Fuel cover (%, 0 if unknown).
#' @param productivity Site productivity (1 = low, 2 = medium, default 2).
#' @param slope Slope (degrees, default 0).
#' @return A list with fire behaviour outputs.
#' @references Marsden-Smedley, J.B. (2003). Buttongrass moorland fire
#'   behaviour model.
#' @export
buttongrass_fire <- function(temp, rh, wind_speed,
                             rain_amount = 0, hours_since_rain = 0,
                             age = 20, cover = 0, productivity = 2,
                             slope = 0) {
  vapour_pressure <- 0.611 * exp(17.2694 * temp / (temp + 237.3)) * rh / 100
  dew_pt <- (1 / 273.16 - 0.000184 * log(vapour_pressure / 0.611))^-1 -
    273.16

  fuel_moisture <- exp(1.66 + 0.0214 * rh - 0.0292 * dew_pt) +
    67.128 * (1 - exp(-3.132 * rain_amount)) *
    exp(-0.0858 * hours_since_rain)

  fuel_load <- buttongrass_fuel_load(age, cover, productivity)
  eff_age <- ifelse(age == 0, 20, age)

  # Wind speed at 1.7 m (reduce 10 m by 1/3)
  wind_17 <- wind_speed * 0.66
  wind_17 <- pmax(wind_17, 0.5)

  head_ros <- 0.678 * wind_17^1.313 * exp(-0.0243 * fuel_moisture) *
    (1 - exp(-0.116 * eff_age)) * exp(0.0693 * slope) * 60

  head_flame <- 0.148 * (((18637 - 24 * fuel_moisture) *
                             head_ros / 60 * fuel_load) / 600)^0.403

  flank_ros   <- head_ros * 0.4
  flank_flame <- head_flame * 0.6
  back_ros    <- head_ros * 0.1
  back_flame  <- head_flame * 0.5

  danger_rating <- 0.65 * (head_ros / 60)^1.02

  # Probability of fire sustaining
  rain_cat <- ifelse(hours_since_rain == 24, 1, 2)
  logit <- -1 + 0.68 * wind_17 - 0.07 * fuel_moisture -
    0.0037 * wind_17 * fuel_moisture + 2.1 * rain_cat
  prob_sustain <- 1 / (1 + exp(-logit))

  list(
    dew_point       = dew_pt,
    fuel_moisture   = fuel_moisture,
    fuel_load       = fuel_load,
    head_ros        = head_ros,
    head_flame      = head_flame,
    flank_ros       = flank_ros,
    flank_flame     = flank_flame,
    back_ros        = back_ros,
    back_flame      = back_flame,
    danger_rating   = danger_rating,
    prob_sustain    = prob_sustain
  )
}
