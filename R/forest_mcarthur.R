#' McArthur Mk5 Forest Fire Danger Index
#'
#' @param drought_factor Drought factor (0-10).
#' @param rh Relative humidity (%).
#' @param temp Air temperature (degrees C).
#' @param wind_speed Wind speed at 10 m in the open (km/h).
#' @param wind_reduction Wind reduction factor from 10 m open to 2 m in
#'   forest (1-6, default 3).
#' @return Forest Fire Danger Index (FDI).
#' @references Noble, Bary & Gill (1980). McArthur's fire danger meters
#'   expressed as equations. \emph{Aust. J. Ecol.} 5:201-203.
#' @export
mcarthur_fdi <- function(drought_factor, rh, temp, wind_speed,
                         wind_reduction = 3) {
  wind_forest <- wind_speed * 3 / wind_reduction
  2 * exp(-0.45 + 0.987 * log(drought_factor) - 0.0345 * rh +
            0.0338 * temp + 0.0234 * wind_forest)
}

#' McArthur Mk5 Forest Fire Behaviour
#'
#' Calculates rate of spread, flame height, spotting distance, fireline
#' intensity and heat output based on McArthur Mk5.
#'
#' @param temp Air temperature (degrees C).
#' @param rh Relative humidity (%).
#' @param wind_speed Wind speed at 10 m in the open (km/h).
#' @param drought_factor Drought factor (0-10).
#' @param fuel_load Fine fuel load (t/ha).
#' @param slope Slope (degrees, default 0).
#' @param wind_reduction Wind reduction factor (1-6, default 3).
#' @return A list with fire behaviour outputs.
#' @export
forest_mcarthur_mk5 <- function(temp, rh, wind_speed, drought_factor,
                                fuel_load, slope = 0,
                                wind_reduction = 3) {
  fdi <- mcarthur_fdi(drought_factor, rh, temp, wind_speed, wind_reduction)
  fdi_nowind <- 2 * exp(-0.45 + 0.987 * log(drought_factor) -
                           0.0345 * rh + 0.0338 * temp + 0.0234 * 0)

  ros <- 0.0012 * fdi * fuel_load * exp(0.069 * slope) * 1000
  ros_flat <- 0.0012 * fdi * fuel_load * 1000
  flank_ros <- 0.0012 * fdi_nowind * fuel_load * 1000

  flame_ht <- 13 * ros / exp(0.069 * slope) / 1000 + 0.24 * fuel_load - 2

  ros_flat_km <- ros / exp(0.069 * slope) / 1000
  spot_dist <- (ros_flat_km * (4.17 - 0.033 * fuel_load) - 0.36) * 1000
  spot_dist <- pmax(spot_dist, 0)

  intensity <- 516.7 * fuel_load * drought_factor / 10 * ros / 1000
  heat_output <- 1860 * fuel_load * drought_factor / 10

  ffmc <- fuel_moisture_mcarthur(temp, rh)

  list(
    fdi           = fdi,
    ros           = ros,
    ros_flat      = ros_flat,
    flank_ros     = flank_ros,
    flame_height  = flame_ht,
    spotting_dist = spot_dist,
    intensity     = intensity,
    heat_output   = heat_output,
    ffmc          = ffmc
  )
}

#' McArthur Leaflet 80 Control Burning Guide
#'
#' Rate of spread and related outputs under low-intensity conditions.
#' Uses equations published by Gould (1994).
#'
#' @inheritParams forest_mcarthur_mk5
#' @param time_hour Hour of day (0-23). Used to select FMC equation.
#' @param kbdi Keetch-Byram Drought Index.
#' @param days_since_rain Days since last rain.
#' @param rain_amount Amount of last rain (mm).
#' @param use_bom_df Logical. If TRUE, use the provided drought factor
#'   directly for fuel availability.
#' @return A list with fire behaviour outputs.
#' @references Gould, J.S. (1994). Evaluation of McArthur's control burning
#'   guide in regrowth \emph{Eucalyptus sieberi} forest. \emph{Aust. Forestry}
#'   57, 86-93.
#' @export
forest_leaflet80 <- function(temp, rh, wind_speed, drought_factor,
                             fuel_load, slope = 0, wind_reduction = 3,
                             time_hour = 14, kbdi = 50,
                             days_since_rain = 5, rain_amount = 0,
                             use_bom_df = FALSE) {
  fmc <- if (time_hour < 12) {
    12.519 - 0.282 * temp + 0.112 * rh
  } else {
    6.783 - 0.17 * temp + 0.133 * rh
  }

  if (use_bom_df) {
    fuel_avail <- drought_factor / 10
  } else {
    fuel_avail <- min(0.972 + 0.342 * log(days_since_rain) -
                        0.245 * log(rain_amount), 1)
  }

  ros_flat <- 0.22 * fuel_load * exp(0.158 * (1.674 + 0.179 * wind_speed) -
                                       0.227 * fmc) * fuel_avail * 60
  ros_slope <- ros_flat * exp(0.0662 * slope)
  flank_ros <- 0.22 * fuel_load * exp(0.158 * 0 - 0.227 * fmc) *
    fuel_avail * 60

  flame_ht <- 0.3048 * 31.7537 *
    (1 - exp(-0.31772 * ros_slope / 18.3)) *
    (1 - exp(-(0.03644 * 1.1289^(ros_slope / 18.3)) *
               (fuel_avail * fuel_load / 2.2417)))

  scorch_ht <- if (temp < 20) {
    -2.19 + 2.23 * sqrt(ros_flat)
  } else {
    -0.296 + 2.23 * sqrt(ros_flat)
  }

  intensity <- 18000 * fuel_load * fuel_avail * ros_slope / 36000

  out_of_range <- (temp > 35 | temp < 5 | rh > 70 | rh < 20)

  list(
    fmc             = fmc,
    fuel_avail      = fuel_avail,
    ros_flat        = ros_flat,
    ros_slope       = ros_slope,
    flank_ros       = flank_ros,
    flame_height    = flame_ht,
    scorch_height   = scorch_ht,
    intensity       = intensity,
    out_of_range    = out_of_range
  )
}

#' McArthur FDI using Matthews fuel moisture method
#'
#' @param drought_factor Drought factor (0-10).
#' @param wind_speed Wind speed at 10 m in the open (km/h).
#' @param ffmc Fine fuel moisture content (%).
#' @return FDI value.
#' @export
mcarthur_fdi_matthews <- function(drought_factor, wind_speed, ffmc) {
  34.8 * drought_factor * exp(0.0234 * wind_speed) * ffmc^(-2.1)
}
