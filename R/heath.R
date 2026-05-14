#' Heathland Fire Behaviour (Anderson et al. 2015)
#'
#' @param temp Air temperature (degrees C).
#' @param rh Relative humidity (%).
#' @param wind_speed Wind speed at 10 m in the open (km/h).
#' @param veg_height Vegetation top height (m).
#' @param slope Slope (degrees, default 0).
#' @param overcast Logical. If TRUE, delta correction for overcast sky is
#'   disabled (default FALSE).
#' @param hour Hour of day (0-23, default 14). Used for delta correction.
#' @param month Month (1-12, default 2). Used for delta correction.
#' @return A list with heath and woodland fire behaviour outputs.
#' @references Anderson et al. (2015). A generic, empirical-based model for
#'   predicting rate of fire spread in shrublands. \emph{Int. J. Wildland Fire}.
#'
#'   Catchpole et al. (1998). Cooperative development of equations for
#'   heathland fire behaviour.
#' @export
heath_fire <- function(temp, rh, wind_speed, veg_height,
                       slope = 0, overcast = FALSE,
                       hour = 14, month = 2) {
  # Wind reduction: 10 m open -> 2 m in heath = 1 / 0.67
  wrf_heath <- 1 / 0.67
  # Woodland wind reduction = 0.35 / 0.67
  wrf_woodland <- 0.35 / 0.67

  r0 <- 5  # zero-wind ROS (m/min)

  # Delta correction: suppressed during summer nights or under overcast
  is_summer_night <- (hour < 12 | hour > 17)
  is_summer_month <- month >= 3 & month <= 10
  delta <- ifelse(overcast | is_summer_night | is_summer_month, 0, 1)

  dead_fmc <- 4.37 + 0.161 * rh - 0.1 * (temp - 25) - delta * 0.027 * rh

  # Heath ROS
  wind_coeff <- 5.67 * (wind_speed / wrf_heath)^0.91
  heath_ros <- ifelse(
    wind_speed < 5,
    (r0 + 0.2 * (5.67 * (5 / wrf_heath)^0.91 - r0) * wind_speed) *
      veg_height^0.22 * exp(-0.076 * dead_fmc),
    5.67 * (wind_speed / wrf_heath)^0.91 *
      veg_height^0.22 * exp(-0.076 * dead_fmc)
  ) * 60

  heath_intensity <- 516 * heath_ros / 1000 * 7.53 * veg_height

  # Woodland ROS
  woodland_ros <- ifelse(
    wind_speed < 5,
    (r0 + 0.2 * (5.67 * (5 * wrf_woodland / wrf_heath)^0.91 - r0) *
       wind_speed) * veg_height^0.22 * exp(-0.076 * dead_fmc),
    5.67 * (wind_speed * wrf_woodland / wrf_heath)^0.91 *
      veg_height^0.22 * exp(-0.076 * dead_fmc)
  ) * 60

  list(
    dead_fmc         = dead_fmc,
    heath_ros        = heath_ros,
    heath_intensity  = heath_intensity,
    woodland_ros     = woodland_ros
  )
}
