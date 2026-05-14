#' WA Mallee Fire Spread (McCaw 1998)
#'
#' Model developed for mature (24-year-old) mallee with relatively high
#' fuel continuity.
#'
#' @param wind_speed Wind speed at 10 m (km/h).
#' @param dead_fmc Dead fuel moisture content (%).
#' @param slope Slope (degrees, default 0).
#' @return A list with rate of spread (m/h).
#' @references McCaw, W.L. (1998). Fire spread in WA mallee.
#' @export
mallee_fire <- function(wind_speed, dead_fmc, slope = 0) {
  ros <- 274 * (wind_speed / 1.7)^1.05 * exp(-0.11 * dead_fmc)
  ros_slope <- ros * exp(0.069 * slope)
  list(ros = ros, ros_slope = ros_slope)
}

#' Mallee-Heath Fire Behaviour (Cruz et al. 2013)
#'
#' Combines WA and SA experimental data for mallee-heath shrublands.
#'
#' @param temp Air temperature (degrees C).
#' @param rh Relative humidity (%).
#' @param wind_speed Wind speed at 10 m (km/h).
#' @param wind_speed_2m Wind speed at 2 m (km/h, optional). If NULL, only
#'   the 10 m wind model is used.
#' @param surface_fmc Surface fine fuel moisture content (%).
#' @param overstorey_cover Overstorey cover (%).
#' @param overstorey_height Overstorey height (m).
#' @param surface_fuel_load Combined surface fine fuel load (t/ha).
#' @param slope Slope (degrees, default 0).
#' @return A list with fire behaviour outputs.
#' @references Cruz, M.G., McCaw, W.L. et al. (2013). \emph{Environmental
#'   Modelling & Software} 40:21-34.
#' @export
mallee_heath_fire <- function(temp, rh, wind_speed,
                              wind_speed_2m = NULL,
                              surface_fmc,
                              overstorey_cover,
                              overstorey_height,
                              surface_fuel_load,
                              slope = 0) {
  # Probability of sustained fire (10 m wind)
  prob_go_10m <- 1 / (1 + exp(-(14.626 + 0.2066 * wind_speed -
                                   1.8789 * surface_fmc -
                                   30.442 * overstorey_cover / 100)))

  # Probability of sustained fire (2 m wind)
  prob_go_2m <- if (!is.null(wind_speed_2m) && wind_speed_2m > 0) {
    1 / (1 + exp(-(16.626 + 0.7536 * wind_speed_2m -
                     2.2569 * surface_fmc -
                     34.106 * overstorey_cover / 100)))
  } else {
    0
  }

  # Probability of crown fire
  prob_crown <- 1 / (1 + exp(-(-11.138 + 1.4054 * wind_speed -
                                  3.4217 * surface_fmc)))

  # Surface ROS (10 m wind)
  surface_ros_10m <- (3.337 + wind_speed * exp(-0.1284 * surface_fmc) -
                        0.7073 * overstorey_height) * 60

  # Surface ROS (2 m wind)
  surface_ros_2m <- if (!is.null(wind_speed_2m) && wind_speed_2m > 0) {
    (4.0276 + wind_speed_2m * exp(-0.1246 * surface_fmc)) * 60
  } else {
    0
  }

  # Crown fire ROS
  crown_ros <- ifelse(prob_crown > 0.4,
    (9.5751 + wind_speed * exp(-0.1795 * surface_fmc) +
       0.3589 * overstorey_cover) * 60,
    0)

  # Overall ROS
  ros <- ifelse(prob_crown > 0.49, crown_ros,
                pmax(surface_ros_2m, surface_ros_10m))

  # Fireline intensity
  intensity <- 18600 * surface_fuel_load / 10 * ros / 3600

  # Flame height
  flame_ht <- 0.01589 * intensity^0.633

  list(
    prob_go_2m       = prob_go_2m,
    prob_go_10m      = prob_go_10m,
    prob_crown       = prob_crown,
    surface_ros_2m   = surface_ros_2m,
    surface_ros_10m  = surface_ros_10m,
    crown_ros        = crown_ros,
    ros              = ros,
    intensity        = intensity,
    flame_height     = flame_ht
  )
}
