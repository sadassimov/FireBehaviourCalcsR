#' CSIRO Grassland Fire Danger Index
#'
#' @param curing Degree of grass curing (%).
#' @param temp Air temperature (degrees C).
#' @param wind_speed Wind speed at 10 m (km/h).
#' @param rh Relative humidity (%).
#' @return Grassland FDI.
#' @references Purton (1982) BoM Meteorological Note No.147.
#' @export
grass_fdi <- function(curing, temp, wind_speed, rh) {
  10^(0.009254 - 0.004096 * (100 - curing)^1.536 +
        0.01201 * temp + 0.2789 * wind_speed^0.5 -
        0.09577 * rh^0.5)
}

#' CSIRO Grassland Fire Behaviour
#'
#' Implements the CSIRO grassland fire spread model with Cruz et al. (2015)
#' curing function. Five sub-models: natural, grazed, eaten-out, open
#' woodland, and woodland.
#'
#' @param temp Air temperature (degrees C).
#' @param rh Relative humidity (%).
#' @param wind_speed Wind speed at 10 m (km/h).
#' @param curing Degree of grass curing (%).
#' @param slope Slope (degrees, default 0).
#' @return A list with fire behaviour outputs for all five grass types.
#' @references Cheney, Gould & Catchpole (1998). Prediction of fire spread
#'   in grasslands. \emph{Int. J. Wildland Fire} 8(1):1-13.
#'
#'   Cruz et al. (2015). Updated curing function.
#' @export
grass_fire <- function(temp, rh, wind_speed, curing, slope = 0) {
  fdi <- grass_fdi(curing, temp, wind_speed, rh)
  dead_fmc <- -0.19625 * temp + 0.1356 * rh + 9.575

  mc_below12 <- exp(-0.108 * dead_fmc)
  mc_above12_low <- pmax(0.001, 0.684 - 0.0342 * dead_fmc)
  mc_above12_high <- pmax(0.001, 0.547 - 0.0228 * dead_fmc)

  mc_coeff <- ifelse(dead_fmc < 12, mc_below12,
                     ifelse(wind_speed > 10, mc_above12_high, mc_above12_low))

  cure_coeff <- 1.036 / (1 + 103.989 * exp(-0.0996 * (curing - 20)))

  slope_factor <- 1.0715^slope

  # --- Natural grass ---
  nat_ros_low  <- (0.054 + 0.269 * wind_speed) * mc_coeff * cure_coeff
  nat_ros_high <- (1.4 + 0.838 * (wind_speed - 5)^0.844) * mc_coeff * cure_coeff
  nat_ros_km   <- ifelse(wind_speed < 5, nat_ros_low, nat_ros_high)
  nat_ros      <- nat_ros_km * 1000
  nat_ros_slope <- nat_ros * slope_factor
  nat_flank    <- (0.054 + 0.269 * 5) * mc_coeff * cure_coeff * 1000
  nat_flame    <- (0.003595 * 55.816 + 101.04 * (nat_ros_slope / 1000)^0.3167) /
    (55.816 + (nat_ros_slope / 1000)^0.3167)
  nat_intensity <- 516.7 * nat_ros_slope * 6 / 1000

  # --- Grazed grass ---
  graz_ros_low  <- (0.054 + 0.209 * wind_speed) * mc_coeff * cure_coeff
  graz_ros_high <- (1.1 + 0.715 * (wind_speed - 5)^0.844) * mc_coeff * cure_coeff
  graz_ros_km   <- ifelse(wind_speed < 5, graz_ros_low, graz_ros_high)
  graz_ros      <- graz_ros_km * 1000
  graz_ros_slope <- graz_ros * slope_factor
  graz_flank    <- (0.054 + 0.209 * 5) * mc_coeff * cure_coeff * 1000
  graz_flame    <- (0.001316 * 149.4025 + 117.22 * (graz_ros_slope / 1000)^0.3141) /
    (149.4025 + (graz_ros_slope / 1000)^0.3141)
  graz_intensity <- 516.7 * graz_ros_slope * 4 / 1000

  # --- Eaten-out grass ---
  eat_ros_high <- (0.55 + 0.357 * (wind_speed - 5)^0.844) * mc_coeff * cure_coeff
  eat_ros      <- ifelse(wind_speed < 5, 0, eat_ros_high * 1000)
  eat_ros_slope <- eat_ros * slope_factor
  eat_flank    <- (0.55 + 0.357 * (5 - 5)^0.844) * mc_coeff * cure_coeff * 1000
  eat_flame    <- 1 / (32.915 - 29.388 * (eat_ros_slope / 1000)^0.02969)
  eat_intensity <- 516.7 * eat_ros_slope * 1.5 / 1000

  # --- Open woodland grass ---
  ow_ros      <- ifelse(wind_speed < 5, graz_ros_low * 0.7, graz_ros_high * 0.7) * 1000
  ow_ros_slope <- ow_ros * slope_factor
  ow_flank    <- graz_flank
  ow_flame    <- (0.001316 * 149.4025 + 117.22 * (ow_ros_slope / 1000)^0.3141) /
    (149.4025 + (ow_ros_slope / 1000)^0.3141)
  ow_intensity <- 516.7 * ow_ros_slope * 4 / 1000

  # --- Woodland grass ---
  wl_ros      <- ifelse(wind_speed < 5, graz_ros_low * 0.5, graz_ros_high * 0.5) * 1000
  wl_ros_slope <- wl_ros * slope_factor
  wl_flank    <- graz_flank
  wl_flame    <- (0.001316 * 149.4025 + 117.22 * (wl_ros_slope / 1000)^0.3141) /
    (149.4025 + (wl_ros_slope / 1000)^0.3141)
  wl_intensity <- 516.7 * wl_ros_slope * 4 / 1000

  list(
    fdi           = fdi,
    dead_fmc      = dead_fmc,
    mc_coeff      = mc_coeff,
    cure_coeff    = cure_coeff,
    natural = list(
      ros = nat_ros_slope, flank_ros = nat_flank,
      flame_height = nat_flame, intensity = nat_intensity
    ),
    grazed = list(
      ros = graz_ros_slope, flank_ros = graz_flank,
      flame_height = graz_flame, intensity = graz_intensity
    ),
    eaten_out = list(
      ros = eat_ros_slope, flank_ros = eat_flank,
      flame_height = eat_flame, intensity = eat_intensity
    ),
    open_woodland = list(
      ros = ow_ros_slope, flank_ros = ow_flank,
      flame_height = ow_flame, intensity = ow_intensity
    ),
    woodland = list(
      ros = wl_ros_slope, flank_ros = wl_flank,
      flame_height = wl_flame, intensity = wl_intensity
    )
  )
}
