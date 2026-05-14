#' Convert VESTA fuel hazard rating to numeric score
#'
#' @param rating Character rating: "l", "m", "h", "vh", or "e" (case
#'   insensitive). Numeric values (0-4) are returned unchanged.
#' @return Numeric hazard score.
#' @export
vesta_hazard_score <- function(rating) {
  if (is.numeric(rating)) return(rating)
  r <- tolower(trimws(rating))
  switch(r, l = 1, m = 2, h = 3, vh = 3.5, e = 4, 0)
}

#' Convert VESTA hazard score to equivalent fuel load
#'
#' @param score Numeric hazard score (0-4).
#' @param fuel_type One of "surface", "near_surface", "elevated", or "bark".
#' @return Equivalent fuel load (t/ha).
#' @export
vesta_fuel_load <- function(score, fuel_type) {
  lookup <- switch(fuel_type,
    surface      = c(`0` = 0, `1` = 4, `2` = 8, `3` = 12, `3.5` = 14, `4` = 20),
    near_surface = c(`0` = 0, `1` = 1, `2` = 2, `3` = 3, `3.5` = 3.5, `4` = 4),
    elevated     = c(`0` = 0, `1` = 1, `2` = 2, `3` = 3, `3.5` = 4, `4` = 6),
    bark         = c(`0` = 0, `1` = 1, `2` = 2, `3` = 5, `3.5` = 6, `4` = 7),
    stop("fuel_type must be one of: surface, near_surface, elevated, bark")
  )
  unname(lookup[as.character(score)])
}

#' VESTA fine fuel moisture content
#'
#' Three models for different times of day.
#'
#' @param temp Air temperature (degrees C).
#' @param rh Relative humidity (%).
#' @param table Which moisture table to use: "summer_pm" (Table M1, summer
#'   13:00-17:00), "daytime" (Table M2), or "night" (Table M3).
#' @return Fine fuel moisture content (%).
#' @references Equations derived by Kevin Tolhurst based on VESTA field
#'   guide (2007).
#' @export
vesta_fuel_moisture <- function(temp, rh, table = c("summer_pm", "daytime",
                                                     "night")) {
  table <- match.arg(table)
  switch(table,
    summer_pm = {
      num <- (1.6779 * temp^0.09655) * (196.1 * temp^0.3204) +
        (18.944 * 317433^(1 / temp)) * rh^(1.5002 * 0.0196^(1 / temp))
      den <- (196.1 * temp^0.3204) + rh^(1.5002 * 0.0196^(1 / temp))
      num / den
    },
    daytime = {
      num <- (2.143 + 0.0322 * temp - 0.0006135 * temp^2) *
        (663.6 + 17.8 * temp) +
        (193 - 1.366 * temp) * rh^(0.9367 + 0.00487 * temp)
      den <- (663.6 + 17.8 * temp) + rh^(0.9367 + 0.00487 * temp)
      num / den
    },
    night = {
      (2.943 - 0.0415 * temp) + (0.196 * 1.2256^(1 / temp)) * rh
    }
  )
}

#' Determine which VESTA moisture table to use
#'
#' @param month Month (1-12).
#' @param hour Hour of day (0-23).
#' @return Character string: "summer_pm", "daytime", or "night".
#' @export
vesta_moisture_table <- function(month, hour) {
  is_summer <- month > 10 | month < 3
  ifelse(is_summer & hour >= 13 & hour <= 17, "summer_pm",
         ifelse(hour < 6 | hour >= 20, "night", "daytime"))
}

#' VESTA Forest Fire Behaviour
#'
#' Rate of spread model for dry eucalypt forest.
#'
#' @param temp Air temperature (degrees C).
#' @param rh Relative humidity (%).
#' @param wind_speed Wind speed at 10 m in the open (km/h).
#' @param slope Slope (degrees).
#' @param surface_score Surface fuel hazard score (0-4).
#' @param near_surface_score Near-surface fuel hazard score (0-4).
#' @param near_surface_height Near-surface fuel height (cm).
#' @param elevated_score Elevated fuel hazard score (0-4).
#' @param elevated_height Elevated fuel height (m).
#' @param bark_score Bark fuel hazard score (0-4).
#' @param month Month (1-12, default 1).
#' @param hour Hour of day (0-23, default 14).
#' @return A list with fire behaviour outputs.
#' @references Cheney et al. (2007) VESTA fire behaviour model.
#' @export
forest_vesta <- function(temp, rh, wind_speed, slope,
                         surface_score, near_surface_score,
                         near_surface_height, elevated_score,
                         elevated_height, bark_score,
                         month = 1, hour = 14) {
  surface_fl  <- vesta_fuel_load(surface_score, "surface")
  ns_fl       <- vesta_fuel_load(near_surface_score, "near_surface")
  elev_fl     <- vesta_fuel_load(elevated_score, "elevated")
  bark_fl     <- vesta_fuel_load(bark_score, "bark")
  total_fl    <- surface_fl + ns_fl + elev_fl + bark_fl

  tbl <- vesta_moisture_table(month, hour)
  ffmc <- vesta_fuel_moisture(temp, rh, tbl)

  mf <- ffmc^(-1.495) / 0.0545
  sf <- exp(0.069 * slope)

  effective_wind <- pmax(wind_speed - 5, 0)
  ros <- (30 + 3.102 * effective_wind^0.904 *
            exp(0.279 * surface_score + 0.611 * near_surface_score +
                  0.013 * near_surface_height)) * mf * sf

  flank_ew <- ifelse(0.2 * wind_speed + 4 < 5, 0, 0.2 * wind_speed + 1)
  flank_ros <- (30 + 3.102 * flank_ew^0.904 *
                  exp(0.279 * surface_score + 0.611 * near_surface_score +
                        0.013 * near_surface_height)) * mf

  flame_ht <- 0.0193 * ros^0.723 * exp(0.64 * elevated_height)

  spot_dist <- ifelse(ros < 150, 50,
    abs(176.969 * (atan(surface_score) * (ros / wind_speed^0.25)^0.5) +
          1568800 * (surface_score^(-1) * (ros / wind_speed^0.25)^(-1.5)) -
          3015.09))

  intensity <- 18600 * ros / 3600 * total_fl / 10

  list(
    fuel_moisture    = ffmc,
    moisture_factor  = mf,
    slope_factor     = sf,
    total_fuel_load  = total_fl,
    ros              = ros,
    flank_ros        = flank_ros,
    flame_height     = flame_ht,
    spotting_dist    = spot_dist,
    intensity        = intensity
  )
}
