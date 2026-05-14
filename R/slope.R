#' Calculate slope from rise and distance
#'
#' @param rise Elevation change (m).
#' @param distance Horizontal distance (m).
#' @return Slope in degrees.
#' @export
slope_from_rise <- function(rise, distance) {
  atan(rise / distance) * 180 / pi
}

#' Rate of spread slope modification factor
#'
#' Standard exponential slope correction used across multiple Australian
#' fire behaviour models.
#'
#' @param slope Slope in degrees.
#' @return Multiplicative factor for rate of spread.
#' @export
slope_factor <- function(slope) {
  exp(0.069 * slope)
}

#' Convert slope between percent and degrees
#'
#' @param value Slope value to convert.
#' @param from Unit of input: "percent" or "degrees".
#' @return Converted slope value.
#' @export
slope_convert <- function(value, from = c("percent", "degrees")) {
  from <- match.arg(from)
  switch(from,
    percent = atan(value / 100) * 180 / pi,
    degrees = tan(value * pi / 180) * 100
  )
}

#' Calculate horizontal distance for a slope template
#'
#' Distance on a map between contours for a given rise and slope.
#'
#' @param rise Elevation change between contours (m).
#' @param slope_deg Slope (degrees).
#' @return Horizontal distance (m).
#' @export
slope_template_distance <- function(rise, slope_deg) {
  rise / tan(slope_deg * pi / 180)
}

#' Find rise or distance given slope and one measurement
#'
#' @param slope_deg Slope (degrees).
#' @param rise Rise (m). Provide either rise or distance.
#' @param distance Distance (m). Provide either rise or distance.
#' @return The missing measurement (rise or distance in m).
#' @export
slope_solve <- function(slope_deg, rise = NULL, distance = NULL) {
  if (!is.null(rise) && is.null(distance)) {
    rise / tan(slope_deg * pi / 180)
  } else if (is.null(rise) && !is.null(distance)) {
    distance * tan(slope_deg * pi / 180)
  } else {
    stop("Provide exactly one of 'rise' or 'distance'.")
  }
}
