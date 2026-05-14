#' Hand trail construction rate
#'
#' Estimated fireline construction rate by hand crews over a 12-hour shift.
#'
#' @param n_firefighters Number of fire fighters.
#' @param slope Slope (degrees).
#' @param elevated_fuel Elevated fuel hazard: "Low", "Moderate", "High",
#'   or "Very High".
#' @param flame_height Flame height (m).
#' @return Estimated construction rate (m/h).
#' @references McCarthy, Tolhurst & Wouters (2003). Research Report No.56,
#'   Fire Management, DSE Victoria.
#' @export
hand_trail_rate <- function(n_firefighters, slope, elevated_fuel,
                            flame_height) {
  fuel_penalty <- switch(tolower(elevated_fuel),
    low = 0, moderate = 0, high = 4, `very high` = 10,
    stop("elevated_fuel must be Low, Moderate, High, or Very High"))

  raw <- ((23 - fuel_penalty) - 11 / 30 * slope) * n_firefighters
  rate <- pmax(raw, 0) * (1 - (flame_height / 5)^2)
  pmax(rate, 0)
}

#' D4 bulldozer construction rate
#'
#' @param n_dozers Number of D4 bulldozers.
#' @param slope Slope (degrees).
#' @param debris Surface debris level: "None", "Some", "Significant".
#' @param flame_height Flame height (m).
#' @return Estimated construction rate (m/h).
#' @export
d4_dozer_rate <- function(n_dozers, slope, debris, flame_height) {
  debris_penalty <- switch(tolower(debris),
    none = 0, some = 220, significant = 380,
    stop("debris must be None, Some, or Significant"))

  raw <- ((680 - debris_penalty) - 64 / 3 * slope) * n_dozers
  rate <- pmax(raw, 0) * (1 - (flame_height / 10)^2)
  pmax(rate, 0)
}

#' D6 bulldozer construction rate
#'
#' @param n_dozers Number of D6 bulldozers.
#' @param slope Slope (degrees).
#' @param debris Surface debris level: "None", "Some", "Significant",
#'   or "Very Significant".
#' @param flame_height Flame height (m).
#' @return Estimated construction rate (m/h).
#' @export
d6_dozer_rate <- function(n_dozers, slope, debris, flame_height) {
  debris_penalty <- switch(tolower(debris),
    none = 0, some = 180, significant = 360,
    `very significant` = 480,
    stop("debris must be None, Some, Significant, or Very Significant"))

  raw <- ((900 - debris_penalty) - 50 / 3 * slope) * n_dozers
  rate <- pmax(raw, 0) * (1 - (flame_height / 10)^2)
  pmax(rate, 0)
}

#' Tanker construction rate
#'
#' @param n_tankers Number of tankers (2500-4000L).
#' @param access Road access quality: "Good" or "Poor".
#' @param bark_hazard Bark hazard: "Low", "Moderate", "High", "Very High",
#'   or "Extreme".
#' @param flame_height Flame height (m).
#' @param wind_speed Wind speed (km/h).
#' @return Estimated construction rate (m/h).
#' @export
tanker_rate <- function(n_tankers, access, bark_hazard, flame_height,
                        wind_speed) {
  hazard_val <- switch(tolower(bark_hazard),
    low = 1, moderate = 2, high = 3, `very high` = 4, extreme = 5,
    stop("Invalid bark_hazard"))

  base <- switch(tolower(access),
    good = 2000, poor = 200,
    stop("access must be Good or Poor"))

  n_tankers * base * (1 / wind_speed) * 5 * (1 / flame_height) *
    (1 / hazard_val)
}

#' Firebomber holding rate
#'
#' @param n_bombers Number of aircraft.
#' @param turnaround Turnaround time (minutes).
#' @param aircraft_type Aircraft type: "1400L Medium Helicopter" or
#'   "2500L Fixed-wing".
#' @return Estimated holding rate (m/h).
#' @export
firebomber_rate <- function(n_bombers, turnaround, aircraft_type) {
  rate_per <- switch(aircraft_type,
    `1400L Medium Helicopter` = 360 / turnaround * 5,
    `2500L Fixed-wing`        = 140 / turnaround * 30,
    stop("aircraft_type must be '1400L Medium Helicopter' or '2500L Fixed-wing'"))
  n_bombers * rate_per
}

#' Combined suppression rate
#'
#' @param hand Hand trail rate (m/h, default 0).
#' @param d4 D4 dozer rate (m/h, default 0).
#' @param d6 D6 dozer rate (m/h, default 0).
#' @param tanker Tanker rate (m/h, default 0).
#' @param bomber1 Firebomber rate 1 (m/h, default 0).
#' @param bomber2 Firebomber rate 2 (m/h, default 0).
#' @return Total suppression rate (m/h).
#' @export
combined_suppression_rate <- function(hand = 0, d4 = 0, d6 = 0,
                                     tanker = 0, bomber1 = 0, bomber2 = 0) {
  hand + d4 + d6 + tanker + bomber1 + bomber2
}
