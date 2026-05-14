#' WA Red Book Basic Drying Unit
#'
#' @param temp Air temperature (degrees C).
#' @param rh Relative humidity (%).
#' @return Basic drying unit value.
#' @export
redbook_bdu <- function(temp, rh) {
  (57.29 / (1 + 1.93 * exp(-0.042 * temp))) /
    (1 + (42.77 / (temp - 5.8)) * exp((-0.016 * temp / (temp - 5.27)) *
                                         (100 - rh)))
}

#' WA Red Book Night Wetting Correction (Rain)
#'
#' @param smc_prev Previous 1500h surface moisture content (%).
#' @param rain_amount Rainfall amount (mm).
#' @param days_since_rain Days since rain.
#' @return Night wetting correction for rain.
#' @export
redbook_nwc_rain <- function(smc_prev, rain_amount, days_since_rain) {
  ifelse(days_since_rain <= 1,
    (121.42 - 94.27 / (1 + 107.55 * exp(-0.037 * smc_prev))) /
      (1 + (101.71 / (1 + 17.13 * exp(-0.017 * smc_prev))) *
         rain_amount^(-0.84 - 0.62 / (1 + 1087.88 * exp(-0.053 * smc_prev)))),
    0)
}

#' WA Red Book Night Wetting Correction (Dry)
#'
#' @param smc_prev Previous 1500h surface moisture content (%).
#' @param rh_count RH count value.
#' @param days_since_rain Days since rain.
#' @return Night wetting correction for dry conditions.
#' @export
redbook_nwc_dry <- function(smc_prev, rh_count, days_since_rain) {
  ifelse(days_since_rain > 1,
    0.29 / (1 + 1.34 * exp(-0.019 * smc_prev)) * rh_count +
      (51.61 - 367.12 * (0.29 / (1 + 1.34 * exp(-0.019 * smc_prev)))),
    0)
}

#' WA Red Book Day Drying Correction (Jarrah Surface)
#'
#' @param smc_0800 0800h surface moisture content (%).
#' @param bdu Basic drying unit.
#' @return Day drying correction.
#' @export
redbook_ddc_jarrah_surface <- function(smc_0800, bdu) {
  ((smc_0800 - 103.79) / ((smc_0800 - 103.79)^2 - 12643.93) * 12.81 -
     0.65) * bdu +
    ((smc_0800 - 99.7) / ((smc_0800 - 99.7)^2 - 25199.32) * 3570.12 -
       23.26) + 10
}

#' WA Red Book Day Drying Correction (Karri Profile)
#'
#' @param pmc_0800 0800h profile moisture content (%).
#' @param bdu Basic drying unit.
#' @return Day drying correction for Karri profile.
#' @export
redbook_ddc_karri_profile <- function(pmc_0800, bdu) {
  ((pmc_0800 - 112.15) / ((pmc_0800 - 112.15)^2 - 21835.42) * 40.96 -
     0.46) * bdu +
    ((pmc_0800 - 140.78) / ((pmc_0800 - 140.78)^2 - 24819.47) * 298.78)
}

#' WA Red Book Northern Jarrah FDI
#'
#' @param smc Surface moisture content (%).
#' @param wind_speed Wind speed at 10 m (km/h).
#' @param wind_reduction Wind reduction factor (default 3).
#' @return Fire Danger Index (m/h).
#' @references Beck, J.A. (1995). Equations for the forest fire behaviour
#'   tables for Western Australia. \emph{CALMScience} 1(3):325-348.
#' @export
redbook_fdi_jarrah <- function(smc, wind_speed, wind_reduction = 3) {
  ifelse(smc <= 27,
    (21.37 - 3.42 * smc + 0.085 * smc^2) +
      (48.09 * smc * exp(-0.6 * smc) + 11.9) *
      exp(wind_speed / wind_reduction * (-0.0096 * smc^1.05 + 0.44)),
    0)
}

#' WA Red Book Fuel Quantity Correction Factor (Jarrah)
#'
#' @param total_fuel Total available fuel quantity (t/ha).
#' @param smc Surface moisture content (%).
#' @return Correction factor.
#' @export
redbook_fqcf_jarrah <- function(total_fuel, smc) {
  ifelse(total_fuel < 8,
    1.02 / (1 + 7266.83 * exp(-1.36 * total_fuel)) + 0.1,
    ifelse(smc <= 9, (6.03 + 5.81 * total_fuel) / 53.44,
           ifelse(smc <= 18, (11.19 + 2.92 * total_fuel) / 35.02,
                  (0.055 + 0.0023 * total_fuel) / 0.074)))
}

#' WA Red Book Karri FDI
#'
#' @param smc Surface moisture content (%).
#' @param wind_speed Wind speed at 10 m (km/h).
#' @param wind_reduction Wind reduction factor (default 3).
#' @return FDI (m/h).
#' @export
redbook_fdi_karri <- function(smc, wind_speed, wind_reduction = 3) {
  ifelse(smc <= 27,
    (4.88 - 263.78 * smc^(-1.8)) +
      (163.4 * smc^(-1.18)) *
      exp(wind_speed / wind_reduction * (-0.0059 * smc + 0.54)),
    0)
}

#' WA Red Book Available Fuel Factor (Karri)
#'
#' @param smc Surface moisture content (%).
#' @param profile_mc Profile moisture content (%).
#' @return Available fuel factor (0-1).
#' @export
redbook_aff_karri <- function(smc, profile_mc) {
  pmax(1 - (1 / (1 + (0.43 * exp(0.23 * smc) + 2) *
                   exp((-0.0085 * smc + 0.024) * profile_mc)) +
              (0.013 * smc - 0.43)), 0)
}
