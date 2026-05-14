#' Wind Reduction Factor Guide
#'
#' Lookup table for wind reduction factors by vegetation type.
#'
#' @format A data frame with 10 rows and 3 columns:
#' \describe{
#'   \item{factor_low}{Lower numeric wind reduction factor}
#'   \item{factor_high}{Upper numeric wind reduction factor}
#'   \item{vegetation}{Vegetation type description}
#' }
#' @source Tolhurst (2011) revised wind reduction factors.
"wind_reduction_factors"

#' NSW Fuel Types
#'
#' Fuel type classification table used in NSW fire behaviour prediction,
#' including fuel load equations, hazard scores, and Phoenix inputs.
#'
#' @format A data frame with 68 rows and fuel type parameters.
#' @source NSW Rural Fire Service fuel type classification.
"nsw_fuel_types"
