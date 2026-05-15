#' Drought Factor (spatial)
#'
#' Raster version of \code{\link{drought_factor}}.
#'
#' @inheritParams drought_factor
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path.
#' @return A \code{SpatRaster} of drought factor values (0-10).
#' @seealso \code{\link{drought_factor}} for the scalar version.
#' @export
drought_factor_spatial <- function(kbdi, days_since_rain, rain_amount,
                                   target_crs = NULL, target_res = NULL,
                                   filename = NULL) {
  fire_lapp(
    fun = drought_factor,
    inputs = list(kbdi = kbdi, days_since_rain = days_since_rain,
                  rain_amount = rain_amount),
    target_crs = target_crs, target_res = target_res,
    filename = filename
  )
}
