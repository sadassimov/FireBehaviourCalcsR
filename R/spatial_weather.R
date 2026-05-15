#' Dew point (spatial)
#'
#' Raster version of \code{\link{dew_point}}.
#'
#' @inheritParams dew_point
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path.
#' @return A \code{SpatRaster} of dew point values (degrees C).
#' @seealso \code{\link{dew_point}} for the scalar version.
#' @export
dew_point_spatial <- function(temp, rh,
                              target_crs = NULL, target_res = NULL,
                              filename = NULL) {
  fire_lapp(
    fun = dew_point,
    inputs = list(temp = temp, rh = rh),
    target_crs = target_crs, target_res = target_res,
    filename = filename
  )
}

#' Relative humidity from dew point (spatial)
#'
#' Raster version of \code{\link{rh_from_dewpoint}}.
#'
#' @inheritParams rh_from_dewpoint
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path.
#' @return A \code{SpatRaster} of RH values (%).
#' @seealso \code{\link{rh_from_dewpoint}} for the scalar version.
#' @export
rh_from_dewpoint_spatial <- function(temp, dew_pt,
                                     target_crs = NULL, target_res = NULL,
                                     filename = NULL) {
  fire_lapp(
    fun = rh_from_dewpoint,
    inputs = list(temp = temp, dew_pt = dew_pt),
    target_crs = target_crs, target_res = target_res,
    filename = filename
  )
}

#' McArthur surface fine fuel moisture content (spatial)
#'
#' Raster version of \code{\link{fuel_moisture_mcarthur}}.
#'
#' @inheritParams fuel_moisture_mcarthur
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path.
#' @return A \code{SpatRaster} of fuel moisture values (%).
#' @seealso \code{\link{fuel_moisture_mcarthur}} for the scalar version.
#' @export
fuel_moisture_mcarthur_spatial <- function(temp, rh,
                                           target_crs = NULL,
                                           target_res = NULL,
                                           filename = NULL) {
  fire_lapp(
    fun = fuel_moisture_mcarthur,
    inputs = list(temp = temp, rh = rh),
    target_crs = target_crs, target_res = target_res,
    filename = filename
  )
}
