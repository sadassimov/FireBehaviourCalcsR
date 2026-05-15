#' WA Red Book Basic Drying Unit (spatial)
#'
#' Raster version of \code{\link{redbook_bdu}}.
#'
#' @inheritParams redbook_bdu
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path.
#' @return A \code{SpatRaster} of BDU values.
#' @seealso \code{\link{redbook_bdu}} for the scalar version.
#' @export
redbook_bdu_spatial <- function(temp, rh,
                                target_crs = NULL, target_res = NULL,
                                filename = NULL) {
  fire_lapp(
    fun = redbook_bdu,
    inputs = list(temp = temp, rh = rh),
    target_crs = target_crs, target_res = target_res,
    filename = filename
  )
}

#' WA Red Book Northern Jarrah FDI (spatial)
#'
#' Raster version of \code{\link{redbook_fdi_jarrah}}.
#'
#' @inheritParams redbook_fdi_jarrah
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path.
#' @return A \code{SpatRaster} of FDI values (m/h).
#' @seealso \code{\link{redbook_fdi_jarrah}} for the scalar version.
#' @export
redbook_fdi_jarrah_spatial <- function(smc, wind_speed, wind_reduction = 3,
                                       target_crs = NULL, target_res = NULL,
                                       filename = NULL) {
  fire_lapp(
    fun = redbook_fdi_jarrah,
    inputs = list(smc = smc, wind_speed = wind_speed,
                  wind_reduction = wind_reduction),
    target_crs = target_crs, target_res = target_res,
    filename = filename
  )
}

#' WA Red Book Fuel Quantity Correction Factor, Jarrah (spatial)
#'
#' Raster version of \code{\link{redbook_fqcf_jarrah}}.
#'
#' @inheritParams redbook_fqcf_jarrah
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path.
#' @return A \code{SpatRaster} of correction factor values.
#' @seealso \code{\link{redbook_fqcf_jarrah}} for the scalar version.
#' @export
redbook_fqcf_jarrah_spatial <- function(total_fuel, smc,
                                        target_crs = NULL, target_res = NULL,
                                        filename = NULL) {
  fire_lapp(
    fun = redbook_fqcf_jarrah,
    inputs = list(total_fuel = total_fuel, smc = smc),
    target_crs = target_crs, target_res = target_res,
    filename = filename
  )
}

#' WA Red Book Karri FDI (spatial)
#'
#' Raster version of \code{\link{redbook_fdi_karri}}.
#'
#' @inheritParams redbook_fdi_karri
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path.
#' @return A \code{SpatRaster} of FDI values (m/h).
#' @seealso \code{\link{redbook_fdi_karri}} for the scalar version.
#' @export
redbook_fdi_karri_spatial <- function(smc, wind_speed, wind_reduction = 3,
                                      target_crs = NULL, target_res = NULL,
                                      filename = NULL) {
  fire_lapp(
    fun = redbook_fdi_karri,
    inputs = list(smc = smc, wind_speed = wind_speed,
                  wind_reduction = wind_reduction),
    target_crs = target_crs, target_res = target_res,
    filename = filename
  )
}

#' WA Red Book Available Fuel Factor, Karri (spatial)
#'
#' Raster version of \code{\link{redbook_aff_karri}}.
#'
#' @inheritParams redbook_aff_karri
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path.
#' @return A \code{SpatRaster} of available fuel factor values (0-1).
#' @seealso \code{\link{redbook_aff_karri}} for the scalar version.
#' @export
redbook_aff_karri_spatial <- function(smc, profile_mc,
                                      target_crs = NULL, target_res = NULL,
                                      filename = NULL) {
  fire_lapp(
    fun = redbook_aff_karri,
    inputs = list(smc = smc, profile_mc = profile_mc),
    target_crs = target_crs, target_res = target_res,
    filename = filename
  )
}
