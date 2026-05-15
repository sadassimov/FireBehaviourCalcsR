#' Buttongrass Fire Behaviour (spatial)
#'
#' Raster version of \code{\link{buttongrass_fire}}.
#'
#' @inheritParams buttongrass_fire
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path prefix.
#' @return A multi-layer \code{SpatRaster}.
#' @seealso \code{\link{buttongrass_fire}} for the scalar version.
#' @export
buttongrass_fire_spatial <- function(temp, rh, wind_speed,
                                     rain_amount = 0,
                                     hours_since_rain = 0,
                                     age = 20, cover = 0,
                                     productivity = 2, slope = 0,
                                     target_crs = NULL, target_res = NULL,
                                     filename = NULL) {
  check_terra()

  inputs <- list(temp = temp, rh = rh, wind_speed = wind_speed,
                 rain_amount = rain_amount,
                 hours_since_rain = hours_since_rain,
                 age = age, cover = cover,
                 productivity = productivity, slope = slope)
  aligned <- align_inputs(inputs, target_crs = target_crs,
                          target_res = target_res)

  is_rast <- vapply(aligned, inherits, logical(1), "SpatRaster")
  rast_args <- aligned[is_rast]
  scalar_args <- aligned[!is_rast]
  rast_stack <- terra::rast(rast_args)

  rast_names <- names(rast_args)
  all_names <- names(aligned)
  out_names <- c("dew_point", "fuel_moisture", "fuel_load",
                 "head_ros", "head_flame", "flank_ros", "flank_flame",
                 "back_ros", "back_flame", "danger_rating", "prob_sustain")

  wrapper <- function(...) {
    vals <- stats::setNames(list(...), rast_names)
    args <- c(vals, scalar_args)[all_names]
    res <- do.call(buttongrass_fire, args)
    flatten_result(res)
  }

  result <- terra::lapp(rast_stack, fun = wrapper)
  names(result) <- out_names

  if (!is.null(filename)) {
    base <- tools::file_path_sans_ext(filename)
    ext <- tools::file_ext(filename)
    if (ext == "") ext <- "tif"
    for (nm in out_names) {
      write_fire_raster(result[[nm]],
                        sprintf("%s_%s.%s", base, nm, ext))
    }
  }

  result
}
