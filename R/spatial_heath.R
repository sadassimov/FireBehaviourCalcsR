#' Heathland Fire Behaviour (spatial)
#'
#' Raster version of \code{\link{heath_fire}}.
#'
#' @inheritParams heath_fire
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path prefix.
#' @return A multi-layer \code{SpatRaster}.
#' @seealso \code{\link{heath_fire}} for the scalar version.
#' @export
heath_fire_spatial <- function(temp, rh, wind_speed, veg_height,
                               slope = 0, overcast = FALSE,
                               hour = 14, month = 2,
                               target_crs = NULL, target_res = NULL,
                               filename = NULL) {
  check_terra()

  inputs <- list(temp = temp, rh = rh, wind_speed = wind_speed,
                 veg_height = veg_height, slope = slope,
                 overcast = overcast, hour = hour, month = month)
  aligned <- align_inputs(inputs, target_crs = target_crs,
                          target_res = target_res)

  is_rast <- vapply(aligned, inherits, logical(1), "SpatRaster")
  rast_args <- aligned[is_rast]
  scalar_args <- aligned[!is_rast]
  rast_stack <- terra::rast(rast_args)

  rast_names <- names(rast_args)
  all_names <- names(aligned)
  out_names <- c("dead_fmc", "heath_ros", "heath_intensity", "woodland_ros")

  wrapper <- function(...) {
    vals <- stats::setNames(list(...), rast_names)
    args <- c(vals, scalar_args)[all_names]
    res <- do.call(heath_fire, args)
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
