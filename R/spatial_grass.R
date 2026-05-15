#' CSIRO Grassland Fire Behaviour (spatial)
#'
#' Raster version of \code{\link{grass_fire}}.  Returns a multi-layer
#' \code{SpatRaster} with outputs for all five grass types.
#'
#' @inheritParams grass_fire
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path prefix.
#' @return A multi-layer \code{SpatRaster}.
#' @seealso \code{\link{grass_fire}} for the scalar version.
#' @export
grass_fire_spatial <- function(temp, rh, wind_speed, curing, slope = 0,
                               target_crs = NULL, target_res = NULL,
                               filename = NULL) {
  check_terra()

  inputs <- list(temp = temp, rh = rh, wind_speed = wind_speed,
                 curing = curing, slope = slope)
  aligned <- align_inputs(inputs, target_crs = target_crs,
                          target_res = target_res)

  is_rast <- vapply(aligned, inherits, logical(1), "SpatRaster")
  rast_args <- aligned[is_rast]
  scalar_args <- aligned[!is_rast]
  rast_stack <- terra::rast(rast_args)

  rast_names <- names(rast_args)
  all_names <- names(aligned)
  out_names <- c("fdi", "dead_fmc", "mc_coeff", "cure_coeff",
                 "natural_ros", "natural_flame", "natural_intensity",
                 "grazed_ros", "grazed_flame", "grazed_intensity",
                 "eaten_out_ros", "eaten_out_flame", "eaten_out_intensity",
                 "open_woodland_ros", "open_woodland_flame",
                 "open_woodland_intensity",
                 "woodland_ros", "woodland_flame", "woodland_intensity")

  wrapper <- function(...) {
    vals <- stats::setNames(list(...), rast_names)
    args <- c(vals, scalar_args)[all_names]
    res <- do.call(grass_fire, args)
    c(res$fdi, res$dead_fmc, res$mc_coeff, res$cure_coeff,
      res$natural$ros, res$natural$flame_height, res$natural$intensity,
      res$grazed$ros, res$grazed$flame_height, res$grazed$intensity,
      res$eaten_out$ros, res$eaten_out$flame_height, res$eaten_out$intensity,
      res$open_woodland$ros, res$open_woodland$flame_height,
      res$open_woodland$intensity,
      res$woodland$ros, res$woodland$flame_height, res$woodland$intensity)
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
