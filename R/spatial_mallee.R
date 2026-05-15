#' WA Mallee Fire Spread (spatial)
#'
#' Raster version of \code{\link{mallee_fire}}.
#'
#' @inheritParams mallee_fire
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path prefix.
#' @return A multi-layer \code{SpatRaster} with ros and ros_slope.
#' @seealso \code{\link{mallee_fire}} for the scalar version.
#' @export
mallee_fire_spatial <- function(wind_speed, dead_fmc, slope = 0,
                                target_crs = NULL, target_res = NULL,
                                filename = NULL) {
  check_terra()

  inputs <- list(wind_speed = wind_speed, dead_fmc = dead_fmc,
                 slope = slope)
  aligned <- align_inputs(inputs, target_crs = target_crs,
                          target_res = target_res)

  is_rast <- vapply(aligned, inherits, logical(1), "SpatRaster")
  rast_args <- aligned[is_rast]
  scalar_args <- aligned[!is_rast]
  rast_stack <- terra::rast(rast_args)

  rast_names <- names(rast_args)
  all_names <- names(aligned)
  out_names <- c("ros", "ros_slope")

  wrapper <- function(...) {
    vals <- stats::setNames(list(...), rast_names)
    args <- c(vals, scalar_args)[all_names]
    res <- do.call(mallee_fire, args)
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

#' Mallee-Heath Fire Behaviour (spatial)
#'
#' Raster version of \code{\link{mallee_heath_fire}}.
#'
#' @inheritParams mallee_heath_fire
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path prefix.
#' @return A multi-layer \code{SpatRaster}.
#' @seealso \code{\link{mallee_heath_fire}} for the scalar version.
#' @export
mallee_heath_fire_spatial <- function(temp, rh, wind_speed,
                                      wind_speed_2m = NULL,
                                      surface_fmc,
                                      overstorey_cover,
                                      overstorey_height,
                                      surface_fuel_load,
                                      slope = 0,
                                      target_crs = NULL,
                                      target_res = NULL,
                                      filename = NULL) {
  check_terra()

  inputs <- list(temp = temp, rh = rh, wind_speed = wind_speed,
                 surface_fmc = surface_fmc,
                 overstorey_cover = overstorey_cover,
                 overstorey_height = overstorey_height,
                 surface_fuel_load = surface_fuel_load,
                 slope = slope)
  if (!is.null(wind_speed_2m)) {
    inputs$wind_speed_2m <- wind_speed_2m
  }

  aligned <- align_inputs(inputs, target_crs = target_crs,
                          target_res = target_res)

  is_rast <- vapply(aligned, inherits, logical(1), "SpatRaster")
  rast_args <- aligned[is_rast]
  scalar_args <- aligned[!is_rast]
  rast_stack <- terra::rast(rast_args)

  rast_names <- names(rast_args)
  all_names <- names(aligned)
  out_names <- c("prob_go_2m", "prob_go_10m", "prob_crown",
                 "surface_ros_2m", "surface_ros_10m", "crown_ros",
                 "ros", "intensity", "flame_height")

  wrapper <- function(...) {
    vals <- stats::setNames(list(...), rast_names)
    args <- c(vals, scalar_args)[all_names]
    res <- do.call(mallee_heath_fire, args)
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
