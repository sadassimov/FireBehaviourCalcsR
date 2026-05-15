#' McArthur Mk5 Forest Fire Danger Index (spatial)
#'
#' Raster version of \code{\link{mcarthur_fdi}}.  Each input can be a
#' \code{SpatRaster}, a file path to a raster, or a numeric scalar.
#' All rasters are automatically aligned to a common CRS and resolution.
#'
#' @inheritParams mcarthur_fdi
#' @param target_crs Target CRS (e.g. \code{"EPSG:4326"}).  Defaults to the
#'   CRS of the first raster input.
#' @param target_res Target resolution in map units.
#' @param filename Optional output GeoTIFF path.
#' @return A \code{SpatRaster} of FDI values.
#' @seealso \code{\link{mcarthur_fdi}} for the scalar version.
#' @export
mcarthur_fdi_spatial <- function(drought_factor, rh, temp, wind_speed,
                                 wind_reduction = 3,
                                 target_crs = NULL, target_res = NULL,
                                 filename = NULL) {
  fire_lapp(
    fun = mcarthur_fdi,
    inputs = list(drought_factor = drought_factor, rh = rh,
                  temp = temp, wind_speed = wind_speed,
                  wind_reduction = wind_reduction),
    target_crs = target_crs, target_res = target_res,
    filename = filename
  )
}

#' McArthur Mk5 Forest Fire Behaviour (spatial)
#'
#' Raster version of \code{\link{forest_mcarthur_mk5}}.  Returns a multi-layer
#' \code{SpatRaster} with FDI, rate of spread, flame height, spotting
#' distance, fireline intensity, and related outputs.
#'
#' @inheritParams forest_mcarthur_mk5
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path (multi-band).
#' @return A named list of \code{SpatRaster} layers.
#' @seealso \code{\link{forest_mcarthur_mk5}} for the scalar version.
#' @export
forest_mcarthur_mk5_spatial <- function(temp, rh, wind_speed, drought_factor,
                                        fuel_load, slope = 0,
                                        wind_reduction = 3,
                                        target_crs = NULL, target_res = NULL,
                                        filename = NULL) {
  check_terra()

  inputs <- list(temp = temp, rh = rh, wind_speed = wind_speed,
                 drought_factor = drought_factor, fuel_load = fuel_load,
                 slope = slope, wind_reduction = wind_reduction)
  aligned <- align_inputs(inputs, target_crs = target_crs,
                          target_res = target_res)

  is_rast <- vapply(aligned, inherits, logical(1), "SpatRaster")
  rast_args <- aligned[is_rast]
  scalar_args <- aligned[!is_rast]
  rast_stack <- terra::rast(rast_args)

  rast_names <- names(rast_args)
  all_names <- names(aligned)
  out_names <- c("fdi", "ros", "ros_flat", "flank_ros", "flame_height",
                 "spotting_dist", "intensity", "heat_output", "ffmc")

  wrapper <- function(...) {
    vals <- stats::setNames(list(...), rast_names)
    args <- c(vals, scalar_args)[all_names]
    res <- do.call(forest_mcarthur_mk5, args)
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

#' McArthur Leaflet 80 Control Burning Guide (spatial)
#'
#' Raster version of \code{\link{forest_leaflet80}}.
#'
#' @inheritParams forest_leaflet80
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path prefix.
#' @return A multi-layer \code{SpatRaster}.
#' @seealso \code{\link{forest_leaflet80}} for the scalar version.
#' @export
forest_leaflet80_spatial <- function(temp, rh, wind_speed, drought_factor,
                                     fuel_load, slope = 0,
                                     wind_reduction = 3,
                                     time_hour = 14, kbdi = 50,
                                     days_since_rain = 5, rain_amount = 0,
                                     use_bom_df = FALSE,
                                     target_crs = NULL, target_res = NULL,
                                     filename = NULL) {
  check_terra()

  inputs <- list(temp = temp, rh = rh, wind_speed = wind_speed,
                 drought_factor = drought_factor, fuel_load = fuel_load,
                 slope = slope, wind_reduction = wind_reduction,
                 time_hour = time_hour, kbdi = kbdi,
                 days_since_rain = days_since_rain,
                 rain_amount = rain_amount, use_bom_df = use_bom_df)
  aligned <- align_inputs(inputs, target_crs = target_crs,
                          target_res = target_res)

  is_rast <- vapply(aligned, inherits, logical(1), "SpatRaster")
  rast_args <- aligned[is_rast]
  scalar_args <- aligned[!is_rast]
  rast_stack <- terra::rast(rast_args)

  rast_names <- names(rast_args)
  all_names <- names(aligned)
  out_names <- c("fmc", "fuel_avail", "ros_flat", "ros_slope",
                 "flank_ros", "flame_height", "scorch_height",
                 "intensity", "out_of_range")

  wrapper <- function(...) {
    vals <- stats::setNames(list(...), rast_names)
    args <- c(vals, scalar_args)[all_names]
    res <- do.call(forest_leaflet80, args)
    res$out_of_range <- as.numeric(res$out_of_range)
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

#' McArthur FDI via Matthews fuel moisture method (spatial)
#'
#' Raster version of \code{\link{mcarthur_fdi_matthews}}.
#'
#' @inheritParams mcarthur_fdi_matthews
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path.
#' @return A \code{SpatRaster} of FDI values.
#' @seealso \code{\link{mcarthur_fdi_matthews}} for the scalar version.
#' @export
mcarthur_fdi_matthews_spatial <- function(drought_factor, wind_speed, ffmc,
                                          target_crs = NULL,
                                          target_res = NULL,
                                          filename = NULL) {
  fire_lapp(
    fun = mcarthur_fdi_matthews,
    inputs = list(drought_factor = drought_factor,
                  wind_speed = wind_speed, ffmc = ffmc),
    target_crs = target_crs, target_res = target_res,
    filename = filename
  )
}

#' VESTA Forest Fire Behaviour (spatial)
#'
#' Raster version of \code{\link{forest_vesta}}.
#'
#' @inheritParams forest_vesta
#' @param target_crs Target CRS.
#' @param target_res Target resolution.
#' @param filename Optional output GeoTIFF path prefix.
#' @return A named list of \code{SpatRaster} layers.
#' @seealso \code{\link{forest_vesta}} for the scalar version.
#' @export
forest_vesta_spatial <- function(temp, rh, wind_speed, slope,
                                 surface_score, near_surface_score,
                                 near_surface_height, elevated_score,
                                 elevated_height, bark_score,
                                 month = 1, hour = 14,
                                 target_crs = NULL, target_res = NULL,
                                 filename = NULL) {
  check_terra()

  inputs <- list(temp = temp, rh = rh, wind_speed = wind_speed,
                 slope = slope, surface_score = surface_score,
                 near_surface_score = near_surface_score,
                 near_surface_height = near_surface_height,
                 elevated_score = elevated_score,
                 elevated_height = elevated_height,
                 bark_score = bark_score,
                 month = month, hour = hour)
  aligned <- align_inputs(inputs, target_crs = target_crs,
                          target_res = target_res)

  is_rast <- vapply(aligned, inherits, logical(1), "SpatRaster")
  rast_args <- aligned[is_rast]
  scalar_args <- aligned[!is_rast]
  rast_stack <- terra::rast(rast_args)

  rast_names <- names(rast_args)
  all_names <- names(aligned)
  out_names <- c("fuel_moisture", "moisture_factor", "slope_factor",
                 "total_fuel_load", "ros", "flank_ros",
                 "flame_height", "spotting_dist", "intensity")

  wrapper <- function(...) {
    vals <- stats::setNames(list(...), rast_names)
    args <- c(vals, scalar_args)[all_names]
    res <- do.call(forest_vesta, args)
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
