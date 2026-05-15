#' Check that terra is available
#'
#' @return TRUE invisibly; stops with a message if terra is not installed.
#' @keywords internal
check_terra <- function() {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required for spatial functions. ",
         "Install it with install.packages('terra').", call. = FALSE)
  }
  invisible(TRUE)
}

#' Align rasters to a common grid
#'
#' Reprojects and resamples all \code{SpatRaster} inputs to a common CRS and
#' resolution.  The reference grid is taken from the first raster argument
#' unless \code{target_crs} or \code{target_res} are supplied.  Scalar
#' (numeric) inputs are returned unchanged.
#'
#' @param inputs Named list of inputs.  Each element is either a
#'   \code{SpatRaster}, a file path (character of length 1 pointing to a
#'   raster file), or a numeric scalar.
#' @param target_crs Target CRS as a string (e.g. \code{"EPSG:4326"}).
#'   If NULL, the CRS of the first raster input is used.
#' @param target_res Target resolution as a numeric vector of length 1 or 2
#'   (x, y).  If NULL, the resolution of the first raster input is used.
#' @param method Resampling method passed to \code{\link[terra]{project}};
#'   one of \code{"bilinear"} (default) or \code{"near"}.
#' @return A named list with the same names as \code{inputs}, where every
#'   raster has been aligned to the common grid and scalars are unchanged.
#' @export
align_inputs <- function(inputs, target_crs = NULL, target_res = NULL,
                         method = "bilinear") {
  check_terra()

  # Load file paths as SpatRaster
  inputs <- lapply(inputs, function(x) {
    if (is.character(x) && length(x) == 1 && file.exists(x)) {
      terra::rast(x)
    } else {
      x
    }
  })

  is_rast <- vapply(inputs, inherits, logical(1), "SpatRaster")
  if (!any(is_rast)) {
    stop("At least one input must be a SpatRaster or raster file path.",
         call. = FALSE)
  }

  ref <- inputs[[which(is_rast)[1]]]

  crs_out <- if (!is.null(target_crs)) target_crs else terra::crs(ref)
  res_out <- if (!is.null(target_res)) target_res else terra::res(ref)

  # Build reference grid
  if (!is.null(target_crs) || !is.null(target_res)) {
    ref <- terra::project(ref, crs_out, res = res_out)
  }

  aligned <- lapply(inputs, function(x) {
    if (!inherits(x, "SpatRaster")) return(x)

    needs_align <- !identical(terra::crs(x), terra::crs(ref)) ||
      !identical(terra::res(x), terra::res(ref)) ||
      !terra::compareGeom(x, ref, stopOnError = FALSE)

    if (needs_align) {
      terra::project(x, ref, method = method)
    } else {
      x
    }
  })

  aligned
}

#' Apply a fire model function across raster and scalar inputs
#'
#' Wraps \code{\link[terra]{lapp}} to call a vectorised function with a mix
#' of \code{SpatRaster} and numeric scalar arguments.  All rasters are first
#' aligned via \code{\link{align_inputs}}.
#'
#' @param fun A vectorised function (e.g. \code{mcarthur_fdi}).
#' @param inputs Named list of arguments to \code{fun}.  Each element is a
#'   \code{SpatRaster}, a raster file path, or a numeric scalar.
#' @param target_crs Optional target CRS (see \code{\link{align_inputs}}).
#' @param target_res Optional target resolution.
#' @param filename Optional output file path.  If provided, the result is
#'   written as a GeoTIFF and the \code{SpatRaster} is returned.
#' @return A \code{SpatRaster}.
#' @export
fire_lapp <- function(fun, inputs, target_crs = NULL, target_res = NULL,
                      filename = NULL) {
  check_terra()

  aligned <- align_inputs(inputs, target_crs = target_crs,
                          target_res = target_res)

  is_rast <- vapply(aligned, inherits, logical(1), "SpatRaster")
  rast_args <- aligned[is_rast]
  scalar_args <- aligned[!is_rast]

  rast_stack <- terra::rast(rast_args)

  rast_names <- names(rast_args)
  scalar_names <- names(scalar_args)
  all_names <- names(aligned)

  wrapper <- function(...) {
    vals <- list(...)
    named <- stats::setNames(vals, rast_names)
    args <- c(named, scalar_args)[all_names]
    do.call(fun, args)
  }

  result <- terra::lapp(rast_stack, fun = wrapper)

  write_fire_raster(result, filename)
}

#' Flatten a list result for lapp
#'
#' Ensures all elements have the same length by recycling scalars,
#' then concatenates into a single vector.
#'
#' @param x A named list (possibly nested) of numeric values.
#' @return A numeric vector.
#' @keywords internal
flatten_result <- function(x) {
  flat <- rapply(x, identity, how = "unlist")
  n <- max(lengths(rapply(x, identity, how = "list")))
  if (n > 1) {
    flat_list <- rapply(x, function(v) {
      if (length(v) == 1) rep(v, n) else v
    }, how = "list")
    flat <- unlist(flat_list)
  }
  flat
}

#' Build a multi-output lapp wrapper
#'
#' Creates a wrapper function suitable for \code{terra::lapp} that calls
#' a fire model and returns a fixed-length vector per pixel.  Handles the
#' case where some model outputs are scalar (recycled to match vector
#' outputs).
#'
#' @param fun The fire model function.
#' @param rast_names Names of raster arguments.
#' @param scalar_args Named list of scalar arguments.
#' @param all_names Argument names in the original order.
#' @return A function for use with \code{terra::lapp}.
#' @keywords internal
make_lapp_wrapper <- function(fun, rast_names, scalar_args, all_names) {
  function(...) {
    vals <- stats::setNames(list(...), rast_names)
    args <- c(vals, scalar_args)[all_names]
    res <- do.call(fun, args)
    flatten_result(res)
  }
}

#' Compute slope in degrees from a DEM
#'
#' Thin wrapper around \code{\link[terra]{terrain}}.
#'
#' @param dem A \code{SpatRaster} or file path to a DEM.
#' @param filename Optional output file path for the slope raster.
#' @return A \code{SpatRaster} of slope in degrees.
#' @export
slope_from_dem <- function(dem, filename = NULL) {
  check_terra()
  if (is.character(dem)) dem <- terra::rast(dem)
  s <- terra::terrain(dem, v = "slope", unit = "degrees")
  write_fire_raster(s, filename)
}

#' Write a SpatRaster to GeoTIFF
#'
#' Writes with LZW compression and Float32 data type.  If \code{filename}
#' is NULL the raster is returned unchanged (no file written).
#'
#' @param x A \code{SpatRaster}.
#' @param filename File path (NULL to skip writing).
#' @return The \code{SpatRaster}, invisibly.
#' @export
write_fire_raster <- function(x, filename = NULL) {
  if (!is.null(filename)) {
    check_terra()
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    terra::writeRaster(x, filename, overwrite = TRUE,
                       wopt = list(datatype = "FLT4S",
                                   gdal = c("COMPRESS=LZW")))
  }
  invisible(x)
}
