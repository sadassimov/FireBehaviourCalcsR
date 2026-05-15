#!/usr/bin/env Rscript
#
# FireBehaviourCalcsR -- spatial function tests
# Run with:  Rscript tests/test_spatial.R

# ---- bootstrap (always source from R/ to pick up latest code) ----
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
script_dir <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
for (f in list.files(file.path(root, "R"), full.names = TRUE)) source(f)

if (!requireNamespace("terra", quietly = TRUE)) {
  cat("SKIP: terra not installed\n")
  quit(status = 0)
}
library(terra)

# ---- helpers ----
pass <- 0L
fail <- 0L

check <- function(actual, expected, label, tol = 1e-2) {
  ok <- isTRUE(all.equal(actual, expected, tolerance = tol,
                         check.attributes = FALSE))
  if (ok) {
    pass <<- pass + 1L
    cat(sprintf("  PASS  %s\n", label))
  } else {
    fail <<- fail + 1L
    cat(sprintf("  FAIL  %s: expected %s, got %s\n",
                label, format(expected, digits = 8),
                format(actual, digits = 8)))
  }
}

check_true <- function(value, label) {
  if (isTRUE(value)) {
    pass <<- pass + 1L
    cat(sprintf("  PASS  %s\n", label))
  } else {
    fail <<- fail + 1L
    cat(sprintf("  FAIL  %s: expected TRUE\n", label))
  }
}

section <- function(title) cat(sprintf("\n--- %s ---\n", title))

# ---- create test rasters ----
# 10x10 grid, GDA2020 / MGA Zone 55 (EPSG:7855), 100 m resolution
r_template <- rast(nrows = 10, ncols = 10,
                   xmin = 500000, xmax = 501000,
                   ymin = 5800000, ymax = 5801000,
                   crs = "EPSG:7855")

r_temp <- setValues(r_template, 38.9)
r_rh   <- setValues(r_template, 10.6)
r_wind <- setValues(r_template, 32)
r_df   <- setValues(r_template, 9.9)
r_fuel <- setValues(r_template, 15)

# Slope raster with gradient 0-20 degrees
r_slope <- setValues(r_template, rep(seq(0, 18, by = 2), each = 10))

# ============================================================
#  1. align_inputs
# ============================================================
section("align_inputs")

aligned <- align_inputs(list(temp = r_temp, rh = r_rh, wind = 32))
check_true(inherits(aligned$temp, "SpatRaster"), "raster preserved")
check_true(is.numeric(aligned$wind), "scalar preserved")
check(ncell(aligned$temp), 100, "grid size preserved")

# Test file path input
tmp_tif <- tempfile(fileext = ".tif")
writeRaster(r_temp, tmp_tif, overwrite = TRUE)
aligned2 <- align_inputs(list(temp = tmp_tif, rh = 10.6))
check_true(inherits(aligned2$temp, "SpatRaster"), "file path loaded as raster")
file.remove(tmp_tif)

# Test CRS reprojection (aligns to first raster's CRS)
r_wgs <- project(r_temp, "EPSG:4326")
aligned3 <- align_inputs(list(rh = r_rh, temp = r_wgs))
check(crs(aligned3$temp, describe = TRUE)$code, "7855",
      "CRS aligned to first raster (7855)")

# Test forced target CRS
aligned4 <- align_inputs(list(temp = r_temp, rh = r_rh),
                         target_crs = "EPSG:4326")
check(crs(aligned4$temp, describe = TRUE)$code, "4326",
      "target_crs overrides to 4326")

# ============================================================
#  2. slope_from_dem
# ============================================================
section("slope_from_dem")

r_dem <- setValues(r_template, rep(seq(100, 200, length.out = 10), each = 10))
slp <- slope_from_dem(r_dem)
check_true(inherits(slp, "SpatRaster"), "returns SpatRaster")
check_true(all(values(slp, na.rm = TRUE) >= 0), "slope values >= 0")

# ============================================================
#  3. mcarthur_fdi_spatial
# ============================================================
section("mcarthur_fdi_spatial")

fdi_r <- mcarthur_fdi_spatial(
  drought_factor = r_df, rh = r_rh, temp = r_temp,
  wind_speed = r_wind, wind_reduction = 3
)
check_true(inherits(fdi_r, "SpatRaster"), "returns SpatRaster")

# Compare with scalar version
fdi_scalar <- mcarthur_fdi(9.9, 10.6, 38.9, 32, 3)
fdi_vals <- unique(round(values(fdi_r, na.rm = TRUE), 4))
check(length(fdi_vals), 1, "uniform inputs -> one unique value")
check(fdi_vals[1], round(fdi_scalar, 4), "spatial matches scalar")

# Test with mixed raster + scalar
fdi_mix <- mcarthur_fdi_spatial(
  drought_factor = 9.9, rh = r_rh, temp = 38.9,
  wind_speed = r_wind
)
check_true(inherits(fdi_mix, "SpatRaster"), "mixed input returns SpatRaster")

# Test file output
tmp_out <- tempfile(fileext = ".tif")
mcarthur_fdi_spatial(r_df, r_rh, r_temp, r_wind, filename = tmp_out)
check_true(file.exists(tmp_out), "GeoTIFF written")
fdi_read <- rast(tmp_out)
check(ncell(fdi_read), 100, "output grid correct")
file.remove(tmp_out)

# ============================================================
#  4. forest_mcarthur_mk5_spatial
# ============================================================
section("forest_mcarthur_mk5_spatial")

mk5_r <- forest_mcarthur_mk5_spatial(
  temp = r_temp, rh = r_rh, wind_speed = r_wind,
  drought_factor = r_df, fuel_load = r_fuel, slope = r_slope
)
check_true(inherits(mk5_r, "SpatRaster"), "returns SpatRaster")
check(nlyr(mk5_r), 9, "9 output layers")
check_true("fdi" %in% names(mk5_r), "has fdi layer")
check_true("ros" %in% names(mk5_r), "has ros layer")
check_true("intensity" %in% names(mk5_r), "has intensity layer")

ros_vals <- values(mk5_r[["ros"]])
check_true(all(ros_vals > 0, na.rm = TRUE), "all ROS > 0")

# Verify slope effect: column 1 (slope=0) < column 10 (slope=18)
ros_flat <- mean(ros_vals[1:10])
ros_steep <- mean(ros_vals[91:100])
check_true(ros_steep > ros_flat, "steeper slope -> higher ROS")

# ============================================================
#  5. grass_fire_spatial
# ============================================================
section("grass_fire_spatial")

r_curing <- setValues(r_template, 90)
gr_r <- grass_fire_spatial(
  temp = r_temp, rh = r_rh, wind_speed = r_wind,
  curing = r_curing, slope = 0
)
check_true(inherits(gr_r, "SpatRaster"), "returns SpatRaster")
check_true("natural_ros" %in% names(gr_r), "has natural_ros layer")
check_true("grazed_ros" %in% names(gr_r), "has grazed_ros layer")

nat_ros <- unique(round(values(gr_r[["natural_ros"]], na.rm = TRUE), 2))
gr_scalar <- grass_fire(38.9, 10.6, 32, 90, 0)
check(nat_ros[1], round(gr_scalar$natural$ros, 2),
      "spatial natural ROS matches scalar")

# ============================================================
#  6. heath_fire_spatial
# ============================================================
section("heath_fire_spatial")

r_vh <- setValues(r_template, 1.5)
ht_r <- heath_fire_spatial(
  temp = r_temp, rh = r_rh, wind_speed = r_wind,
  veg_height = r_vh, slope = 0
)
check_true(inherits(ht_r, "SpatRaster"), "returns SpatRaster")
check_true("heath_ros" %in% names(ht_r), "has heath_ros layer")

ht_scalar <- heath_fire(38.9, 10.6, 32, 1.5, 0)
ht_val <- unique(round(values(ht_r[["heath_ros"]], na.rm = TRUE), 2))
check(ht_val[1], round(ht_scalar$heath_ros, 2),
      "spatial heath ROS matches scalar")

# ============================================================
#  7. buttongrass_fire_spatial
# ============================================================
section("buttongrass_fire_spatial")

bg_r <- buttongrass_fire_spatial(
  temp = r_temp, rh = r_rh, wind_speed = r_wind,
  age = 60, cover = 0, productivity = 1, slope = 0
)
check_true(inherits(bg_r, "SpatRaster"), "returns SpatRaster")
check_true("head_ros" %in% names(bg_r), "has head_ros layer")

bg_scalar <- buttongrass_fire(38.9, 10.6, 32, 0, 0, 60, 0, 1, 0)
bg_val <- unique(round(values(bg_r[["head_ros"]], na.rm = TRUE), 2))
check(bg_val[1], round(bg_scalar$head_ros, 2),
      "spatial buttongrass ROS matches scalar")

# ============================================================
#  8. mallee_fire_spatial
# ============================================================
section("mallee_fire_spatial")

r_fmc <- setValues(r_template, 8)
ml_r <- mallee_fire_spatial(
  wind_speed = r_wind, dead_fmc = r_fmc, slope = r_slope
)
check_true(inherits(ml_r, "SpatRaster"), "returns SpatRaster")
check(nlyr(ml_r), 2, "2 output layers")

ml_scalar <- mallee_fire(32, 8, 0)
ml_flat <- unique(round(values(ml_r[["ros"]], na.rm = TRUE), 2))
check(ml_flat[1], round(ml_scalar$ros, 2),
      "spatial mallee ROS matches scalar")

# ============================================================
#  9. mallee_heath_fire_spatial
# ============================================================
section("mallee_heath_fire_spatial")

mh_r <- mallee_heath_fire_spatial(
  temp = 35, rh = 15, wind_speed = r_wind,
  surface_fmc = 6, overstorey_cover = 30,
  overstorey_height = 4, surface_fuel_load = 8
)
check_true(inherits(mh_r, "SpatRaster"), "returns SpatRaster")
check_true("ros" %in% names(mh_r), "has ros layer")

# ============================================================
# 10. drought_factor_spatial
# ============================================================
section("drought_factor_spatial")

r_kbdi <- setValues(r_template, 100)
df_r <- drought_factor_spatial(
  kbdi = r_kbdi, days_since_rain = 10, rain_amount = 5
)
check_true(inherits(df_r, "SpatRaster"), "returns SpatRaster")

df_scalar <- drought_factor(100, 10, 5)
df_val <- unique(round(values(df_r, na.rm = TRUE), 4))
check(df_val[1], round(df_scalar, 4), "spatial DF matches scalar")

# ============================================================
# 11. CRS & resolution alignment integration
# ============================================================
section("CRS & resolution alignment")

# Create rasters with different CRS
r_temp_wgs <- project(r_temp, "EPSG:4326")
fdi_cross <- mcarthur_fdi_spatial(
  drought_factor = 9.9, rh = r_rh,
  temp = r_temp_wgs, wind_speed = 32,
  target_crs = "EPSG:7855"
)
check(crs(fdi_cross, describe = TRUE)$code, "7855",
      "output CRS is EPSG:7855")

# ============================================================
#  Summary
# ============================================================
cat(sprintf("\n============================\n"))
cat(sprintf("  %d passed, %d failed\n", pass, fail))
cat(sprintf("============================\n"))
if (fail > 0) quit(status = 1)
