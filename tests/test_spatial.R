#!/usr/bin/env Rscript
#
# FireBehaviourCalcsR -- spatial function tests
# Uses synthetic rasters from inst/testdata/
# Run with:  Rscript tests/test_spatial.R

# ---- bootstrap (always source from R/) ----
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

# ---- load test rasters ----
td <- file.path(root, "inst", "testdata")
r_temp   <- rast(file.path(td, "temp.tif"))
r_rh     <- rast(file.path(td, "rh.tif"))
r_wind   <- rast(file.path(td, "wind_speed.tif"))
r_df     <- rast(file.path(td, "drought_factor.tif"))
r_fuel   <- rast(file.path(td, "fuel_load.tif"))
r_slope  <- rast(file.path(td, "slope.tif"))
r_dem    <- rast(file.path(td, "dem.tif"))
r_curing <- rast(file.path(td, "curing.tif"))
r_vh     <- rast(file.path(td, "veg_height.tif"))
r_kbdi   <- rast(file.path(td, "kbdi.tif"))
r_fmc    <- rast(file.path(td, "dead_fmc.tif"))
r_sfmc   <- rast(file.path(td, "surface_fmc.tif"))
r_oc     <- rast(file.path(td, "overstorey_cover.tif"))
r_oh     <- rast(file.path(td, "overstorey_height.tif"))
r_smc    <- rast(file.path(td, "smc.tif"))

n <- ncell(r_temp)
target_crs <- "EPSG:7855"

# ============================================================
#  1. align_inputs
# ============================================================
section("align_inputs")

aligned <- align_inputs(list(temp = r_temp, rh = r_rh, wind = 32))
check_true(inherits(aligned$temp, "SpatRaster"), "raster preserved")
check_true(is.numeric(aligned$wind), "scalar preserved")
check(ncell(aligned$temp), n, "grid size preserved")

r_wgs <- project(r_temp, "EPSG:4326")
aligned2 <- align_inputs(list(rh = r_rh, temp = r_wgs))
check(crs(aligned2$temp, describe = TRUE)$code, "7855",
      "CRS aligned to first raster")

aligned3 <- align_inputs(list(temp = r_temp, rh = r_rh),
                         target_crs = "EPSG:4326")
check(crs(aligned3$temp, describe = TRUE)$code, "4326",
      "target_crs override")

# file path input
tmp_tif <- tempfile(fileext = ".tif")
writeRaster(r_temp, tmp_tif, overwrite = TRUE)
aligned4 <- align_inputs(list(temp = tmp_tif, rh = 10.6))
check_true(inherits(aligned4$temp, "SpatRaster"), "file path -> SpatRaster")
file.remove(tmp_tif)

# ============================================================
#  2. slope_from_dem
# ============================================================
section("slope_from_dem")

slp <- slope_from_dem(r_dem)
check_true(inherits(slp, "SpatRaster"), "returns SpatRaster")
check_true(all(values(slp, na.rm = TRUE) >= 0), "all slopes >= 0")

# ============================================================
#  3. Weather utilities (spatial)
# ============================================================
section("dew_point_spatial")

dp <- dew_point_spatial(r_temp, r_rh)
check_true(inherits(dp, "SpatRaster"), "returns SpatRaster")
dp_vals <- values(dp, na.rm = TRUE)
check_true(all(dp_vals < values(r_temp, na.rm = TRUE)),
           "dew point < temperature everywhere")

section("fuel_moisture_mcarthur_spatial")

fm <- fuel_moisture_mcarthur_spatial(r_temp, r_rh)
check_true(inherits(fm, "SpatRaster"), "returns SpatRaster")
check_true(all(values(fm, na.rm = TRUE) > 0), "FFMC > 0 everywhere")

section("rh_from_dewpoint_spatial")

rh_back <- rh_from_dewpoint_spatial(r_temp, dp)
check_true(inherits(rh_back, "SpatRaster"), "returns SpatRaster")
rh_diff <- abs(values(rh_back, na.rm = TRUE) - values(r_rh, na.rm = TRUE))
check_true(all(rh_diff < 5), "round-trip RH within 5%")

# ============================================================
#  4. drought_factor_spatial
# ============================================================
section("drought_factor_spatial")

df_r <- drought_factor_spatial(r_kbdi, days_since_rain = 10, rain_amount = 5)
check_true(inherits(df_r, "SpatRaster"), "returns SpatRaster")
df_vals <- values(df_r, na.rm = TRUE)
check_true(all(df_vals >= 0 & df_vals <= 10), "DF in 0-10")

# ============================================================
#  5. mcarthur_fdi_spatial
# ============================================================
section("mcarthur_fdi_spatial")

fdi <- mcarthur_fdi_spatial(r_df, r_rh, r_temp, r_wind)
check_true(inherits(fdi, "SpatRaster"), "returns SpatRaster")
check_true(all(values(fdi, na.rm = TRUE) > 0), "FDI > 0 everywhere")

tmp_out <- tempfile(fileext = ".tif")
mcarthur_fdi_spatial(r_df, r_rh, r_temp, r_wind, filename = tmp_out)
check_true(file.exists(tmp_out), "GeoTIFF written")
file.remove(tmp_out)

# ============================================================
#  6. forest_mcarthur_mk5_spatial
# ============================================================
section("forest_mcarthur_mk5_spatial")

mk5 <- forest_mcarthur_mk5_spatial(r_temp, r_rh, r_wind, r_df,
                                    r_fuel, r_slope)
check_true(inherits(mk5, "SpatRaster"), "returns SpatRaster")
check(nlyr(mk5), 9, "9 output layers")
check_true(all(values(mk5[["ros"]], na.rm = TRUE) > 0), "all ROS > 0")
check_true(all(values(mk5[["intensity"]], na.rm = TRUE) > 0),
           "all intensity > 0")

# ============================================================
#  7. forest_leaflet80_spatial
# ============================================================
section("forest_leaflet80_spatial")

l80 <- forest_leaflet80_spatial(r_temp, r_rh, r_wind, r_df, r_fuel,
                                 slope = r_slope, days_since_rain = 5,
                                 rain_amount = 10)
check_true(inherits(l80, "SpatRaster"), "returns SpatRaster")
check(nlyr(l80), 9, "9 output layers")
check_true("ros_slope" %in% names(l80), "has ros_slope layer")
check_true("fmc" %in% names(l80), "has fmc layer")

# ============================================================
#  8. mcarthur_fdi_matthews_spatial
# ============================================================
section("mcarthur_fdi_matthews_spatial")

fdi_m <- mcarthur_fdi_matthews_spatial(r_df, r_wind, fm)
check_true(inherits(fdi_m, "SpatRaster"), "returns SpatRaster")
check_true(all(values(fdi_m, na.rm = TRUE) > 0), "Matthews FDI > 0")

# ============================================================
#  9. forest_vesta_spatial
# ============================================================
section("forest_vesta_spatial")

vesta <- forest_vesta_spatial(r_temp, r_rh, r_wind, r_slope,
                               surface_score = 4, near_surface_score = 3.5,
                               near_surface_height = 25, elevated_score = 2,
                               elevated_height = 1.5, bark_score = 3.5,
                               month = 2, hour = 13)
check_true(inherits(vesta, "SpatRaster"), "returns SpatRaster")
check(nlyr(vesta), 9, "9 output layers")
check_true("ros" %in% names(vesta), "has ros layer")

# ============================================================
# 10. grass_fire_spatial
# ============================================================
section("grass_fire_spatial")

gr <- grass_fire_spatial(r_temp, r_rh, r_wind, r_curing, slope = r_slope)
check_true(inherits(gr, "SpatRaster"), "returns SpatRaster")
check(nlyr(gr), 19, "19 output layers")
check_true("natural_ros" %in% names(gr), "has natural_ros")
check_true("woodland_ros" %in% names(gr), "has woodland_ros")
nat <- values(gr[["natural_ros"]], na.rm = TRUE)
wl  <- values(gr[["woodland_ros"]], na.rm = TRUE)
check_true(all(nat >= wl), "natural ROS >= woodland ROS")

# ============================================================
# 11. heath_fire_spatial
# ============================================================
section("heath_fire_spatial")

ht <- heath_fire_spatial(r_temp, r_rh, r_wind, r_vh)
check_true(inherits(ht, "SpatRaster"), "returns SpatRaster")
check(nlyr(ht), 4, "4 output layers")
h_ros <- values(ht[["heath_ros"]], na.rm = TRUE)
w_ros <- values(ht[["woodland_ros"]], na.rm = TRUE)
check_true(all(h_ros >= w_ros), "heath ROS >= woodland ROS")

# ============================================================
# 12. buttongrass_fire_spatial
# ============================================================
section("buttongrass_fire_spatial")

bg <- buttongrass_fire_spatial(r_temp, r_rh, r_wind,
                                age = 60, cover = 0, productivity = 1,
                                slope = r_slope)
check_true(inherits(bg, "SpatRaster"), "returns SpatRaster")
check(nlyr(bg), 11, "11 output layers")
check_true("head_ros" %in% names(bg), "has head_ros")
check_true("prob_sustain" %in% names(bg), "has prob_sustain")

# ============================================================
# 13. mallee_fire_spatial
# ============================================================
section("mallee_fire_spatial")

ml <- mallee_fire_spatial(r_wind, r_fmc, slope = r_slope)
check_true(inherits(ml, "SpatRaster"), "returns SpatRaster")
check(nlyr(ml), 2, "2 output layers")
flat <- as.vector(values(ml[["ros"]]))
sloped <- as.vector(values(ml[["ros_slope"]]))
ok_idx <- !is.na(flat) & !is.na(sloped)
check_true(all(sloped[ok_idx] >= flat[ok_idx] - 0.01),
           "slope ROS >= flat ROS")

# ============================================================
# 14. mallee_heath_fire_spatial
# ============================================================
section("mallee_heath_fire_spatial")

mh <- mallee_heath_fire_spatial(r_temp, r_rh, r_wind,
                                 surface_fmc = r_sfmc,
                                 overstorey_cover = r_oc,
                                 overstorey_height = r_oh,
                                 surface_fuel_load = r_fuel)
check_true(inherits(mh, "SpatRaster"), "returns SpatRaster")
check(nlyr(mh), 9, "9 output layers")
check_true("prob_crown" %in% names(mh), "has prob_crown")

# ============================================================
# 15. Red Book spatial
# ============================================================
section("Red Book spatial")

bdu <- redbook_bdu_spatial(r_temp, r_rh)
check_true(inherits(bdu, "SpatRaster"), "BDU returns SpatRaster")

fdi_j <- redbook_fdi_jarrah_spatial(r_smc, r_wind)
check_true(inherits(fdi_j, "SpatRaster"), "Jarrah FDI returns SpatRaster")
check_true(all(values(fdi_j, na.rm = TRUE) >= 0), "Jarrah FDI >= 0")

fdi_k <- redbook_fdi_karri_spatial(r_smc, r_wind)
check_true(inherits(fdi_k, "SpatRaster"), "Karri FDI returns SpatRaster")

fqcf <- redbook_fqcf_jarrah_spatial(r_fuel, r_smc)
check_true(inherits(fqcf, "SpatRaster"), "Jarrah FQCF returns SpatRaster")

aff <- redbook_aff_karri_spatial(r_smc, profile_mc = 20)
check_true(inherits(aff, "SpatRaster"), "Karri AFF returns SpatRaster")
aff_vals <- values(aff, na.rm = TRUE)
check_true(all(aff_vals >= 0), "Karri AFF >= 0")

# ============================================================
# 16. Multi-output file writing
# ============================================================
section("File output")

tmp_dir <- tempdir()
tmp_prefix <- file.path(tmp_dir, "test_mk5")
mk5_out <- forest_mcarthur_mk5_spatial(
  r_temp, r_rh, r_wind, r_df, r_fuel, 0,
  filename = paste0(tmp_prefix, ".tif")
)
expected_files <- paste0(tmp_prefix, "_",
  c("fdi", "ros", "ros_flat", "flank_ros", "flame_height",
    "spotting_dist", "intensity", "heat_output", "ffmc"), ".tif")
written <- file.exists(expected_files)
check(sum(written), 9, "9 individual GeoTIFF files written")
file.remove(expected_files[written])

# ============================================================
#  Summary
# ============================================================
cat(sprintf("\n============================\n"))
cat(sprintf("  %d passed, %d failed\n", pass, fail))
cat(sprintf("============================\n"))
if (fail > 0) quit(status = 1)
