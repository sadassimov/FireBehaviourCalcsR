#!/usr/bin/env Rscript
#
# Generate synthetic fire weather rasters for testing and demonstration.
#
# Produces a 50x50 grid (1 km resolution) over a fictional landscape in
# GDA2020 / MGA Zone 55 (EPSG:7855).  Values are spatially varied using
# simple gradients to exercise the spatial functions.
#
# Run from the package root:
#   Rscript inst/testdata/build_test_rasters.R

library(terra)

outdir <- "inst/testdata"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# --- template grid ---
nx <- 50; ny <- 50; res <- 1000  # 1 km
r <- rast(nrows = ny, ncols = nx,
          xmin = 300000, xmax = 300000 + nx * res,
          ymin = 5800000, ymax = 5800000 + ny * res,
          crs = "EPSG:7855")

set.seed(42)
noise <- function(n, sd = 1) rnorm(n, 0, sd)

# row/col indices (0-1 normalised)
coords <- xyFromCell(r, 1:ncell(r))
xn <- (coords[, 1] - min(coords[, 1])) / diff(range(coords[, 1]))
yn <- (coords[, 2] - min(coords[, 2])) / diff(range(coords[, 2]))

# --- DEM: ridge running N-S, valleys on sides ---
dem_vals <- 200 + 300 * exp(-((xn - 0.5)^2) / 0.08) + noise(ncell(r), 10)
dem <- setValues(r, dem_vals)
writeRaster(dem, file.path(outdir, "dem.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Slope: derived from DEM ---
slp <- terrain(dem, v = "slope", unit = "degrees")
writeRaster(slp, file.path(outdir, "slope.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Temperature: warm NW, cooler SE + elevation lapse ---
temp_vals <- 38 - 8 * yn + 2 * (1 - xn) - 0.006 * (dem_vals - 200) +
  noise(ncell(r), 0.5)
temp_vals <- pmax(temp_vals, 15)
temp <- setValues(r, temp_vals)
writeRaster(temp, file.path(outdir, "temp.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Relative humidity: inverse of temperature pattern ---
rh_vals <- 15 + 30 * yn - 5 * (1 - xn) + noise(ncell(r), 2)
rh_vals <- pmin(pmax(rh_vals, 5), 95)
rh <- setValues(r, rh_vals)
writeRaster(rh, file.path(outdir, "rh.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Wind speed: stronger on ridge, sheltered in valleys ---
wind_vals <- 20 + 15 * exp(-((xn - 0.5)^2) / 0.1) + noise(ncell(r), 1.5)
wind_vals <- pmax(wind_vals, 2)
wind <- setValues(r, wind_vals)
writeRaster(wind, file.path(outdir, "wind_speed.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Drought factor: drier in NW ---
df_vals <- 4 + 5 * (1 - yn) + noise(ncell(r), 0.3)
df_vals <- pmin(pmax(df_vals, 0), 10)
df <- setValues(r, df_vals)
writeRaster(df, file.path(outdir, "drought_factor.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Fuel load: varies with elevation (more fuel at higher elevations) ---
fuel_vals <- 8 + 12 * (dem_vals - min(dem_vals)) /
  diff(range(dem_vals)) + noise(ncell(r), 0.5)
fuel_vals <- pmax(fuel_vals, 2)
fuel <- setValues(r, fuel_vals)
writeRaster(fuel, file.path(outdir, "fuel_load.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Grass curing: higher curing in drier areas ---
curing_vals <- 60 + 35 * (1 - yn) + noise(ncell(r), 3)
curing_vals <- pmin(pmax(curing_vals, 20), 100)
curing <- setValues(r, curing_vals)
writeRaster(curing, file.path(outdir, "curing.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Vegetation height: taller in valleys ---
vh_vals <- 0.5 + 2.5 * (1 - exp(-((xn - 0.5)^2) / 0.08)) +
  noise(ncell(r), 0.1)
vh_vals <- pmax(vh_vals, 0.2)
vh <- setValues(r, vh_vals)
writeRaster(vh, file.path(outdir, "veg_height.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- KBDI: drier in the north ---
kbdi_vals <- 50 + 100 * (1 - yn) + noise(ncell(r), 5)
kbdi_vals <- pmin(pmax(kbdi_vals, 0), 200)
kbdi <- setValues(r, kbdi_vals)
writeRaster(kbdi, file.path(outdir, "kbdi.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Dead fuel moisture content ---
fmc_vals <- 4 + 8 * yn + noise(ncell(r), 0.5)
fmc_vals <- pmax(fmc_vals, 2)
fmc <- setValues(r, fmc_vals)
writeRaster(fmc, file.path(outdir, "dead_fmc.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Surface fuel moisture (for mallee-heath) ---
sfmc_vals <- 5 + 6 * yn + noise(ncell(r), 0.3)
sfmc_vals <- pmax(sfmc_vals, 2)
sfmc <- setValues(r, sfmc_vals)
writeRaster(sfmc, file.path(outdir, "surface_fmc.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Overstorey cover (%) ---
oc_vals <- 20 + 40 * (1 - exp(-((xn - 0.5)^2) / 0.08)) +
  noise(ncell(r), 3)
oc_vals <- pmin(pmax(oc_vals, 0), 100)
oc <- setValues(r, oc_vals)
writeRaster(oc, file.path(outdir, "overstorey_cover.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Overstorey height (m) ---
oh_vals <- 3 + 6 * (1 - exp(-((xn - 0.5)^2) / 0.08)) +
  noise(ncell(r), 0.3)
oh_vals <- pmax(oh_vals, 1)
oh <- setValues(r, oh_vals)
writeRaster(oh, file.path(outdir, "overstorey_height.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

# --- Surface moisture content (for Red Book) ---
smc_vals <- 8 + 12 * yn + noise(ncell(r), 0.5)
smc_vals <- pmin(pmax(smc_vals, 3), 27)
smc <- setValues(r, smc_vals)
writeRaster(smc, file.path(outdir, "smc.tif"), overwrite = TRUE,
            wopt = list(datatype = "FLT4S", gdal = "COMPRESS=LZW"))

cat("Test rasters written to", outdir, "\n")
cat("Files:\n")
cat(paste(" ", list.files(outdir, pattern = "\\.tif$")), sep = "\n")
