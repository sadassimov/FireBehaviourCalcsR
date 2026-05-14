if (requireNamespace("FireBehaviourCalcsR", quietly = TRUE)) {
  library(FireBehaviourCalcsR)
} else {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  script_dir <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
  root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)

  for (file in c(
    "weather.R",
    "drought.R",
    "slope.R",
    "forest_mcarthur.R",
    "forest_vesta.R",
    "grass.R",
    "heath.R",
    "buttongrass.R",
    "mallee.R",
    "redbook.R",
    "suppression.R"
  )) {
    source(file.path(root, "R", file))
  }
}

expect_close <- function(actual, expected, label, tolerance = 1e-8) {
  ok <- isTRUE(all.equal(actual, expected, tolerance = tolerance,
                         check.attributes = FALSE))
  if (!ok) {
    stop(sprintf("%s: expected %.15g, got %.15g", label, expected, actual),
         call. = FALSE)
  }
}

# Workbook: Forest(VESTA)!Q14, first weather row in the original XLSM.
vesta <- forest_vesta(
  temp = 38.9, rh = 10.6, wind_speed = 32, slope = 0,
  surface_score = 4, near_surface_score = 3.5,
  near_surface_height = 25, elevated_score = 2,
  elevated_height = 1.5, bark_score = 3.5,
  month = 2, hour = 13
)
expect_close(vesta$flank_ros, 2204.43979502205, "VESTA flank ROS")

# Workbook: Buttongrass!J13:T13, first weather row in the original XLSM.
button <- buttongrass_fire(
  temp = 38.9, rh = 10.6, wind_speed = 32,
  rain_amount = 0, hours_since_rain = 0,
  age = 60, cover = 0, productivity = 1, slope = 0
)
expect_close(button$dew_point, 2.60443141316762, "Buttongrass dew point")
expect_close(button$fuel_moisture, 6.11528027886485,
             "Buttongrass fuel moisture")
expect_close(button$head_ros, 1922.00689245082, "Buttongrass head ROS")
expect_close(button$prob_sustain, 0.999999941582946,
             "Buttongrass probability of sustaining")

cat("Workbook regression checks passed\n")
