wind_reduction_factors <- data.frame(
  factor_low = c(1, 1.2, 1.5, 2, 2.5, 3, 3.5, 4, 4, 5),
  factor_high = c(1, 1.2, 1.5, 2, 2.5, 3, 3.5, 5, 6, 9),
  vegetation = c(
    "Herbfield",
    "Grassland, sedgeland",
    "Heathland, Mallee woodland",
    "Tall shrubland (>1.5 m)",
    "Eucalypt woodland (>6 m)",
    "Open Eucalypt Forest (standard McArthur forest)",
    "Shrubby open forest",
    "Damp forest with shrubs, Karri",
    "Wet eucalypt forest, Mature plantation",
    "Rainforest"
  ),
  stringsAsFactors = FALSE
)

nsw_fuel_types <- read.csv("data-raw/nsw_fuel_types.csv", stringsAsFactors = FALSE)

dir.create("data", showWarnings = FALSE)
save(wind_reduction_factors, file = "data/wind_reduction_factors.rda")
save(nsw_fuel_types, file = "data/nsw_fuel_types.rda")
cat("Done:", nrow(wind_reduction_factors), "wind,", nrow(nsw_fuel_types), "fuel\n")
