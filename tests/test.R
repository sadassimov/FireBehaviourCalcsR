#!/usr/bin/env Rscript
#
# FireBehaviourCalcsR -- unit & regression tests
# Run with:  Rscript tests/test.R
#
# Expected values are taken from the Advanced Fire Behaviour Prediction
# Standard Workbook (Tolhurst, 2007-2016), Weather_Site row 1:
#   Sat 2017-02-11 13:00, Melbourne
#   Temp 38.9 C, RH 10.6%, Wind 32 km/h (10 m open)

# ---- bootstrap ----
if (requireNamespace("FireBehaviourCalcsR", quietly = TRUE)) {
  library(FireBehaviourCalcsR)
} else {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  script_dir <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
  root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
  for (f in list.files(file.path(root, "R"), full.names = TRUE)) source(f)
}

# ---- helpers ----
pass <- 0L
fail <- 0L

check <- function(actual, expected, label, tol = 1e-4) {
  ok <- isTRUE(all.equal(actual, expected, tolerance = tol,
                         check.attributes = FALSE))
  if (ok) {
    pass <<- pass + 1L
    cat(sprintf("  PASS  %s\n", label))
  } else {
    fail <<- fail + 1L
    cat(sprintf("  FAIL  %s: expected %s, got %s\n",
                label, format(expected, digits = 10),
                format(actual, digits = 10)))
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

# ============================================================
#  1. Weather utilities
# ============================================================
section("Weather utilities")

dp <- dew_point(34, 20)
check(round(dp, 1), 7.8, "dew_point(34, 20)")

dp2 <- dew_point(38.9, 10.6)
check(round(dp2, 2), 2.57, "dew_point(38.9, 10.6)")

rh <- rh_from_dewpoint(34, 7.8)
check(round(rh, 0), 20, "rh_from_dewpoint(34, 7.8)")

wb <- rh_from_wetbulb(34, 20)
check_true(wb$rh > 0 && wb$rh < 100, "rh_from_wetbulb returns valid RH")

check(wind_dir_to_degrees("N"), 360, "wind N -> 360")
check(wind_dir_to_degrees("SW"), 225, "wind SW -> 225")
check(wind_dir_to_degrees("ESE"), 112.5, "wind ESE -> 112.5")

check(fire_spread_direction(360), 180, "spread from N -> 180")
check(fire_spread_direction(90), 270, "spread from E -> 270")

ffmc <- fuel_moisture_mcarthur(38.9, 10.6)
check_true(ffmc > 0 && ffmc < 30, "McArthur FFMC in valid range")

# ============================================================
#  2. Drought factor
# ============================================================
section("Drought factor")

df1 <- drought_factor(100, 10, 5)
check_true(df1 > 0 && df1 <= 10, "drought_factor bounded 0-10")

df_cap <- drought_factor(200, 30, 0)
check(df_cap, 10, "drought_factor capped at 10")

# ============================================================
#  3. Slope utilities
# ============================================================
section("Slope utilities")

check(slope_factor(0), 1, "slope_factor(0) = 1")
check(slope_factor(10), exp(0.069 * 10), "slope_factor(10)")

s_deg <- slope_from_rise(100, 200)
check(round(s_deg, 2), 26.57, "slope_from_rise(100, 200)")

s_pct <- slope_convert(45, from = "degrees")
check(round(s_pct, 1), 100, "slope_convert 45 deg -> 100%")

s_back <- slope_convert(100, from = "percent")
check(round(s_back, 0), 45, "slope_convert 100% -> 45 deg")

dist <- slope_template_distance(20, 10)
check_true(dist > 0, "slope_template_distance > 0")

solved <- slope_solve(30, rise = 100)
check(round(solved, 2), 173.21, "slope_solve(30, rise=100)")

# ============================================================
#  4. McArthur Mk5 Forest
# ============================================================
section("McArthur Mk5 Forest")

mk5 <- forest_mcarthur_mk5(
  temp = 38.9, rh = 10.6, wind_speed = 32,
  drought_factor = 9.9, fuel_load = 15,
  slope = 0, wind_reduction = 3
)

fdi <- mcarthur_fdi(9.9, 10.6, 38.9, 32, 3)
check(fdi, mk5$fdi, "mcarthur_fdi matches forest_mcarthur_mk5$fdi")
check_true(mk5$fdi > 0, "FDI > 0")
check_true(mk5$ros > 0, "ROS > 0")
check_true(mk5$flame_height > 0, "Flame height > 0")
check_true(mk5$intensity > 0, "Fireline intensity > 0")
check(mk5$flank_ros < mk5$ros, TRUE, "Flank ROS < head ROS")
check(mk5$spotting_dist >= 0, TRUE, "Spotting distance >= 0")
check_true(mk5$ffmc > 0 && mk5$ffmc < 30, "FFMC in valid range")

mk5_slope <- forest_mcarthur_mk5(
  temp = 38.9, rh = 10.6, wind_speed = 32,
  drought_factor = 9.9, fuel_load = 15,
  slope = 10, wind_reduction = 3
)
check_true(mk5_slope$ros > mk5$ros, "Upslope increases ROS")

# ============================================================
#  5. Leaflet 80 Control Burning
# ============================================================
section("Leaflet 80")

l80 <- forest_leaflet80(
  temp = 25, rh = 40, wind_speed = 10,
  drought_factor = 5, fuel_load = 10,
  slope = 0, wind_reduction = 3,
  time_hour = 14, days_since_rain = 5, rain_amount = 10
)

check_true(l80$fmc > 0, "Leaflet80 FMC > 0")
check_true(l80$ros_slope > 0, "Leaflet80 ROS > 0")
check_true(l80$fuel_avail > 0 && l80$fuel_avail <= 1, "Fuel availability 0-1")
check_true(!l80$out_of_range, "Leaflet80 in range for T=25, RH=40")

l80_oor <- forest_leaflet80(
  temp = 40, rh = 15, wind_speed = 10,
  drought_factor = 5, fuel_load = 10
)
check_true(l80_oor$out_of_range, "Leaflet80 out of range for T=40, RH=15")

# ============================================================
#  6. VESTA Dry Eucalypt Forest
# ============================================================
section("VESTA")

vesta <- forest_vesta(
  temp = 38.9, rh = 10.6, wind_speed = 32, slope = 0,
  surface_score = 4, near_surface_score = 3.5,
  near_surface_height = 25, elevated_score = 2,
  elevated_height = 1.5, bark_score = 3.5,
  month = 2, hour = 13
)

check(vesta$flank_ros, 2204.43979502205, "VESTA flank ROS (workbook)")
check_true(vesta$ros > 0, "VESTA head ROS > 0")
check_true(vesta$ros > vesta$flank_ros, "VESTA head > flank")
check_true(vesta$fuel_moisture > 0, "VESTA fuel moisture > 0")

# ============================================================
#  7. CSIRO Grassland
# ============================================================
section("CSIRO Grassland")

gr <- grass_fire(
  temp = 38.9, rh = 10.6, wind_speed = 32,
  curing = 90, slope = 0
)

fdi_g <- grass_fdi(90, 38.9, 32, 10.6)
check(fdi_g, gr$fdi, "grass_fdi matches grass_fire$fdi")
check_true(gr$fdi > 0, "Grassland FDI > 0")
check_true(gr$natural$ros > 0, "Natural grass ROS > 0")
check_true(gr$grazed$ros > 0, "Grazed grass ROS > 0")
check_true(gr$eaten_out$ros >= 0, "Eaten-out grass ROS >= 0")
check_true(gr$natural$ros > gr$grazed$ros, "Natural ROS > Grazed ROS")
check_true(gr$grazed$ros > gr$woodland$ros, "Grazed ROS > Woodland ROS")
check_true(gr$natural$flame_height > 0, "Grass flame height > 0")
check_true(gr$cure_coeff > 0 && gr$cure_coeff <= 1, "Curing coefficient 0-1")

gr_slope <- grass_fire(
  temp = 38.9, rh = 10.6, wind_speed = 32,
  curing = 90, slope = 10
)
check_true(gr_slope$natural$ros > gr$natural$ros, "Slope increases grass ROS")

# ============================================================
#  8. Heathland (Anderson et al. 2015)
# ============================================================
section("Heathland")

ht <- heath_fire(
  temp = 38.9, rh = 10.6, wind_speed = 32,
  veg_height = 1.5, slope = 0
)

check_true(ht$dead_fmc > 0, "Heath dead FMC > 0")
check_true(ht$heath_ros > 0, "Heath ROS > 0")
check_true(ht$woodland_ros > 0, "Woodland ROS > 0")
check_true(ht$heath_ros > ht$woodland_ros,
           "Heath ROS > Woodland ROS (less sheltered)")
check_true(ht$heath_intensity > 0, "Heath fireline intensity > 0")

# ============================================================
#  9. Buttongrass (Marsden-Smedley 2003)
# ============================================================
section("Buttongrass")

bg <- buttongrass_fire(
  temp = 38.9, rh = 10.6, wind_speed = 32,
  rain_amount = 0, hours_since_rain = 0,
  age = 60, cover = 0, productivity = 1, slope = 0
)

check(bg$dew_point, 2.60443141316762, "Buttongrass dew point (workbook)")
check(bg$fuel_moisture, 6.11528027886485, "Buttongrass FMC (workbook)")
check(bg$head_ros, 1922.00689245082, "Buttongrass head ROS (workbook)")
check(bg$prob_sustain, 0.999999941582946, "Buttongrass P(sustain) (workbook)")

check_true(bg$flank_ros < bg$head_ros, "Flank < Head")
check_true(bg$back_ros < bg$flank_ros, "Back < Flank")
check(bg$flank_ros, bg$head_ros * 0.4, "Flank = 0.4 * Head")
check(bg$back_ros, bg$head_ros * 0.1, "Back = 0.1 * Head")

fl <- buttongrass_fuel_load(age = 20, cover = 0, productivity = 1)
check_true(fl > 0, "Fuel load (low productivity) > 0")
fl2 <- buttongrass_fuel_load(age = 20, cover = 0, productivity = 2)
check_true(fl2 > fl, "Medium productivity > low productivity fuel load")

# ============================================================
# 10. WA Red Book
# ============================================================
section("WA Red Book")

bdu <- redbook_bdu(30, 30)
check_true(bdu > 0, "BDU > 0")

nwc_r <- redbook_nwc_rain(smc_prev = 10, rain_amount = 5, days_since_rain = 1)
check_true(nwc_r >= 0, "NWC rain >= 0")
nwc_r2 <- redbook_nwc_rain(smc_prev = 10, rain_amount = 5, days_since_rain = 3)
check(nwc_r2, 0, "NWC rain = 0 when days > 1")

nwc_d <- redbook_nwc_dry(smc_prev = 10, rh_count = 5, days_since_rain = 3)
check_true(nwc_d != 0, "NWC dry active when days > 1")
nwc_d2 <- redbook_nwc_dry(smc_prev = 10, rh_count = 5, days_since_rain = 1)
check(nwc_d2, 0, "NWC dry = 0 when days <= 1")

fdi_j <- redbook_fdi_jarrah(smc = 10, wind_speed = 20, wind_reduction = 3)
check_true(fdi_j > 0, "Jarrah FDI > 0 at SMC=10")
fdi_j0 <- redbook_fdi_jarrah(smc = 30, wind_speed = 20)
check(fdi_j0, 0, "Jarrah FDI = 0 when SMC > 27")

fdi_k <- redbook_fdi_karri(smc = 10, wind_speed = 20, wind_reduction = 3)
check_true(fdi_k > 0, "Karri FDI > 0 at SMC=10")
fdi_k0 <- redbook_fdi_karri(smc = 30, wind_speed = 20)
check(fdi_k0, 0, "Karri FDI = 0 when SMC > 27")

fqcf <- redbook_fqcf_jarrah(total_fuel = 5, smc = 10)
check_true(fqcf > 0, "Jarrah FQCF > 0")

aff <- redbook_aff_karri(smc = 10, profile_mc = 20)
check_true(aff >= 0 && aff <= 1, "Karri AFF in 0-1")

# ============================================================
# 11. WA Mallee (McCaw 1998)
# ============================================================
section("WA Mallee")

ml <- mallee_fire(wind_speed = 20, dead_fmc = 8, slope = 0)
check_true(ml$ros > 0, "Mallee ROS > 0")
check(ml$ros, ml$ros_slope, "Mallee flat = slope ROS at slope=0")

ml_s <- mallee_fire(wind_speed = 20, dead_fmc = 8, slope = 10)
check_true(ml_s$ros_slope > ml$ros, "Mallee slope ROS > flat ROS")

# ============================================================
# 12. Mallee-Heath (Cruz et al. 2013)
# ============================================================
section("Mallee-Heath")

mh <- mallee_heath_fire(
  temp = 35, rh = 15, wind_speed = 25,
  surface_fmc = 6, overstorey_cover = 30,
  overstorey_height = 4, surface_fuel_load = 8
)

check_true(mh$prob_go_10m >= 0 && mh$prob_go_10m <= 1,
           "P(go) 10m in 0-1")
check_true(mh$prob_crown >= 0 && mh$prob_crown <= 1,
           "P(crown) in 0-1")
check_true(mh$surface_ros_10m > 0, "Surface ROS 10m > 0")
check_true(mh$ros > 0, "Overall ROS > 0")
check_true(mh$intensity > 0, "Intensity > 0")
check_true(mh$flame_height > 0, "Flame height > 0")

# ============================================================
# 13. Suppression
# ============================================================
section("Suppression")

h <- hand_trail_rate(6, slope = 10, "High", flame_height = 0.7)
check_true(h > 0, "Hand trail rate > 0")

d4 <- d4_dozer_rate(2, slope = 10, "Some", flame_height = 1.2)
check_true(d4 > 0, "D4 dozer rate > 0")

d6 <- d6_dozer_rate(1, slope = 5, "Significant", flame_height = 2)
check_true(d6 > 0, "D6 dozer rate > 0")

tnk <- tanker_rate(2, "Good", "Moderate", flame_height = 1, wind_speed = 15)
check_true(tnk > 0, "Tanker rate > 0")

fb <- firebomber_rate(1, turnaround = 20, "1400L Medium Helicopter")
check_true(fb > 0, "Firebomber rate > 0")

total <- combined_suppression_rate(hand = h, d4 = d4, d6 = d6,
                                   tanker = tnk, bomber1 = fb)
check(total, h + d4 + d6 + tnk + fb, "Combined = sum of components")

# ============================================================
#  Summary
# ============================================================
cat(sprintf("\n============================\n"))
cat(sprintf("  %d passed, %d failed\n", pass, fail))
cat(sprintf("============================\n"))
if (fail > 0) quit(status = 1)
