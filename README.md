# FireBehaviourCalcsR

Australian fire behaviour prediction models as an R package. Translated from
the *Advanced Fire Behaviour Prediction Standard Workbook* compiled by Kevin
Tolhurst (University of Melbourne, 2007-2016), originally distributed by NSW
Rural Fire Service.

## Models

| Model | Function | Reference |
|-------|----------|-----------|
| McArthur Mk5 Forest | `forest_mcarthur_mk5()` | Noble, Bary & Gill (1980) |
| McArthur Leaflet 80 | `forest_leaflet80()` | Gould (1994) |
| VESTA Dry Eucalypt Forest | `forest_vesta()` | Cheney et al. (2007) |
| CSIRO Grassland | `grass_fire()` | Cheney, Gould & Catchpole (1998); Cruz et al. (2015) |
| Heathland | `heath_fire()` | Anderson et al. (2015) |
| Buttongrass Moorland | `buttongrass_fire()` | Marsden-Smedley (2003) |
| WA Red Book (Jarrah/Karri) | `redbook_fdi_jarrah()`, `redbook_fdi_karri()` | Beck (1995) |
| WA Mallee | `mallee_fire()` | McCaw (1998) |
| Mallee-Heath | `mallee_heath_fire()` | Cruz et al. (2013) |

## Validation Notes

- WA Red Book outputs should be used with caution. In the original workbook,
  the `RedBook` sheet calculates `#DIV/0!` under the bundled example inputs
  and is marked "NEEDS FURTHER VALIDATION - USE WITH CAUTION".

## Utilities

- **Weather**: `dew_point()`, `rh_from_dewpoint()`, `rh_from_wetbulb()`,
  `wind_dir_to_degrees()`
- **Fuel moisture**: `fuel_moisture_mcarthur()`, `vesta_fuel_moisture()`,
  `drought_factor()`
- **Slope**: `slope_factor()`, `slope_from_rise()`, `slope_convert()`,
  `slope_template_distance()`
- **Suppression**: `hand_trail_rate()`, `d4_dozer_rate()`, `d6_dozer_rate()`,
  `tanker_rate()`, `firebomber_rate()`, `combined_suppression_rate()`

## Bundled Data

- `wind_reduction_factors` -- wind reduction factor guide by vegetation type
- `nsw_fuel_types` -- 68 NSW fuel type classifications with fuel load
  equation parameters

## Installation

```r
# install.packages("remotes")
remotes::install_github("sadassimov/FireBehaviourCalcsR")
```

## Usage

```r
library(FireBehaviourCalcsR)

# McArthur Mk5 forest fire behaviour
result <- forest_mcarthur_mk5(
  temp           = 38.9,
  rh             = 10.6,
  wind_speed     = 32,
  drought_factor = 9.9,
  fuel_load      = 15,
  slope          = 0,
  wind_reduction = 3
)
result$fdi        # Fire Danger Index
result$ros        # Rate of spread (m/h)
result$intensity  # Fireline intensity (kW/m)

# CSIRO grassland fire behaviour (five grass types)
grass <- grass_fire(
  temp       = 38.9,
  rh         = 10.6,
  wind_speed = 32,
  curing     = 90,
  slope      = 0
)
grass$natural$ros         # Natural grass ROS (m/h)
grass$grazed$flame_height # Grazed grass flame height (m)

# Dew point from temperature and RH
dew_point(temp = 34, rh = 20)  # 7.8 degrees C

# Suppression resources
hand  <- hand_trail_rate(6, slope = 10, "High", flame_height = 0.7)
dozer <- d4_dozer_rate(2, slope = 10, "Some", flame_height = 1.2)
combined_suppression_rate(hand = hand, d4 = dozer)
```

## References

- Anderson, W.R. et al. (2015). A generic, empirical-based model for
  predicting rate of fire spread in shrublands. *Int. J. Wildland Fire*.
- Beck, J.A. (1995). Equations for the forest fire behaviour tables for
  Western Australia. *CALMScience* 1(3):325-348.
- Cheney, N.P., Gould, J.S. & Catchpole, W.R. (1998). Prediction of fire
  spread in grasslands. *Int. J. Wildland Fire* 8(1):1-13.
- Cruz, M.G. et al. (2013). Fire behaviour in mallee-heath. *Environmental
  Modelling & Software* 40:21-34.
- Cruz, M.G. et al. (2015). Updated grass curing function.
- Gould, J.S. (1994). Evaluation of McArthur's control burning guide in
  regrowth *Eucalyptus sieberi* forest. *Aust. Forestry* 57:86-93.
- Marsden-Smedley, J.B. (2003). Buttongrass moorland fire behaviour model.
- McCarthy, G.J., Tolhurst, K.G. & Wouters, M. (2003). Research Report
  No.56, Fire Management. DSE Victoria.
- McCaw, W.L. (1998). Fire spread in WA mallee.
- Noble, I.R., Bary, G.A.V. & Gill, A.M. (1980). McArthur's fire danger
  meters expressed as equations. *Aust. J. Ecol.* 5:201-203.
- Tolhurst, K.G. (2007-2016). Advanced Fire Behaviour Prediction Standard
  Workbook.

## License

MIT
