# NOAA North American Regional Reanalysis (NARR)

This article demonstrates a compact workflow for NOAA NARR data.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

Each workflow uses two small example surfaces: `example_points_sf`, a
set of real Triangle-region AQS monitoring locations derived from
`tests/testdata/aqs/aqs_daily_88101_triangle.csv`, for point extraction;
and the packaged Durham County Uber H3 resolution-8 hexagons at
`system.file("extdata", "data_files", "durham_h3_res8.rds", package = "amadeus")`
for polygon extraction.

## Available inputs and data availability

`download_data(dataset_name = "narr", ...)` expects NARR variable
abbreviations.

- Temporal resolution: daily meteorology from the NOAA PSL NARR Dailies
  archive.
- Year input: use a single year (for example, `2020`) or a start/end
  pair such as `c(2020, 2022)`; the wrapper supports years from 1979
  through the current calendar year.
- Major constraints:
  - Pressure-level variables (`air`, `hgt`, `omega`, `shum`, `tke`,
    `uwnd`, `vwnd`) always download all 29 pressure levels from 1000 to
    100 hPa.
  - Subsurface variables (`soill`, `soilw`, `tsoil`) include all 4 soil
    layers.
  - Variables that are only available in the raw merged GRIB archive are
    not supported by this wrapper.

### Monolevel variables

| variable   | description                                                  |
|:-----------|:-------------------------------------------------------------|
| acpcp      | Convective precipitation                                     |
| air.2m     | Air temperature at 2 m                                       |
| air.sfc    | Air temperature at surface                                   |
| albedo     | Surface albedo                                               |
| apcp       | Total accumulated precipitation                              |
| bgrun      | Baseflow-groundwater runoff                                  |
| bmixl.hl1  | Blackadar mixing length scale at hybrid level 1              |
| cape       | Convective available potential energy                        |
| ccond      | Canopy conductance                                           |
| cdcon      | Convective cloud cover                                       |
| cdlyr      | Non-convective cloud cover                                   |
| cfrzr      | Categorical freezing rain                                    |
| cicep      | Categorical ice pellets                                      |
| cin        | Convective inhibition                                        |
| cnwat      | Plant canopy surface water                                   |
| crain      | Categorical rain                                             |
| csnow      | Categorical snow                                             |
| dlwrf      | Downward longwave radiation flux                             |
| dpt.2m     | Dew point temperature at 2 m                                 |
| dswrf      | Downward shortwave radiation flux                            |
| evap       | Evaporation                                                  |
| gflux      | Ground heat flux                                             |
| hcdc       | High cloud cover                                             |
| hgt.tropo  | Geopotential height at tropopause                            |
| hlcy       | Storm relative helicity                                      |
| hpbl       | Planetary boundary layer height                              |
| lcdc       | Low cloud cover                                              |
| lftx4      | Best (4-layer) lifted index                                  |
| lhtfl      | Latent heat net flux                                         |
| mcdc       | Mid-cloud cover                                              |
| mconv.hl1  | Horizontal moisture divergence at hybrid level 1             |
| mslet      | Mean sea level pressure (ETA model reduction)                |
| mstav      | Moisture availability                                        |
| pevap      | Potential evaporation                                        |
| pottmp.hl1 | Potential temperature at hybrid level 1                      |
| pottmp.sfc | Potential temperature at surface                             |
| prate      | Precipitation rate                                           |
| pres.sfc   | Surface pressure                                             |
| pres.tropo | Pressure at tropopause                                       |
| prmsl      | Pressure reduced to mean sea level                           |
| pr_wtr     | Precipitable water                                           |
| rcq        | Specific humidity tendency from all physics                  |
| rcs        | Snowfall water equivalent tendency                           |
| rcsol      | Solar radiative heating rates                                |
| rct        | Temperature tendency from all physics                        |
| rhum.2m    | Relative humidity at 2 m                                     |
| shtfl      | Sensible heat net flux                                       |
| shum.2m    | Specific humidity at 2 m                                     |
| snod       | Snow depth                                                   |
| snohf      | Snow phase-change heat flux                                  |
| snom       | Snow melt                                                    |
| snowc      | Snow cover                                                   |
| soilm      | Soil moisture content (0-200 cm layer)                       |
| ssrun      | Storm surface runoff                                         |
| tcdc       | Total cloud cover                                            |
| tke.hl1    | Turbulent kinetic energy at hybrid level 1                   |
| ulwrf.ntat | Upward longwave radiation flux at nominal top of atmosphere  |
| ulwrf.sfc  | Upward longwave radiation flux at surface                    |
| ustm       | U-component of storm motion                                  |
| uswrf.ntat | Upward shortwave radiation flux at nominal top of atmosphere |
| uswrf.sfc  | Upward shortwave radiation flux at surface                   |
| uwnd.10m   | U-component of wind at 10 m                                  |
| veg        | Vegetation fraction                                          |
| vis        | Visibility                                                   |
| vstm       | V-component of storm motion                                  |
| vvel.hl1   | Vertical velocity at hybrid level 1                          |
| vwnd.10m   | V-component of wind at 10 m                                  |
| vwsh.tropo | Vertical wind shear at tropopause                            |
| wcconv     | Convective wetting of vegetation canopy                      |
| wcinc      | Wetting of vegetation canopy                                 |
| wcuflx     | U-component of convective canopy moisture flux               |
| wcvflx     | V-component of convective canopy moisture flux               |
| weasd      | Water-equivalent accumulated snow depth                      |
| wvconv     | Convective column moisture convergence                       |
| wvinc      | Column moisture increase                                     |
| wvuflx     | U-component of vertically-integrated moisture flux           |
| wvvflx     | V-component of vertically-integrated moisture flux           |

### Pressure level variables

| variable | description                          |
|:---------|:-------------------------------------|
| air      | Air temperature                      |
| hgt      | Geopotential height                  |
| omega    | Vertical velocity (pressure / omega) |
| shum     | Specific humidity                    |
| tke      | Turbulent kinetic energy             |
| uwnd     | U-component of wind                  |
| vwnd     | V-component of wind                  |

### Subsurface (soil) variables

| variable | description                                           |
|:---------|:------------------------------------------------------|
| soill    | Liquid volumetric soil moisture (non-frozen fraction) |
| soilw    | Volumetric soil moisture content                      |
| tsoil    | Soil temperature                                      |

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "narr_workflow")
download_data(
  dataset_name = "narr",
  variables = c("dpt.2m", "air.2m"),
  year = 2020,
  directory_to_save = directory_to_save,
  acknowledgement = TRUE
)
```

## Process one workflow-ready data product

``` r

processed_data <- process_covariates(
  covariate = "narr",
  variable = "air.2m",
  date = c("2020-01-01", "2020-01-10"),
  path = file.path(directory_to_save, "air.2m")
)
```

## Calculate covariates at points

``` r

hex_values <- calculate_covariates(
  covariate = "narr",
  from = processed_data,
  locs = durham_hex,
  locs_id = "h3_id",
  radius = 0,
  fun = "mean",
  geom = "sf"
)
print(hex_values)
```

## Visualize the data and point outputs

``` r

usa <- sf::st_as_sf(maps::map("usa", plot = FALSE, fill = TRUE))
bb <- sf::st_bbox(usa)

# Project first layer to WGS84 and convert to data frame for safe ggplot2 rendering
lyr1 <- terra::project(processed_data[[1]], "EPSG:4326")
lyr1_df <- as.data.frame(lyr1, xy = TRUE)
names(lyr1_df)[3] <- "value"

ggplot2::ggplot() +
  ggplot2::geom_raster(
    data = lyr1_df,
    ggplot2::aes(x = x, y = y, fill = value)
  ) +
  ggplot2::geom_sf(data = usa, fill = NA, color = "grey80", linewidth = 0.3) +
  ggplot2::scale_fill_viridis_c(option = "C", na.value = "white") +
  ggplot2::coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], expand = FALSE) +
  ggplot2::labs(x = NULL, y = NULL, fill = "air temp at 2m (K)") +
  ggplot2::theme_minimal()
```

We plot the NARR 2m air temperature at the Uber H3 hexagons. NARR is
fairly coarse spatial resolution at ~32sq-km, so we don’t see much
spatial variability, but the facets show the daily trend

``` r


ggplot2::ggplot() +
ggplot2::geom_sf(data = hex_values, ggplot2::aes(fill = air.2m_0)) +
ggplot2::scale_fill_viridis_c() +
ggplot2::facet_wrap(~time)
```

## Notes

- The download request now covers both snow water equivalent (`weasd`)
  and 2m air temperature (`air.2m`).
- NARR uses projected source data internally, so the vignette leaves
  reprojection to `amadeus` and keeps the extraction geometry in WGS84
  input coordinates.
