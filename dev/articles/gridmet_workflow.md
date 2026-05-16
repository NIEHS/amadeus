# Climatology Lab gridMET

This article demonstrates a compact workflow for Climatology Lab
`gridMET` data using multiple variables, multiple summaries, and both
point and polygon extraction.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

`download_data(dataset_name = "gridmet", ...)` accepts either the full
variable names below or their gridMET codes.

| Code     | Variable                               |
|----------|----------------------------------------|
| `sph`    | Near-Surface Specific Humidity         |
| `vpd`    | Mean Vapor Pressure Deficit            |
| `pr`     | Precipitation                          |
| `rmin`   | Minimum Near-Surface Relative Humidity |
| `rmax`   | Maximum Near-Surface Relative Humidity |
| `srad`   | Surface Downwelling Solar Radiation    |
| `tmmn`   | Minimum Near-Surface Air Temperature   |
| `tmmx`   | Maximum Near-Surface Air Temperature   |
| `vs`     | Wind speed at 10 m                     |
| `th`     | Wind direction at 10 m                 |
| `pdsi`   | Palmer Drought Severity Index          |
| `pet`    | Reference grass evapotranspiration     |
| `etr`    | Reference alfalfa evapotranspiration   |
| `ERC`    | Energy Release Component               |
| `BI`     | Burning Index                          |
| `FM100`  | 100-hour dead fuel moisture            |
| `FM1000` | 1000-hour dead fuel moisture           |

- Temporal resolution: daily; each download is an annual NetCDF file for
  one variable.
- Year input: use a single year or a start/end pair such as
  `c(2018, 2022)`.
- Availability check: the wrapper validates the first requested
  variable-year URL and stops if that request returns HTTP 404.
- Major constraint: gridMET downloads do not require authentication.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "gridmet_workflow")
download_data(
  dataset_name = "gridmet",
  variables = c("tmmx"),
  year = 2020,
  directory_to_save = directory_to_save,
  acknowledgement = TRUE
)
```

## Process workflow-ready data product

``` r

processed_data <- process_covariates(
  covariate = "gridmet",
  variable = "tmmx",
  date = c("2020-01-01", "2020-01-31"),
  path = file.path(directory_to_save, "tmmx"),
  extent = terra::ext(-79.2, -78.6, 35.8, 36.3)
)
```

## Calculate covariates at points using native daily temporal resolution and use .by_time to calculate monthly means

``` r



domain_x <- c(terra::xmin(processed_data), terra::xmax(processed_data))
domain_y <- c(terra::ymin(processed_data), terra::ymax(processed_data))
domain_dx <- diff(domain_x)
domain_dy <- diff(domain_y)

candidate_xy <- expand.grid(
  lon = seq(domain_x[1] + 0.12 * domain_dx, domain_x[2] - 0.12 * domain_dx, length.out = 5),
  lat = seq(domain_y[1] + 0.12 * domain_dy, domain_y[2] - 0.12 * domain_dy, length.out = 5)
)
example_points_sf <- sf::st_as_sf(
  candidate_xy,
  coords = c("lon", "lat"),
  crs = 4326
)
example_points_sf$site_id <- paste0("site_", seq_len(nrow(example_points_sf)))


point_values <- calculate_covariates(
  covariate = "gridmet",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  fun = "mean",
  geom = "sf"
)

point_values_month <- calculate_covariates(
  covariate = "gridmet",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  .by_time = "month",
  radius = 0,
  fun = "mean",
  geom = "sf"
)

print(point_values)
print(point_values_month)
```

## Visualize the point outputs

``` r


ggplot(data = point_values) + geom_sf(aes(color = `tmmx_0`)) + ggtitle("Daily values")
```
