# Climatology Lab TerraClimate

This article demonstrates a compact workflow for Climatology Lab
TerraClimate data.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

`download_data(dataset_name = "terraclimate", ...)` accepts either the
full variable names below or their TerraClimate codes.

| Code   | Variable                                |
|--------|-----------------------------------------|
| `aet`  | Actual Evapotranspiration               |
| `def`  | Climate Water Deficit                   |
| `pet`  | Potential evapotranspiration            |
| `ppt`  | Precipitation                           |
| `q`    | Runoff                                  |
| `soil` | Soil Moisture                           |
| `srad` | Downward surface shortwave radiation    |
| `swe`  | Snow water equivalent - at end of month |
| `tmax` | Max Temperature                         |
| `tmin` | Min Temperature                         |
| `vap`  | Vapor pressure                          |
| `ws`   | Wind speed                              |
| `vpd`  | Vapor Pressure Deficit                  |
| `PDSI` | Palmer Drought Severity Index           |

- Temporal resolution: monthly; each download is an annual NetCDF file
  containing monthly layers for one variable.
- Year input: use a single year or a start/end pair such as
  `c(2018, 2022)`.
- Availability check: the wrapper validates the first requested
  variable-year URL and stops if that request returns HTTP 404.
- Major constraint: TerraClimate downloads do not require
  authentication.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "terraclimate_workflow")
download_data(
  dataset_name = "terraclimate",
  variables = c("Precipitation"),
  year = 2019,
  directory_to_save = directory_to_save,
  acknowledgement = TRUE
)
```

## Process one workflow-ready data product

``` r

processed_data <- process_covariates(
  covariate = "terraclimate",
  variable = "ppt",
  date = c("2019-01-01", "2019-02-01"),
  path = dirname(list.files(
    directory_to_save,
    pattern = "\\.nc$",
    recursive = TRUE,
    full.names = TRUE
  )[1]),
  extent = terra::ext(-114.9, -102.0, 31.3, 41.1)
)

terra::plot(processed_data, main = "TerraClimate ppt for Jan-Feb 2019")
```

## Calculate covariates at points

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
  covariate = "terraclimate",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  fun = "mean",
  geom = "sf"
)

print(point_values)
```

## Workflow for an annual average covariate

``` r

annual_process <- process_covariates(
  covariate = "terraclimate",
  variable = "ppt",
  date = c("2019-01-01", "2019-12-31"),
  path = dirname(list.files(
    directory_to_save,
    pattern = "\\.nc$",
    recursive = TRUE,
    full.names = TRUE
  )[1]),
  extent = terra::ext(-114.9, -102.0, 31.3, 41.1)
)

point_year <- calculate_covariates(
  covariate = "terraclimate",
  from = annual_process,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  .by_time = "year",
  fun = "mean",
  geom = "sf"
)

print(point_year)
```
