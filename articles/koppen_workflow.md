# Koppen-Geiger Climate Classes

This article demonstrates a compact workflow for Koppen-Geiger climate
classification rasters.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

- `download_data(dataset_name = "koppen", ...)` accepts
  `data_resolution = "0.0083"`, `"0.083"`, or `"0.5"`.
- `time_period` is either `"Present"` for the 1980-2016 classification
  or `"Future"` for the 2071-2100 classification.
- Downloads arrive as zipped climate-classification raster files; the
  wrapper exposes period-level products rather than individual years or
  scenarios.
- Köppen-Geiger outputs are categorical climate classes, so treat the
  raster values as class codes instead of a continuous climate surface.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "koppen_workflow")
download_data(
  dataset_name = "koppen",
  data_resolution = "0.5",
  time_period = "Present",
  directory_to_save = directory_to_save,
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = FALSE
)
```

## Process one workflow-ready data product

``` r

processed_data <- process_covariates(
  covariate = "koppen",
  path = list.files(
    directory_to_save,
    pattern = "\\.tif$",
    recursive = TRUE,
    full.names = TRUE
  )[1],
  extent = terra::ext(-114.9, -102.0, 31.3, 41.1)
)

plot(processed_data, main = "Koppen-Geiger Climate Classes (0.5° resolution, 1980-2016)")
```

## Calculate covariates at points. Demonstrate the dummy variable and fraction options for categorical covariates.

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
  covariate = "koppen",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  geom = "sf"
)

print(point_values, n = 25)

frac_values <- calculate_covariates(
  covariate = "koppen",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  frac = TRUE,
  radius = 100000,
  geom = "sf"
)

print(frac_values, n = 25)
```
