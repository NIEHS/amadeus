# NASA SEDAC Population Density

This article demonstrates a compact workflow for the NASA SEDAC
population-density rasters. These downloads require a NASA EarthData
token.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

`download_data(dataset_name = "sedac_population", ...)` wraps
[`download_population()`](https://niehs.github.io/amadeus/dev/reference/download_population.md).

- Inputs are global GPW population-density rasters keyed by `year`;
  request a single year or `year = "all"` for the all-years product.
- `data_resolution` supports `"30 second"`, `"2.5 minute"`,
  `"15 minute"`, `"30 minute"`, and `"60 minute"`. The all-years product
  is not available at 30-second resolution, so `amadeus` automatically
  falls back to 2.5-minute resolution in that case.
- `data_format` accepts `"GeoTIFF"`, `"ASCII"`, or `"netCDF"`. The
  all-years product is only available as netCDF and will be downloaded
  that way even if another format is requested.
- Downloads are global rather than extent-based; crop to your study area
  during processing.
- Files arrive as zip archives, with `unzip` and `remove_zip`
  controlling archive handling. Access may require NASA EarthData
  authentication, and `amadeus` will use `NASA_EARTHDATA_TOKEN` when
  needed.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "population_workflow")
download_data(
  dataset_name = "sedac_population",
  year = "2020",
  data_format = "GeoTIFF",
  data_resolution = "15 minute",
  directory_to_save = directory_to_save,
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = FALSE
)
```

## Process one workflow-ready data product

``` r

processed_data <- process_covariates(
  covariate = "population",
  path = list.files(
    directory_to_save,
    pattern = "\\.tif$",
    recursive = TRUE,
    full.names = TRUE
  )[1]
)
plot(log10(processed_data))
```

## Calculate covariates at points

``` r

df <- data.frame(
  site_id = c("site_1", "site_2", "site_3","site_4"),
  lon = c(-157.8583, -155.5319, -155.5828,-159.5),
  lat = c(21.3069, 19.5, 19.8968, 22.0)
)
example_points_sf <- sf::st_as_sf(
  df,
  coords = c("lon", "lat"),
  crs = 4326
)


point_values <- calculate_covariates(
  covariate = "population",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 1000,
  fun = "mean",
  geom = "sf"
)

print(point_values)
```
