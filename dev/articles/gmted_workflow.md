# USGS GMTED2010

This article demonstrates a compact workflow for GMTED2010 terrain
products.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

Each workflow uses two small example surfaces: `example_points_sf`, a
saved subset of AQS monitor locations from
`tests/testdata/aqs/aqs-location-sample.rds`, for point extraction; and
the packaged Durham County Uber H3 resolution-8 hexagons at
`system.file("extdata", "data_files", "durham_h3_res8.rds", package = "amadeus")`
for polygon extraction.

## Available inputs and data availability

- `download_data(dataset_name = "gmted", ...)` accepts `statistic`
  values of `"Breakline Emphasis"`, `"Systematic Subsample"`,
  `"Median Statistic"`, `"Minimum Statistic"`, `"Mean Statistic"`,
  `"Maximum Statistic"`, and `"Standard Deviation Statistic"`.
- Available `resolution` values are `"7.5 arc-seconds"`,
  `"15 arc-seconds"`, and `"30 arc-seconds"`.
- GMTED2010 is a static terrain reference, so the wrapper has no year or
  scenario arguments; downloads arrive as zipped elevation grid bundles.
- Pick the statistic intentionally: each layer represents a different
  summary of elevation, so values from different statistics should not
  be treated as interchangeable.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "gmted_workflow")
download_data(
  dataset_name = "gmted",
  statistic = "Breakline Emphasis",
  resolution = "30 arc-seconds",
  directory_to_save = directory_to_save,
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = FALSE
)
```

## Process one workflow-ready data product

``` r

gmted_dirs <- list.dirs(directory_to_save, recursive = TRUE, full.names = TRUE)
gmted_path <- gmted_dirs[grepl("be30_grd$", gmted_dirs)][1]
processed_data <- process_covariates(
  covariate = "gmted",
  variable = c("Breakline Emphasis", "30 arc-seconds"),
  path = gmted_path,
  extent = terra::ext(-79.2, -78.6, 35.8, 36.3)
)
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
  covariate = "gmted",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  fun = "mean",
  geom = "sf"
)
```

## Calculate covariates across Durham County H3 hexagons

``` r

polygon_values <- calculate_covariates(
  covariate = "gmted",
  from = processed_data,
  locs = durham_hex,
  locs_id = "h3_id",
  radius = 0,
  fun = "mean",
  geom = "sf"
)

print(polygon_values)
```

## Visualize the point outputs

``` r

ggplot(data = point_values) + geom_sf(aes(color = `_00000`))
```
