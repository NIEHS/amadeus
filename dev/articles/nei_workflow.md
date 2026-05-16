# US EPA National Emissions Inventory (NEI)

This article demonstrates a compact workflow for county-level NEI
summaries.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

[`download_nei()`](https://niehs.github.io/amadeus/dev/reference/download_nei.md)
is intentionally narrow and currently targets EPA on-road NEI summary
releases:

- `year` is the main selector and currently supports the two NEI
  releases wired into `amadeus`: `2017` and `2020`.
- Requests can include one year or both available years, yielding one
  annual on-road summary file per year requested.
- NEI downloads arrive as zip archives and can be unzipped automatically
  with `unzip = TRUE`; the workflow then processes the extracted annual
  summary tables.
- No jurisdiction argument is needed because the source files are EPA
  by-region summaries rather than state-by-state download variants.
- NEI does not require authentication. Temporal resolution is annual.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "nei_workflow")
download_data(
  dataset_name = "nei",
  year = 2020L,
  directory_to_save = directory_to_save,
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = FALSE
)
```

## Process one workflow-ready data product

``` r

county <- tigris::counties("NC", year = 2020, cb = TRUE, class = "sf")
data_path = paste0(directory_to_save, "/data_files/2020nei_onroad_byregion/")
county$FIPS <- county$GEOID
processed_data <- process_covariates(
  covariate = "nei",
  path = data_path,
  county = county,
  year = 2020
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
  covariate = "nei",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  geom = "sf"
)

print(point_values, n = 25)
```

## Visualize the point and polygon outputs

``` r


ggplot() + geom_sf(data = county, color = NA) +
  geom_sf(data = point_values, aes(color = TRF_NEINP_0_00000), size = 2) +
  labs(title = "NEI On-Road Emissions by County (2020)", color = "Total Emissions") +
  theme_minimal()
```
