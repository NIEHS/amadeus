# NOAA GOES Aerosol Detection Product (ADP)

This article demonstrates a compact workflow for NOAA GOES Aerosol
Detection Product (ADP) data.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Data description

GOES ADP is a geostationary satellite aerosol detection product that
reports smoke and dust presence from ABI observations.

| Component | Description |
|----|----|
| Products | `ADP-C` (CONUS), `ADP-F` (Full Disk), `ADP-M` (Mesoscale) |
| Variables available in `process_covariates(covariate = "goes", ...)` | `Smoke`, `Dust` |
| Spatial domain | GOES-East (`satellite = "16"`) and GOES-West (`satellite = "18"`) viewing domains; sector depends on product (`C`, `F`, `M`) |
| Spatial resolution | Native GOES fixed-grid product (approximately 2 km at nadir; effective footprint increases away from nadir) |
| Temporal domain | Daily archive by UTC day (`YYYY-MM-DD`), with intra-day granules available throughout the day |
| Temporal resolution | High-frequency geostationary updates (sector-dependent cadence; often every few minutes for CONUS/Full Disk and faster for Mesoscale) |
| Output format | NetCDF (`.nc`) files from NOAA Open Data S3 |

## Available inputs and data availability

`download_data(dataset_name = "goes", ...)` wraps
[`download_goes()`](https://niehs.github.io/amadeus/dev/reference/download_goes.md).

- `satellite` accepts `"16"` (GOES-East) or `"18"` (GOES-West).
- `product` accepts `"ADP-C"` (CONUS), `"ADP-F"` (Full Disk), or
  `"ADP-M"` (Mesoscale).
- `date` accepts either a single day or a start/end range in
  `YYYY-MM-DD` format.
- GOES ADP does not require authentication.
- `process_covariates(covariate = "goes", ...)` supports
  `variable = "Smoke"` or `"Dust"`.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "goes_workflow")
download_goes(
  date = c("2024-01-01","2024-01-01"),
  satellite = "16",
  product = "ADP-C",
  directory_to_save = directory_to_save,
  acknowledgement = TRUE
)
```

## Process one workflow-ready data product

``` r

goes_path <- file.path(directory_to_save, "ABI-L2-ADPC", "2024", "001")

processed_mean <- process_covariates(
  covariate = "goes",
  variable = "Smoke",
  date = "2024-01-01",
  path = goes_path,
  daily_agg = TRUE,
  fun = "mean"
)

processed_sum <- process_covariates(
  covariate = "goes",
  variable = "Smoke",
  date = "2024-01-01",
  path = goes_path,
  daily_agg = TRUE,
  fun = "sum"
)
```

### Choosing a daily aggregation

Use `fun = "mean"` when you want a daily field that behaves like a
fractional or probability-like surface. For GOES Smoke/Dust, the mean is
usually the better default for exposure-oriented summaries because it
represents the typical share of available granules indicating smoke or
dust at each pixel during the day.

Use `fun = "sum"` when you want to integrate granule-level detections
into a count-like daily total. This can be useful for identifying places
with repeated detections across the day, but it is a coarse
approximation because GOES granule cadence varies by sector, satellite
operations, and data availability.

For most epidemiologic or environmental exposure workflows, start with
`fun = "mean"` unless your analysis specifically needs a
detection-frequency metric.

> **Computational notes:** For memory, runtime, and parallel-processing
> guidance, see
> [`vignette("computational_considerations", package = "amadeus")`](https://niehs.github.io/amadeus/dev/articles/computational_considerations.md).

## Calculate covariates at points

``` r

domain_x <- c(terra::xmin(processed_mean), terra::xmax(processed_mean))
domain_y <- c(terra::ymin(processed_mean), terra::ymax(processed_mean))
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
  covariate = "goes",
  from = processed_mean,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  fun = "mean",
  geom = "sf"
)
```

## Visualize the point outputs

``` r

smoke_col <- grep("^Smoke_", names(point_values), value = TRUE)[1]
if (is.na(smoke_col) || smoke_col == "") {
  excluded <- c("site_id", "h3_id", attr(point_values, "sf_column"))
  fallback_cols <- setdiff(names(point_values), excluded)
  fallback_numeric <- fallback_cols[
    vapply(point_values[fallback_cols], is.numeric, logical(1))
  ]
  smoke_col <- fallback_numeric[1]
}

ggplot(data = point_values) +
  geom_sf(aes(color = .data[[smoke_col]])) +
  ggtitle("Daily GOES Smoke values")
```

## Polygon (areal) extraction example

The same processed daily raster can be summarized over polygon features.
This example builds two small synthetic rectangles from the processed
raster extent and extracts the mean daily Smoke value for each area.

``` r

poly_x <- domain_x[1] + c(0.20, 0.35, 0.50) * domain_dx
poly_y <- domain_y[1] + c(0.20, 0.40, 0.60) * domain_dy
make_rect <- function(xmin, xmax, ymin, ymax) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)
  )))
}

example_polys_sf <- sf::st_sf(
  area_id = c("area_1", "area_2"),
  geometry = sf::st_sfc(
    make_rect(poly_x[1], poly_x[2], poly_y[1], poly_y[2]),
    make_rect(poly_x[2], poly_x[3], poly_y[2], poly_y[3]),
    crs = 4326
  )
)

polygon_values <- calculate_covariates(
  covariate = "goes",
  from = processed_mean,
  locs = example_polys_sf,
  locs_id = "area_id",
  radius = 0,
  fun = "mean",
  geom = "sf"
)
polygon_values
```
