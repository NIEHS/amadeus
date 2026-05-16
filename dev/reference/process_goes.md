# Process NOAA GOES ADP data

The `process_goes()` function imports and cleans NOAA GOES-16/18 Aerosol
Detection Product (ADP) NetCDF files downloaded by
[`download_goes()`](https://niehs.github.io/amadeus/dev/reference/download_goes.md),
returning a single `SpatRaster` object with CRS `EPSG:4326`.

## Usage

``` r
process_goes(
  date = c("2024-01-01", "2024-01-01"),
  variable = NULL,
  path = NULL,
  extent = NULL,
  daily_agg = FALSE,
  fun = "mean",
  ...
)
```

## Arguments

- date:

  character(1 or 2). Date (YYYY-MM-DD) or start and end dates.

- variable:

  character(1). Variable name to extract: `"Smoke"` or `"Dust"`.

- path:

  character(1+). Directory with downloaded GOES ADP NetCDF files or a
  vector of full NetCDF file paths.

- extent:

  numeric(4) or SpatExtent. Crop extent (`xmin, xmax, ymin, ymax` in
  EPSG:4326). Default `NULL` loads the full raster.

- daily_agg:

  logical(1). If `TRUE`, aggregate sub-daily layers to daily values
  using `fun`. Default `FALSE` preserves original sub-daily layers.

- fun:

  character(1). Aggregation function passed to
  [`terra::tapp()`](https://rspatial.github.io/terra/reference/tapp.html)
  (e.g. `"mean"` or `"sum"`). Ignored when `daily_agg = FALSE`.

- ...:

  Placeholders.

## Value

a `SpatRaster` object

## Note

- Layer names follow the convention `{variable}_{YYYYMMDD}_{HHMMSS}`
  when `daily_agg = FALSE`, e.g. `"Smoke_20240101_000000"`. With
  `daily_agg = TRUE`, layer names contain `{variable}_{YYYYMMDD}` and
  [`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
  is set to midnight UTC.

- [`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
  is set to POSIXct UTC for each layer.

- Files with GOES geostationary projection are reprojected to EPSG:4326.

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires downloaded
##       data files.
if (FALSE) { # \dontrun{
goes <- process_goes(
  date = c("2024-01-01", "2024-01-01"),
  variable = "Smoke",
  path = "./data/goes/"
)
goes_daily <- process_goes(
  date = c("2024-01-01", "2024-01-01"),
  variable = "Smoke",
  path = "./data/goes/",
  daily_agg = TRUE,
  fun = "mean"
)
} # }
```
