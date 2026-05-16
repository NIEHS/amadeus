# Process meteorological and atmospheric data

The `process_merra2()` function imports and cleans raw atmospheric,
meteorological, and MERRA2-based Fire Weather Index data, returning a
single `SpatRaster` object.

## Usage

``` r
process_merra2(
  date = c("2018-01-01", "2018-01-10"),
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

  character(1 or 2). Date (1) or start and end dates (2). Format
  YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").

- variable:

  character(1). MERRA2 variable name(s). For daily corrected Fire
  Weather Index files (`collection = "fwi"` during download), use one of
  `"DC"`, `"DMC"`, `"FFMC"`, `"ISI"`, `"BUI"`, or `"FWI"` (or the full
  raw layer name).

- path:

  character(1). Directory with downloaded netCDF (`.nc4` or `.nc`)
  files.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- daily_agg:

  logical(1). If `TRUE`, aggregate sub-daily layers to daily values
  using `fun`. Default `FALSE` preserves the original sub-daily output.
  Aggregation groups layers by variable/level and date. Silently ignored
  for FWI collections, which are already daily.

- fun:

  character(1). Aggregation function passed to
  [`terra::tapp()`](https://rspatial.github.io/terra/reference/tapp.html)
  (e.g. `"mean"`, `"max"`, `"min"`, `"sum"`). Ignored when
  `daily_agg = FALSE`.

- ...:

  Placeholders.

## Value

a `SpatRaster` object;

## Note

Layer names of the returned `SpatRaster` object contain the variable,
pressure level, date, and hour for standard MERRA-2 collections when
`daily_agg = FALSE` (default). When `daily_agg = TRUE`, layer names
contain the variable, pressure level, and date only, and
[`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
is set to midnight UTC of each date. For daily Fire Weather Index files,
layer names contain the variable and date only regardless of
`daily_agg`. Pressure level values utilized for layer names are taken
directly from raw data and are not edited to retain pressure level
information.

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
merra2 <- process_merra2(
  date = c("2024-01-01", "2024-01-10"),
  variable = "CPT",
  path = "./data/inst1_2d_int_Nx"
)
## daily mean CPT
merra2_daily <- process_merra2(
  date = c("2024-01-01", "2024-01-10"),
  variable = "CPT",
  path = "./data/inst1_2d_int_Nx",
  daily_agg = TRUE,
  fun = "mean"
)
} # }
```
