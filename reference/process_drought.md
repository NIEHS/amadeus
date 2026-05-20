# Process drought index data

The `process_drought()` function imports and cleans raw drought index
files returned by
[`download_drought()`](https://niehs.github.io/amadeus/reference/download_drought.md),
producing a harmonized output object ready for
[`calculate_drought()`](https://niehs.github.io/amadeus/reference/calculate_drought.md):

- **SPEI / EDDI** — returns a `SpatRaster` with one layer per time step,
  layer names in `"<source>_<timescale>_YYYY-MM-DD"` format, CRS set to
  `EPSG:4326`.

- **USDM** — returns a `SpatVector` (polygon) with columns `DM`
  (drought-monitor class, integer 0–4), `date` (`Date`), and `source`
  (`"usdm"`), CRS `EPSG:4326`.

## Usage

``` r
process_drought(
  source = c("spei", "eddi", "usdm"),
  path = NULL,
  date = c("2020-01-01", "2020-12-31"),
  timescale = 1L,
  extent = NULL,
  ...
)
```

## Arguments

- source:

  character(1). Drought data source. One of `"spei"`, `"eddi"`, or
  `"usdm"`. When called through `process_covariates(covariate = "spei")`
  the alias is forwarded automatically.

- path:

  character(1). Directory containing downloaded drought files (output of
  [`download_drought()`](https://niehs.github.io/amadeus/reference/download_drought.md)).

- date:

  character(1 or 2). Single date or start/end dates. Format
  `"YYYY-MM-DD"`.

- timescale:

  integer(1). Accumulation timescale in months (SPEI/EDDI only; ignored
  for USDM). Must match the timescale used in
  [`download_drought()`](https://niehs.github.io/amadeus/reference/download_drought.md).
  Default `1L`.

- extent:

  numeric(4) or `SpatExtent`. Optional spatial crop applied before
  returning. `NULL` (default) returns full extent.

- ...:

  Reserved for future use; currently ignored.

## Value

- `SpatRaster` for SPEI or EDDI sources.

- `SpatVector` (polygons) for USDM source.

## Note

- SPEI/EDDI files are expected to follow the naming convention produced
  by
  [`download_drought()`](https://niehs.github.io/amadeus/reference/download_drought.md):
  `spei<timescale>.nc` and either legacy `eddi<timescale>mn<year>.nc` or
  current `EDDI_ETrs_<timescale>mn_<YYYYMMDD>.asc`.

- USDM files are expected to be weekly shapefiles named
  `USDM_<YYYYMMDD>.shp`.

- Layer/column naming is standardised so that
  [`calculate_drought()`](https://niehs.github.io/amadeus/reference/calculate_drought.md)
  can operate identically regardless of source.

## See also

[`download_drought`](https://niehs.github.io/amadeus/reference/download_drought.md),
[`calculate_drought`](https://niehs.github.io/amadeus/reference/calculate_drought.md)

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
## SPEI
spei <- process_drought(
  source = "spei",
  path = "./data/drought",
  date = c("2020-01-01", "2020-12-31"),
  timescale = 1L
)
## USDM
usdm <- process_drought(
  source = "usdm",
  path = "./data/drought",
  date = c("2020-01-07", "2020-03-31")
)
} # }
```
