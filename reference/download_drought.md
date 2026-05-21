# Download drought index data

The `download_drought()` function downloads drought index data from
publicly available sources. Three source datasets are supported:

- **SPEI** (Standardized Precipitation-Evapotranspiration Index):
  Multi-year netCDF files by timescale from <https://spei.csic.es>.

- **EDDI** (Evaporative Demand Drought Index): Weekly raster files by
  timescale from
  <https://www.drought.gov/data-maps-tools/evaporative-demand-drought-index-eddi>.

- **USDM** (U.S. Drought Monitor): Weekly drought class shapefiles from
  <https://droughtmonitor.unl.edu>.

## Usage

``` r
download_drought(
  source = c("spei", "eddi", "usdm"),
  date = c("2020-01-01", "2020-12-31"),
  timescale = 1L,
  directory_to_save = NULL,
  acknowledgement = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 3L,
  rate_limit = 2,
  unzip = TRUE,
  remove_zip = FALSE,
  ...
)
```

## Arguments

- source:

  character(1). Drought data source. One of `"spei"`, `"eddi"`, or
  `"usdm"`.

- date:

  character(1 or 2). Single date or start/end dates. Format
  `"YYYY-MM-DD"`. For SPEI/EDDI the year component selects the annual
  file(s); for USDM the full date is used to select weekly release(s).

- timescale:

  integer(1). Accumulation timescale in months (SPEI/EDDI only; ignored
  for USDM). Typical values are 1, 3, 6, 12, 24, 48. Default is `1L`.

- directory_to_save:

  character(1). Directory to save downloaded data.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed.

- hash:

  logical(1). Return
  [`rlang::hash_file()`](https://rlang.r-lib.org/reference/hash.html)
  hash of downloaded files. Default `FALSE`.

- show_progress:

  logical(1). Show download progress bar. Default `TRUE`.

- max_tries:

  integer(1). Maximum retry attempts. Default `3L`.

- rate_limit:

  numeric(1). Minimum seconds between HTTP requests. Default `2`.

- unzip:

  logical(1). Unzip downloaded zip archives (USDM only). Default `TRUE`.

- remove_zip:

  logical(1). Remove zip archives after unzipping (USDM only). Default
  `FALSE`.

- ...:

  Reserved for future use; currently ignored.

## Value

`invisible(NULL)` when `hash = FALSE`; a character hash string when
`hash = TRUE`.

## Note

- SPEI and EDDI are raster products; USDM is a polygon product
  (shapefile). Their
  [`process_drought()`](https://niehs.github.io/amadeus/reference/process_drought.md)
  and
  [`calculate_drought()`](https://niehs.github.io/amadeus/reference/calculate_drought.md)
  handling differ accordingly.

- No authentication is required for any of these sources.

## See also

[`process_drought`](https://niehs.github.io/amadeus/reference/process_drought.md),
[`calculate_drought`](https://niehs.github.io/amadeus/reference/calculate_drought.md)

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_drought(
  source = "spei",
  date = c("2020-01-01", "2020-12-31"),
  timescale = 1L,
  directory_to_save = "./data/drought",
  acknowledgement = TRUE
)
download_drought(
  source = "usdm",
  date = c("2020-01-07", "2020-03-31"),
  directory_to_save = "./data/drought",
  acknowledgement = TRUE
)
} # }
```
