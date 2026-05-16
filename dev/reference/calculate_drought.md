# Calculate drought index covariates

The `calculate_drought()` function extracts drought index values at
point locations from an object returned by
[`process_drought()`](https://niehs.github.io/amadeus/dev/reference/process_drought.md).
Three source datasets are supported:

- **SPEI / EDDI** (`SpatRaster`): cell values are extracted at each
  location using the standard raster-extraction pipeline
  ([`calc_prepare_locs()`](https://niehs.github.io/amadeus/dev/reference/calc_prepare_locs.md)
  →
  [`calc_worker()`](https://niehs.github.io/amadeus/dev/reference/calc_worker.md)
  →
  [`calc_return_locs()`](https://niehs.github.io/amadeus/dev/reference/calc_return_locs.md)).
  Time column format is `"YYYY-MM-DD"`.

- **USDM** (`SpatVector` polygons): the drought monitor class (`DM`,
  integer 0–4) at each location is determined via spatial overlay. A
  `time` column of class `Date` is populated from the `date` attribute
  of `from`.

When `.by_time` is supplied the extracted result is passed through
[`calc_summarize_by()`](https://niehs.github.io/amadeus/dev/reference/calc_summarize_by.md)
using the same semantics as all other `calculate_*()` functions in this
package.

## Usage

``` r
calculate_drought(
  from,
  locs,
  locs_id = "site_id",
  radius = 0L,
  fun = "mean",
  weights = NULL,
  geom = FALSE,
  .by_time = NULL,
  ...
)
```

## Arguments

- from:

  SpatRaster or SpatVector. Output of
  [`process_drought()`](https://niehs.github.io/amadeus/dev/reference/process_drought.md).

  - `SpatRaster` for SPEI or EDDI sources.

  - `SpatVector` (polygons) for USDM source.

- locs:

  data.frame, character (path to CSV), `SpatVector`, or `sf` object.
  Point locations at which to extract values.

- locs_id:

  character(1). Name of the unique location identifier column in `locs`.
  Default `"site_id"`.

- radius:

  integer(1). Circular buffer radius in metres around each site location
  used for raster extraction (SPEI/EDDI only; ignored for USDM). Default
  `0L`.

- fun:

  character(1). Summary function applied to raster cells within the
  buffer (SPEI/EDDI only). Default `"mean"`.

- weights:

  `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file path.
  Optional weights raster for weighted extraction. If `NULL` (default),
  unweighted extraction is performed.

- geom:

  `FALSE`, `"sf"`, or `"terra"`. Whether to attach geometry to the
  returned object. Default `FALSE`.

- .by_time:

  NULL or character(1). Name of the time column to use temporal
  summarization unit token. `NULL` disables `"time"`.

- ...:

  Reserved for future use; currently ignored.

## Value

A `data.frame` (default) or `SpatVector`/`sf` object (when `geom` is
set) with columns:

- `<locs_id>`:

  Location identifier.

- `time`:

  Date of the observation (`Date` or `"YYYY-MM-DD"` character).

- `<value_column>`:

  Extracted drought index or class value.

When `.by_time` is non-`NULL`, rows are aggregated to the specified
resolution via
[`calc_summarize_by()`](https://niehs.github.io/amadeus/dev/reference/calc_summarize_by.md).

## Note

- The column name for extracted drought values follows the pattern
  `"<source>_<timescale>_<radius>"` (e.g. `"spei_01_0"`) for SPEI/EDDI,
  and `"usdm_dm_0"` for USDM.

- For USDM, `radius` is accepted but currently unused; future versions
  may support majority-class extraction within a buffer.

## See also

[`process_drought`](https://niehs.github.io/amadeus/dev/reference/process_drought.md),
[`download_drought`](https://niehs.github.io/amadeus/dev/reference/download_drought.md),
[`calc_summarize_by`](https://niehs.github.io/amadeus/dev/reference/calc_summarize_by.md)

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
locs <- data.frame(site_id = "001", lon = -97.5, lat = 35.5)
## SPEI example
spei <- process_drought(
  source = "spei",
  path = "./data/drought",
  date = c("2020-01-01", "2020-12-31"),
  timescale = 1L
)
calculate_drought(
  from = spei,
  locs = locs,
  locs_id = "site_id",
  radius = 0L,
  fun = "mean"
)
## USDM example
usdm <- process_drought(
  source = "usdm",
  path = "./data/drought",
  date = c("2020-01-07", "2020-03-31")
)
calculate_drought(
  from = usdm,
  locs = locs,
  locs_id = "site_id"
)
} # }
```
