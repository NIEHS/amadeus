# Calculate wildfire smoke covariates

Extract wildfire smoke plume values at point or buffered locations.
Returns a `data.frame` object containing `locs_id`, date, and either
binary indicators (`frac = FALSE`) or fractional overlap values
(`frac = TRUE`) for wildfire smoke plume density inherited from `from`.

## Usage

``` r
calculate_hms(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  weights = NULL,
  .by_time = NULL,
  frac = FALSE,
  geom = FALSE,
  ...
)
```

## Arguments

- from:

  SpatVector(1). Output of
  [`process_hms()`](https://niehs.github.io/amadeus/dev/reference/process_hms.md).

- locs:

  data.frame, characater to file path, SpatVector, or sf object.

- locs_id:

  character(1). Column within `locations` CSV file containing identifier
  for each unique coordinate location.

- radius:

  integer(1). Circular buffer distance around site locations. (Default =
  0).

- weights:

  `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file path.
  Optional weights raster for weighted extraction. If `NULL` (default),
  unweighted extraction is performed.

- .by_time:

  NULL or character(1). Optional time grouping key used when `.by_time`
  is provided. When supplied, HMS indicators are summarized by `sum`
  (smoke-day counts) for `frac = FALSE`, or `mean` for `frac = TRUE`.

- frac:

  logical(1). Default `FALSE`. If `FALSE`, return binary 0/1 smoke
  indicators by density class. If `TRUE`, return fractional overlap by
  density class.

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- ...:

  Placeholders.

## Value

a data.frame or SpatVector object. When `.by_time` is provided, rows are
aggregated using
[`calc_summarize_by()`](https://niehs.github.io/amadeus/dev/reference/calc_summarize_by.md).

## See also

[`process_hms()`](https://niehs.github.io/amadeus/dev/reference/process_hms.md)

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_hms(
  from = hms, # derived from process_hms() example
  locs = loc,
  locs_id = "id",
  radius = 0,
  geom = FALSE
)
} # }
```
