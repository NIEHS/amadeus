# Calculate meteorological and atmospheric covariates

Extract meteorological and atmospheric values at point locations.
Returns a `data.frame` object containing `locs_id`, date and hour,
vertical pressure level, and meteorological or atmospheric variable.
Variable column name reflects variable and circular buffer radius.

## Usage

``` r
calculate_merra2(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
)
```

## Arguments

- from:

  SpatRaster(1). Output of
  [`process_merra2()`](https://niehs.github.io/amadeus/dev/reference/process_merra2.md).

- locs:

  data.frame, characater to file path, SpatVector, or sf object.

- locs_id:

  character(1). Column within `locations` CSV file containing identifier
  for each unique coordinate location.

- radius:

  integer(1). Circular buffer distance around site locations. (Default =
  0).

- fun:

  character(1). Function used to summarize multiple raster cells within
  sites location buffer (Default = `mean`).

- weights:

  `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file path.
  Optional weights raster for weighted extraction. If `NULL` (default),
  unweighted extraction is performed.

- .by_time:

  NULL or character(1). Optional time grouping key used with `.by_time`
  for temporal summaries.

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- ...:

  Placeholders

## Value

a data.frame or SpatVector object. When `.by_time` is provided, rows are
aggregated using
[`calc_summarize_by()`](https://niehs.github.io/amadeus/dev/reference/calc_summarize_by.md).

## See also

[`calculate_geos()`](https://niehs.github.io/amadeus/dev/reference/calculate_geos.md),
[`process_merra2()`](https://niehs.github.io/amadeus/dev/reference/process_merra2.md)

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_merra2(
  from = merra2, # derived from process_merra2() example
  locs = loc,
  locs_id = "id",
  radius = 0,
  fun = "mean",
  geom = FALSE
)
} # }
```
