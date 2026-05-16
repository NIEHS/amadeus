# Calculate gridMET covariates

Extract gridMET values at point locations. Returns a `data.frame` object
containing `locs_id` and gridMET variable. gridMET variable column name
reflects the gridMET variable and circular buffer radius.

## Usage

``` r
calculate_gridmet(
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

  SpatRaster(1). Output from
  [`process_gridmet()`](https://niehs.github.io/amadeus/dev/reference/process_gridmet.md).

- locs:

  data.frame. character to file path, SpatVector, or sf object.

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

  Placeholders.

## Value

a data.frame or SpatVector object

## See also

[`process_gridmet()`](https://niehs.github.io/amadeus/dev/reference/process_gridmet.md)

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_gridmet(
  from = gridmet, # derived from process_gridmet() example
  locs = loc,
  locs_id = "id",
  radius = 0,
  fun = "mean",
  geom = FALSE
)
} # }
```
