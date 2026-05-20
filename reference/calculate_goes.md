# Calculate NOAA GOES ADP covariates

Extract NOAA GOES Aerosol Detection Product (ADP) values at point
locations from a `SpatRaster` returned by
[`process_goes()`](https://niehs.github.io/amadeus/reference/process_goes.md).
Returns a `data.frame` (or `SpatVector` / `sf`) containing `locs_id`,
`time`, and the extracted variable column (`{variable}_{radius}`).

## Usage

``` r
calculate_goes(
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
  [`process_goes()`](https://niehs.github.io/amadeus/reference/process_goes.md).

- locs:

  data.frame, character file path, `SpatVector`, or `sf` object with
  point locations.

- locs_id:

  character(1). Column name for unique location identifier.

- radius:

  integer(1). Circular buffer radius in metres around each site (default
  0 = point extraction).

- fun:

  character(1). Summary function for buffered extractions (default
  `"mean"`).

- weights:

  `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file path.
  Optional weights raster for weighted extraction. If `NULL` (default),
  unweighted extraction is performed.

- .by_time:

  NULL or character(1). Optional time grouping key used with `.by_time`
  for temporal summaries.

- geom:

  `FALSE`/`"sf"`/`"terra"`. Return geometry with results. Default
  `FALSE`. The CRS is inherited from `from`.

- ...:

  Placeholders.

## Value

a `data.frame` or `SpatVector` object.

## See also

[`process_goes`](https://niehs.github.io/amadeus/reference/process_goes.md)

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires downloaded
##       and processed data.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -95.0, lat = 34.5)
calculate_goes(
  from = goes,  # derived from process_goes() example
  locs = loc,
  locs_id = "id",
  radius = 0,
  fun = "mean"
)
} # }
```
