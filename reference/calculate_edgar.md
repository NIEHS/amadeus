# Calculate EDGAR covariates

Extract EDGAR gridded emissions values at point locations. For
`radius = 0`, cell values are extracted directly. For `radius > 0`,
means are calculated over a circular buffer around each location.

## Usage

``` r
calculate_edgar(
  from,
  locs,
  locs_id = "site_id",
  radius = 0,
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
)
```

## Arguments

- from:

  SpatRaster(1). Output from
  [`process_edgar()`](https://niehs.github.io/amadeus/reference/process_edgar.md).

- locs:

  data.frame, character to file path, SpatVector, or sf object.

- locs_id:

  character(1). Column within `locations` CSV file containing identifier
  for each unique coordinate location.

- radius:

  numeric(1). Circular buffer distance around site locations. Default is
  `0`.

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

[`process_edgar()`](https://niehs.github.io/amadeus/reference/process_edgar.md)

## Author

Mariana Alifa Kassien, Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires data that is
##       not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_edgar(
  from = edgar, # derived from process_edgar() example
  locs = loc,
  locs_id = "id",
  radius = 0,
  geom = FALSE
)
} # }
```
