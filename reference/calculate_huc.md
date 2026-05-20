# Calculate HUC covariates

Extract HUC IDs at point locations. Returns a `data.frame` object
containing `locs_id` and HUC IDs.

## Usage

``` r
calculate_huc(
  from,
  locs,
  locs_id = "site_id",
  weights = NULL,
  geom = FALSE,
  ...
)
```

## Arguments

- from:

  SpatVector(1). Output from
  [`process_huc()`](https://niehs.github.io/amadeus/reference/process_huc.md).

- locs:

  data.frame. character to file path, SpatVector, or sf object.

- locs_id:

  character(1). Column within `locations` CSV file containing identifier
  for each unique coordinate location.

- weights:

  `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file path.
  Optional weights raster for weighted extraction. If `NULL` (default),
  unweighted extraction is performed.

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- ...:

  Placeholders.

## Value

a data.frame or SpatVector object

## See also

[`process_huc()`](https://niehs.github.io/amadeus/reference/process_huc.md)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_huc(
  from = huc, # derived from process_huc() example
  locs = loc,
  locs_id = "id",
  geom = FALSE
)
} # }
```
