# Calculate road emissions covariates

Calculate road emissions covariates

## Usage

``` r
calculate_nei(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  weights = NULL,
  geom = FALSE,
  ...
)
```

## Arguments

- from:

  SpatVector(1). Output of
  [`process_nei()`](https://niehs.github.io/amadeus/reference/process_nei.md).

- locs:

  sf/SpatVector. Locations at NEI values are joined.

- locs_id:

  character(1). Unique site identifier column name. Unused but kept for
  compatibility.

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

[`process_nei`](https://niehs.github.io/amadeus/reference/process_nei.md)

## Author

Insang Song, Ranadeep Daw

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_nei(
  from = nei, # derived from process_nei example
  locs = loc,
  locs_id = "id"
)
} # }
```
