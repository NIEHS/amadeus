# Calculate toxic release covariates

Calculate toxic release values for polygons or isotropic buffer point
locations. Returns a `data.frame` object containing `locs_id` and
variables for each chemical in `from`.

## Usage

``` r
calculate_tri(
  from = NULL,
  locs,
  locs_id = "site_id",
  radius = c(1000L, 10000L, 50000L),
  geom = FALSE,
  ...
)
```

## Arguments

- from:

  SpatVector(1). Output of
  [`process_tri()`](https://niehs.github.io/amadeus/reference/process_tri.md).

- locs:

  sf/SpatVector. Locations where TRI variables are calculated.

- locs_id:

  character(1). Unique site identifier column name. Default is
  `"site_id"`.

- radius:

  Circular buffer radius. Default is `c(1000, 10000, 50000)` (meters)

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- ...:

  Placeholders.

## Value

a data.frame or SpatVector object

## Note

U.S. context.

## See also

[`sum_edc`](https://niehs.github.io/amadeus/reference/sum_edc.md),
[`process_tri`](https://niehs.github.io/amadeus/reference/process_tri.md)

## Author

Insang Song, Mariana Kassien

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_tri(
  from = tri, # derived from process_tri() example
  locs = loc,
  locs_id = "id",
  radius = c(1e3L, 1e4L, 5e4L)
)
} # }
```
