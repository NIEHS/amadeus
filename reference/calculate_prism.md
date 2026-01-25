# Calculate PRISM covariates

Extract PRISM values at point locations. Returns a `data.frame` object
containing `locs_id` and PRISM variable. PRISM variable column name
reflects the PRISM variable and circular buffer radius.

## Usage

``` r
calculate_prism(from, locs, locs_id = "site_id", radius = 0, geom = FALSE, ...)
```

## Arguments

- from:

  SpatRaster(1). Output from
  [`process_prism()`](https://niehs.github.io/amadeus/reference/process_prism.md).

- locs:

  data.frame. character to file path, SpatVector, or sf object.

- locs_id:

  character(1). Column within `locations` CSV file containing identifier
  for each unique coordinate location.

- radius:

  integer(1). Circular buffer distance around site locations. (Default =
  0).

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- ...:

  Placeholders.

## Value

a data.frame or SpatVector object

## See also

[`process_prism()`](https://niehs.github.io/amadeus/reference/process_prism.md)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_prism(
  from = prism, # derived from process_prism() example
  locs = loc,
  locs_id = "id",
  radius = 0,
  geom = FALSE
)
} # }
```
