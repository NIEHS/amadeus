# Calculate atmospheric composition covariates

Extract atmospheric composition values at point locations. Returns a
`data.frame` object containing `locs_id`, date and hour, vertical
pressure level, and atmospheric composition variable. Atmospheric
composition variable column name reflects variable and circular buffer
radius.

## Usage

``` r
calculate_geos(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  geom = FALSE,
  ...
)
```

## Arguments

- from:

  SpatRaster(1). Output of
  [`process_geos()`](https://niehs.github.io/amadeus/reference/process_geos.md).

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

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- ...:

  Placeholders.

## Value

a data.frame or SpatVector object

## See also

[`process_geos()`](https://niehs.github.io/amadeus/reference/process_geos.md)

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_geos(
  from = geos, # derived from process_geos() example
  locs = loc,
  locs_id = "id",
  radius = 0,
  fun = "mean",
  geom = FALSE
)
} # }
```
