# Calculate wildfire smoke covariates

Extract wildfire smoke plume values at point locations. Returns a
`data.frame` object containing `locs_id`, date, and binary variable for
wildfire smoke plume density inherited from `from` (0 = point not
covered by wildfire smoke plume; 1 = point covered by wildfire smoke
plume).

## Usage

``` r
calculate_hms(from, locs, locs_id = NULL, radius = 0, geom = FALSE, ...)
```

## Arguments

- from:

  SpatVector(1). Output of
  [`process_hms()`](https://niehs.github.io/amadeus/reference/process_hms.md).

- locs:

  data.frame, characater to file path, SpatVector, or sf object.

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

[`process_hms()`](https://niehs.github.io/amadeus/reference/process_hms.md)

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
