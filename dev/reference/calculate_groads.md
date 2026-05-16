# Calculate roads covariates

Prepared groads data is clipped with the buffer polygons of `radius`.
The total length of the roads are calculated. Then the density of the
roads is calculated by dividing the total length from the area of the
buffer.
[`terra::linearUnits()`](https://rspatial.github.io/terra/reference/linearUnits.html)
is used to convert the unit of length to meters.

## Usage

``` r
calculate_groads(
  from = NULL,
  locs = NULL,
  locs_id = NULL,
  radius = 1000,
  fun = "sum",
  drop = FALSE,
  weights = NULL,
  geom = FALSE,
  ...
)
```

## Arguments

- from:

  SpatVector(1). Output of `process_groads`.

- locs:

  data.frame, characater to file path, SpatVector, or sf object.

- locs_id:

  character(1). Column within `locations` CSV file containing identifier
  for each unique coordinate location.

- radius:

  integer(1). Circular buffer distance around site locations. (Default =
  1000).

- fun:

  function(1). Function used to summarize the length of roads within
  sites location buffer (Default is `sum`).

- drop:

  logical(1). Should locations with zero roads in the extraction buffer
  be dropped from results? Default is `FALSE` (retain all locations).

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

## Note

Unit is km / sq km. The returned `data.frame` object contains a `$time`
column to represent the temporal range covered by the dataset. For more
information, see
<https://data.nasa.gov/dataset/global-roads-open-access-data-set-version-1-groadsv1>.

## See also

[`process_groads`](https://niehs.github.io/amadeus/dev/reference/process_groads.md)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_groads(
  from = groads, # derived from process_groads() example
  locs = loc,
  locs_id = "id",
  radius = 1000,
  fun = "sum",
  geom = FALSE
)
} # }
```
