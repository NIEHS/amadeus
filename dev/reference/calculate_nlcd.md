# Calculate land cover covariates

Compute ratio of land cover class in circle buffers around points.
Returns a `data.frame` object containing `locs_id`, longitude, latitude,
time (year), and computed ratio for each land cover class.

## Usage

``` r
calculate_nlcd(
  from,
  locs,
  locs_id = "site_id",
  mode = c("exact", "terra"),
  radius = 1000,
  drop = FALSE,
  weights = NULL,
  max_cells = 5e+07,
  geom = FALSE,
  ...
)
```

## Arguments

- from:

  SpatRaster(1). Output of
  [`process_nlcd()`](https://niehs.github.io/amadeus/dev/reference/process_nlcd.md).

- locs:

  terra::SpatVector of points geometry

- locs_id:

  character(1). Unique identifier of locations

- mode:

  character(1). One of `"exact"` (using
  [`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html))
  or `"terra"` (using
  [`terra::freq()`](https://rspatial.github.io/terra/reference/freq.html)).
  Ignored if `locs` are points.

- radius:

  numeric (non-negative) giving the radius of buffer around points.

- drop:

  logical(1). Default `FALSE`. For buffered outputs (`radius > 0`),
  retain NLCD class columns even when all values are 0 (`drop = FALSE`)
  or remove class columns that are all 0 across all locations
  (`drop = TRUE`).

- weights:

  `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file path.
  Optional weights raster for weighted extraction. If `NULL` (default),
  unweighted extraction is performed.

- max_cells:

  integer(1). Maximum number of cells to be read at once. Higher values
  may expedite processing, but will increase memory usage. Maximum
  possible value is `2^31 - 1`. Only valid when `mode = "exact"`. See
  [`exactextractr::exact_extract`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
  for details.

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- ...:

  Placeholders.

## Value

a data.frame or SpatVector object

## Note

NLCD is available in U.S. only. Users should be aware of the spatial
extent of the data. The results are different depending on `mode`
argument. The `"terra"` mode is less memory intensive but less accurate
because it counts the number of cells intersecting with the buffer. The
`"exact"` may be more accurate but uses more memory as it will account
for the partial overlap with the buffer.

## See also

[`process_nlcd`](https://niehs.github.io/amadeus/dev/reference/process_nlcd.md)

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_nlcd(
  from = nlcd, # derived from process_nlcd() example
  locs = loc,
  locs_id = "id",
  mode = "exact",
  geom = FALSE
)
} # }
```
