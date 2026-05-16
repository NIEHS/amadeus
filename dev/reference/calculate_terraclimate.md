# Calculate TerraClimate covariates

Extract TerraClimate values at point locations. Returns a `data.frame`
object containing `locs_id` and TerraClimate variable. TerraClimate
variable column name reflects the TerraClimate variable and circular
buffer radius. The `$time` column will contain the year and month
("YYYYMM") as TerraClimate products have monthly temporal resolution.

## Usage

``` r
calculate_terraclimate(
  from = NULL,
  locs = NULL,
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
  [`process_terraclimate()`](https://niehs.github.io/amadeus/dev/reference/process_terraclimate.md).

- locs:

  data.frame. character to file path, SpatVector, or sf object.

- locs_id:

  character(1). Column within `locations` CSV file containing identifier
  for each unique coordinate location.

- radius:

  integer(1). Circular buffer distance around site locations. (Default =
  0).

- fun:

  character(1). Function used to summarize multiple raster cells within
  sites location buffer (Default = `mean`).

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

## Note

TerraClimate data has monthly temporal resolution, so the `$time` column
will contain the year and month in YYYYMM format (ie. January, 2018 =
201801).

## See also

[`process_terraclimate()`](https://niehs.github.io/amadeus/dev/reference/process_terraclimate.md)

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_terraclimate(
  from = terraclimate, # derived from process_terraclimate() example
  locs = loc,
  locs_id = "id",
  radius = 0,
  fun = "mean",
  geom = FALSE
)
} # }
```
