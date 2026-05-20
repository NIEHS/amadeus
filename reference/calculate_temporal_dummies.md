# Calculate temporal dummy covariates

Calculate temporal dummy covariates at point locations. Returns a
`data.frame` object with `locs_id`, year binary variable for each value
in `year`, and month and day of week binary variables.

## Usage

``` r
calculate_temporal_dummies(
  locs,
  locs_id = "site_id",
  year = seq(2018L, 2022L),
  weights = NULL,
  geom = FALSE,
  ...
)
```

## Arguments

- locs:

  data.frame with a temporal field named `"time"`

- locs_id:

  character(1). Unique site identifier column name. Default is
  `"site_id"`.

- year:

  integer. Year domain to dummify. Default is `seq(2018L, 2022L)`.

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

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_temporal_dummies(
  locs = loc,
  locs_id = "id",
  year = seq(2018L, 2022L)
)
} # }
```
