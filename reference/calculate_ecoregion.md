# Calculate ecoregions covariates

Extract ecoregions covariates (U.S. EPA Ecoregions Level 2/3) at point
locations. Returns a `data.frame` object containing `locs_id` and binary
(0 = point not in ecoregion; 1 = point in ecoregion) variables for each
ecoregion.

## Usage

``` r
calculate_ecoregion(from = NULL, locs, locs_id = "site_id", geom = FALSE, ...)
```

## Arguments

- from:

  SpatVector(1). Output of
  [`process_ecoregion`](https://niehs.github.io/amadeus/reference/process_ecoregion.md).

- locs:

  sf/SpatVector. Unique locs. Should include a unique identifier field
  named `locs_id`

- locs_id:

  character(1). Name of unique identifier.

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- ...:

  Placeholders.

## Value

a data.frame or SpatVector object object with dummy variables and
attributes of:

- `attr(., "ecoregion2_code")`: Ecoregion lv.2 code and key

- `attr(., "ecoregion3_code")`: Ecoregion lv.3 code and key

## See also

[`process_ecoregion`](https://niehs.github.io/amadeus/reference/process_ecoregion.md)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_ecoregion(
  from = ecoregion, # derived from process_ecoregion() example
  locs = loc,
  locs_id = "id",
  geom = FALSE
)
} # }
```
