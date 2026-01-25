# Calculate climate classification covariates

Extract climate classification values at point locations. Returns a
`data.frame` object containing `locs_id` and binary (0 = point not in
climate region; 1 = point in climate region) variables for each climate
classification region.

## Usage

``` r
calculate_koppen_geiger(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  geom = FALSE,
  ...
)
```

## Arguments

- from:

  SpatVector(1). Output of
  [`process_koppen_geiger()`](https://niehs.github.io/amadeus/reference/process_koppen_geiger.md).

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

a data.frame or SpatVector object

## Note

The returned object contains a `$description` column to represent the
temporal range covered by the dataset. For more information, see
<https://www.nature.com/articles/sdata2018214>.

## See also

[`process_koppen_geiger`](https://niehs.github.io/amadeus/reference/process_koppen_geiger.md)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_koppen_geiger(
  from = kg, # derived from process_koppen_geiger() example
  locs = loc,
  locs_id = "id",
  geom = FALSE
)
} # }
```
