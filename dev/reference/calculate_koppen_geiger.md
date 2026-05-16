# Calculate climate classification covariates

Extract Koppen-Geiger climate classes at point or buffered locations.
Returns a `data.frame` with `locs_id`, a `description` column, and
either binary indicators (`frac = FALSE`) or fractional overlap values
(`frac = TRUE`) for climate groups A-E.

## Usage

``` r
calculate_koppen_geiger(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  weights = NULL,
  geom = FALSE,
  frac = FALSE,
  radius = 0,
  ...
)
```

## Arguments

- from:

  SpatRaster(1). Output of
  [`process_koppen_geiger()`](https://niehs.github.io/amadeus/dev/reference/process_koppen_geiger.md).

- locs:

  sf/SpatVector. Unique locs. Should include a unique identifier field
  named `locs_id`

- locs_id:

  character(1). Name of unique identifier.

- weights:

  `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file path.
  Optional weights raster for weighted extraction. If `NULL` (default),
  unweighted extraction is performed.

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- frac:

  logical(1). Default `FALSE`. If `FALSE`, return binary 0/1 indicators
  by climate group. If `TRUE`, return fractional overlap in the
  extraction footprint.

- radius:

  numeric(1). Circular buffer size (meters) around point locations. Use
  `0` (default) for exact point extraction.

- ...:

  Placeholders.

## Value

a data.frame or SpatVector object with climate columns named like
`DUM_CLRGA_00000` (`frac = FALSE`) or `FRC_CLRGA_100000` (`frac = TRUE`)
where the suffix reflects the extraction radius.

## Note

The returned object contains a `$description` column to represent the
temporal range covered by the dataset. For more information, see
<https://www.nature.com/articles/sdata2018214>.

## See also

[`process_koppen_geiger`](https://niehs.github.io/amadeus/dev/reference/process_koppen_geiger.md)

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
