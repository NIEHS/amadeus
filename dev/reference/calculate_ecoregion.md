# Calculate ecoregions covariates

Extract ecoregions covariates (U.S. EPA Ecoregions Level 2/3) at point
or polygon locations. Returns a `data.frame` object containing `locs_id`
and either dummy indicators (`frac = FALSE`) or area fractions
(`frac = TRUE`) for each ecoregion.

## Usage

``` r
calculate_ecoregion(
  from = NULL,
  locs,
  locs_id = "site_id",
  colnames = c("coded", "full_ecoregion"),
  frac = FALSE,
  drop = FALSE,
  weights = NULL,
  geom = FALSE,
  radius = 0,
  ...
)
```

## Arguments

- from:

  SpatVector(1). Output of
  [`process_ecoregion`](https://niehs.github.io/amadeus/dev/reference/process_ecoregion.md).

- locs:

  sf/SpatVector. Unique locs. Should include a unique identifier field
  named `locs_id`

- locs_id:

  character(1). Name of unique identifier.

- colnames:

  character(1). Naming convention for ecoregion indicator columns.
  Default is `"coded"` for the existing numeric key-based names. Use
  `"full_ecoregion"` to emit sanitized full ecoregion names.

- frac:

  logical(1). Default `FALSE`. If `FALSE`, returns binary dummy
  indicators (0/1). If `TRUE`, returns fractional overlap values.

- drop:

  logical(1). Default `FALSE`. If `TRUE`, remove ecoregion columns that
  are all 0 or `NA` across returned locations.

- weights:

  `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file path.
  Optional weights raster for weighted extraction. If `NULL` (default),
  unweighted extraction is performed.

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- radius:

  numeric(1). Circular buffer size (meters) around point locations. Use
  `0` (default) for exact point extraction.

- ...:

  Placeholders.

## Value

a data.frame or SpatVector object with ecoregion indicator/fraction
variables and attributes of:

- Indicator names are controlled by `colnames`: `"coded"` (default)
  creates key-based names such as `DUM_E2083_00000` and
  `DUM_E3064_00000` when `frac = FALSE`, or `FRC_E2083_00000` and
  `FRC_E3064_00000` when `frac = TRUE`; `"full_ecoregion"` creates
  sanitized name-based columns such as
  `DUM_E2_SOUTHEASTERN_USA_PLAINS_00000` /
  `FRC_E2_SOUTHEASTERN_USA_PLAINS_00000` and
  `DUM_E3_NORTHERN_PIEDMONT_00000` / `FRC_E3_NORTHERN_PIEDMONT_00000`
  (duplicates are suffixed, e.g. `_1`).

- `attr(., "ecoregion2_code")`: Ecoregion lv.2 code and key

- `attr(., "ecoregion3_code")`: Ecoregion lv.3 code and key

## See also

[`process_ecoregion`](https://niehs.github.io/amadeus/dev/reference/process_ecoregion.md)

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
  colnames = "coded",
  geom = FALSE
)
} # }
```
