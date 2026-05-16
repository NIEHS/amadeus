# Calculate covariates wrapper function

The `calculate_covariates()` function extracts values at point locations
from a SpatRaster or SpatVector object returned from
[`process_covariates()`](https://niehs.github.io/amadeus/dev/reference/process_covariates.md).
`calculate_covariates()` and the underlying source-specific covariate
functions have been designed to operate on the processed objects. To
avoid errors, **do not edit the processed SpatRaster or SpatVector
objects before passing to `calculate_covariates()`**.

## Usage

``` r
calculate_covariates(
  covariate = c("modis", "koppen-geiger", "koeppen-geiger", "koppen", "koeppen", "geos",
    "dummies", "gmted", "sedac_groads", "groads", "roads", "ecoregions", "ecoregion",
    "hms", "smoke", "gmted", "narr", "geos", "sedac_population", "population", "nlcd",
    "merra", "merra2", "gridmet", "terraclimate", "tri", "nei", "mcd14ml", "prism",
    "cropscape", "cdl", "huc", "edgar", "goes", "goes_adp", "GOES", "drought", "spei",
    "eddi", "usdm"),
  from,
  locs,
  locs_id = "site_id",
  .by_time = NULL,
  weights = NULL,
  ...
)
```

## Arguments

- covariate:

  character(1). Covariate type.

- from:

  character, SpatRaster, SpatVector, or data.frame depending on the
  selected `covariate` route.

- locs:

  sf/SpatVector. Unique locations. Should include a unique identifier
  field named `locs_id`

- locs_id:

  character(1). Name of unique identifier. Default is `"site_id"`.

- .by_time:

  NULL or character(1). Name of the time column to use temporal
  summarization unit token. `NULL` (default) disables temporal
  summarization.

- weights:

  `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file path. Passed
  through to the underlying source-specific function for weighted
  extraction. If `NULL` (default), unweighted extraction is performed.

- ...:

  Arguments passed to each covariate calculation function.

## Value

Calculated covariates as a data.frame or SpatVector object

## Note

`covariate` argument value is converted to lowercase.

## See also

- [`calculate_modis`](https://niehs.github.io/amadeus/dev/reference/calculate_modis.md):
  "modis", "MODIS"

- [`calculate_koppen_geiger`](https://niehs.github.io/amadeus/dev/reference/calculate_koppen_geiger.md):
  "koppen-geiger", "koeppen-geiger", "koppen"

- [`calculate_ecoregion`](https://niehs.github.io/amadeus/dev/reference/calculate_ecoregion.md):
  "ecoregion", "ecoregions"

- [`calculate_temporal_dummies`](https://niehs.github.io/amadeus/dev/reference/calculate_temporal_dummies.md):
  "dummies", "Dummies"

- [`calculate_hms`](https://niehs.github.io/amadeus/dev/reference/calculate_hms.md):
  "hms", "smoke", "HMS"

- [`calculate_gmted`](https://niehs.github.io/amadeus/dev/reference/calculate_gmted.md):
  "gmted", "GMTED"

- [`calculate_narr`](https://niehs.github.io/amadeus/dev/reference/calculate_narr.md):
  "narr", "NARR"

- [`calculate_geos`](https://niehs.github.io/amadeus/dev/reference/calculate_geos.md):
  "geos", "geos_cf", "GEOS"

- [`calculate_goes`](https://niehs.github.io/amadeus/dev/reference/calculate_goes.md):
  "goes", "goes_adp", "GOES"

- [`calculate_population`](https://niehs.github.io/amadeus/dev/reference/calculate_population.md):
  "population", "sedac_population"

- [`calculate_groads`](https://niehs.github.io/amadeus/dev/reference/calculate_groads.md):
  "roads", "groads", "sedac_groads"

- [`calculate_nlcd`](https://niehs.github.io/amadeus/dev/reference/calculate_nlcd.md):
  "nlcd", "NLCD"

- [`calculate_tri`](https://niehs.github.io/amadeus/dev/reference/calculate_tri.md):
  "tri", "TRI"

- [`calculate_nei`](https://niehs.github.io/amadeus/dev/reference/calculate_nei.md):
  "nei", "NEI"

- [`calculate_merra2`](https://niehs.github.io/amadeus/dev/reference/calculate_merra2.md):
  "merra", "MERRA", "merra2", "MERRA2"

- [`calculate_gridmet`](https://niehs.github.io/amadeus/dev/reference/calculate_gridmet.md):
  "gridMET", "gridmet"

- [`calculate_terraclimate`](https://niehs.github.io/amadeus/dev/reference/calculate_terraclimate.md):
  "terraclimate", "TerraClimate"

- [`calculate_prism`](https://niehs.github.io/amadeus/dev/reference/calculate_prism.md):
  "prism", "PRISM"

- [`calculate_cropscape`](https://niehs.github.io/amadeus/dev/reference/calculate_cropscape.md):
  "cropscape", "cdl"

- [`calculate_huc`](https://niehs.github.io/amadeus/dev/reference/calculate_huc.md):
  "huc", "HUC"

- [`calculate_edgar`](https://niehs.github.io/amadeus/dev/reference/calculate_edgar.md):
  "edgar"

- [`calculate_drought`](https://niehs.github.io/amadeus/dev/reference/calculate_drought.md):
  "drought", "spei", "eddi", "usdm"

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_covariates(
  covariate = "narr",
  from = narr, # derived from process_covariates() example
  locs = loc,
  locs_id = "id",
  geom = FALSE
)
} # }
```
