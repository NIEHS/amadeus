# Process raw data wrapper function

This function processes raw data files which have been downloaded by
[`download_data`](https://niehs.github.io/amadeus/reference/download_data.md).
`process_covariates` and the underlying source-specific processing
functions have been designed to operate on the raw data files. To avoid
errors, **do not edit the raw data files before passing to
`process_covariates`**.

## Usage

``` r
process_covariates(
  covariate = c("modis_swath", "modis_merge", "koppen-geiger", "blackmarble",
    "koeppen-geiger", "koppen", "koeppen", "geos", "dummies", "gmted", "hms", "smoke",
    "sedac_population", "population", "sedac_groads", "groads", "roads", "nlcd", "tri",
    "narr", "nei", "ecoregions", "ecoregion", "merra", "merra2", "gridmet",
    "terraclimate", "huc", "cropscape", "cdl", "prism"),
  path = NULL,
  ...
)
```

## Arguments

- covariate:

  character(1). Covariate type.

- path:

  character(1). Directory or file path to raw data depending on
  `covariate` value.

- ...:

  Arguments passed to each raw data processing function.

## Value

`SpatVector`, `SpatRaster`, `sf`, or `character` depending on covariate
type and selections.

## See also

- [`process_modis_swath`](https://niehs.github.io/amadeus/reference/process_modis_swath.md):
  "modis_swath"

- [`process_modis_merge`](https://niehs.github.io/amadeus/reference/process_modis_merge.md):
  "modis_merge"

- [`process_blackmarble`](https://niehs.github.io/amadeus/reference/process_blackmarble.md):
  "blackmarble"

- [`process_koppen_geiger`](https://niehs.github.io/amadeus/reference/process_koppen_geiger.md):
  "koppen-geiger", "koeppen-geiger", "koppen"

- [`process_ecoregion`](https://niehs.github.io/amadeus/reference/process_ecoregion.md):
  "ecoregion", "ecoregions"

- [`process_nlcd`](https://niehs.github.io/amadeus/reference/process_nlcd.md):
  "nlcd", "NLCD"

- [`process_tri`](https://niehs.github.io/amadeus/reference/process_tri.md):
  "tri", "TRI"

- [`process_nei`](https://niehs.github.io/amadeus/reference/process_nei.md):
  "nei", "NEI"

- [`process_geos`](https://niehs.github.io/amadeus/reference/process_geos.md):
  "geos", "GEOS"

- [`process_gmted`](https://niehs.github.io/amadeus/reference/process_gmted.md):
  "gmted", "GMTED"

- [`process_aqs`](https://niehs.github.io/amadeus/reference/process_aqs.md):
  "aqs", "AQS"

- [`process_hms`](https://niehs.github.io/amadeus/reference/process_hms.md):
  "hms", "smoke", "HMS"

- [`process_narr`](https://niehs.github.io/amadeus/reference/process_narr.md):
  "narr", "NARR"

- [`process_groads`](https://niehs.github.io/amadeus/reference/process_groads.md):
  "sedac_groads", "roads", "groads"

- [`process_population`](https://niehs.github.io/amadeus/reference/process_population.md):
  "sedac_population", "population"

- [`process_merra2`](https://niehs.github.io/amadeus/reference/process_merra2.md):
  "merra", "merra2", "MERRA2"

- [`process_gridmet`](https://niehs.github.io/amadeus/reference/process_gridmet.md):
  "gridmet", "gridMET"

- [`process_terraclimate`](https://niehs.github.io/amadeus/reference/process_terraclimate.md):
  "terraclimate", "TerraClimate"

- [`process_huc`](https://niehs.github.io/amadeus/reference/process_huc.md):
  "huc", "HUC"

- [`process_cropscape`](https://niehs.github.io/amadeus/reference/process_cropscape.md):
  "cropscape", "cdl"

- [`process_prism`](https://niehs.github.io/amadeus/reference/process_prism.md):
  "prism", "PRISM"

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
process_covariates(
  covariate = "narr",
  date = c("2018-01-01", "2018-01-10"),
  variable = "weasd",
  path = system.file("extdata", "examples", "narr", "weasd")
)
} # }
```
