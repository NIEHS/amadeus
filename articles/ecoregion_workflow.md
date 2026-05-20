# US EPA Ecoregions

This article demonstrates a compact workflow for the US EPA ecoregion
layers distributed through `amadeus`. The download is a single national
layer that contains the multiple ecoregion levels used during point and
polygon extraction.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

- `download_data(dataset_name = "ecoregion", ...)` does not take
  product, year, scenario, or region arguments; it downloads the EPA
  U.S. ecoregion distribution used by the package.
- This is a static national vector layer with no temporal dimension.
- Downloads arrive as a zipped vector-data bundle that can be processed
  into workflow-ready ecoregion boundaries and identifiers.
- Ecoregions are a categorical covariate, so we have two options for
  calculation output:
  - Binary (dummy) indicator columns for the intersecting level-2 and
    level-3 ecoregions at each location, with key-based names (for
    example `DUM_E2083_00000`, `DUM_E3064_00000`).
  - Fraction or proportion of each location that falls in each
    intersecting level-2 and level-3 ecoregion, with key-based names
    (for example `FRC_E2083_00000`, `FRC_E3064_00000`).

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "ecoregion_workflow")
download_data(
  dataset_name = "ecoregion",
  directory_to_save = directory_to_save,
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = FALSE
)
```

## Process one workflow-ready data product

``` r

ecoregion_path <- list.files(
  directory_to_save,
  pattern = "\\.(gpkg|shp)$",
  recursive = TRUE,
  full.names = TRUE
)[1]
processed_data <- process_covariates(
  covariate = "ecoregion",
  path = ecoregion_path
)
plot(processed_data, main = "Level 3 Ecoregions of the Conterminous United States")
```

## Calculate dummy (bivariate) covariates at points

[`calculate_ecoregion()`](https://niehs.github.io/amadeus/reference/calculate_ecoregion.md)
by default returns binary (dummy) indicator columns for the intersecting
level-2 and level-3 ecoregions at each location. By default
(`colnames = "coded"`), those indicators use key-based names (for
example `DUM_E2083_00000`, `DUM_E3064_00000`). If you set
`colnames = "full_ecoregion"`, indicators use sanitized full names (for
example `DUM_E2_SOUTHEASTERN_USA_PLAINS_00000`), with duplicate names
automatically disambiguated by suffixes like `_1`.

``` r


sf_process <- sf::st_as_sf(processed_data)

example_points_terra <- terra::spatSample(
  processed_data,
  size = 25,
  method = "random"
)
 example_points_sf <- sf::st_as_sf(example_points_terra)
 example_points_sf$site_id <- paste0("site_", seq_len(nrow(example_points_sf)))



point_values <- calculate_ecoregion(
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 10000,
  geom = "sf"
)

print(point_values)
```

## Calculate fractional covariates at points

[`calculate_ecoregion()`](https://niehs.github.io/amadeus/reference/calculate_ecoregion.md)
can also return fractional (proportion) columns for the intersecting
level-2 and level-3 ecoregions at each location. By default
(`colnames = "coded"`), those indicators use key-based names (for
example `FRC_E2083_00000`, `FRC_E3064_00000`). If you set
`colnames = "full_ecoregion"`, indicators use sanitized full names (for
example `FRC_E2_SOUTHEASTERN_USA_PLAINS_00000`), with duplicate names
automatically disambiguated by suffixes like `_1`.

``` r

point_values <- calculate_ecoregion(
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  geom = "sf",
  radius = 1000,
  frac = TRUE,
  drop = TRUE
)

print(point_values)
```
