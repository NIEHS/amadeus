# EDGAR Emissions

This article demonstrates a compact workflow for EDGAR emissions
surfaces supported by `amadeus`.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

Each workflow uses two small example surfaces: `example_points_sf`, a
saved subset of AQS monitor locations from
`tests/testdata/aqs/aqs-location-sample.rds`, for point extraction; and
the packaged Durham County Uber H3 resolution-8 hexagons at
`system.file("extdata", "data_files", "durham_h3_res8.rds", package = "amadeus")`
for polygon extraction.

## Available EDGAR variables and information

The `species` argument in `download_data(dataset_name = "edgar", ...)`
currently supports the following EDGAR air-pollutant emissions
variables. For the emissions downloads used in this vignette, values are
reported in tonnes.

| amadeus variable name | short description                                  |
|:----------------------|:---------------------------------------------------|
| BC                    | Black carbon (carbonaceous particulate fraction)   |
| CO                    | Carbon monoxide                                    |
| NH3                   | Ammonia                                            |
| NMVOC                 | Non-methane volatile organic compounds             |
| NOx                   | Nitrogen oxides                                    |
| OC                    | Organic carbon (carbonaceous particulate fraction) |
| PM10                  | Particulate matter \<= 10 um aerodynamic diameter  |
| PM2.5                 | Particulate matter \<= 2.5 um aerodynamic diameter |
| SO2                   | Sulfur dioxide                                     |

[`download_edgar()`](https://niehs.github.io/amadeus/dev/reference/download_edgar.md)
also exposes several other useful switches that determine which EDGAR
products are available:

- `version = "8.1"` covers the standard EDGAR air-pollutant products
  shown in this vignette.
- `version = "8.1_voc"` switches to VOC speciation products instead of
  the standard pollutant grids.
- `temp_res` supports `"yearly"`, `"monthly"`, and `"timeseries"` for
  `version = "8.1"`. VOC speciation downloads do not use `temp_res`.
- `output` supports `"emi"` for emissions and `"flx"` for flux products.
- `format` is typically `"nc"` or `"txt"`, but monthly outputs and flux
  outputs are only available in NetCDF form.
- `year_range` can be a single year or a range. Yearly EDGAR products
  span `1970-2022`, while monthly and VOC-speciation products span
  `2000-2022`.

The sector arguments depend on which EDGAR product family you request:

| input | supported values | when used |
|:---|:---|:---|
| sector_yearly | AGS, AWB, CHE, ENE, IND, MNM, NMM, PRU_SOL, RCO, REF_TRF, SWD_INC, SWD_LDF, TNR_Aviation_CDS, TNR_Aviation_CRS, TNR_Aviation_LTO, TNR_Aviation_SPS, TNR_Other, TNR_Ship, TRO, WWT | Yearly standard EDGAR products (`version = "8.1"`) |
| sector_monthly | AGRICULTURE, BUILDINGS, FUEL_EXPLOITATION, IND_COMBUSTION, IND_PROCESSES, POWER_INDUSTRY, TRANSPORT, WASTE | Monthly standard EDGAR products (`version = "8.1"`) |
| sector_voc | AGRICULTURE, BUILDINGS, FUEL_EXPLOITATION, IND_COMBUSTION, IND_PROCESSES, POWER_INDUSTRY, TRANSPORT, WASTE | VOC speciation products (`version = "8.1_voc"`) |
| voc | Integers 1-25 for VOC speciation groups | VOC speciation products (`version = "8.1_voc"`) |

EDGAR groups these variables broadly as ozone precursors (`CO`, `NOx`,
`NMVOC`), acidifying gases (`NH3`, `NOx`, `SO2`), and primary
particulates (`PM10`, `PM2.5`, `BC`, `OC`).

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "edgar_workflow")
download_data(
  dataset_name = "edgar",
  species = c("CO", "NOx"),
  temp_res = "yearly",
  sector_yearly = "ENE",
  year_range = 2021,
  directory_to_save = directory_to_save,
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = FALSE
)
```

## Process one workflow-ready data product

Amadeus process functions returns a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
so we can apply the terra extent option on the bounding box of the
points to reduce processing time.

``` r

processed_data <- process_covariates(
  covariate = "edgar",
  path = list.files(
    directory_to_save,
    pattern = "\\.(tif|tiff|nc4?|grd|img)$",
    recursive = TRUE,
    full.names = TRUE
  ),
  extent = sf::st_bbox(example_points_sf)
)
```

## Calculate covariates at points

We start with a radius of 0, which extracts the raster gric cell value
at each point with no smoothing or averaging over an area.

``` r

point_values <- calculate_covariates(
  covariate = "edgar",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  geom = "sf"
)

print(point_values)
```

At point-level extractions, we often see more NA because data has to
overlap exactly. Here, we smooth out the covariate calculation by using
a buffer radius.

``` r

point_values_1km <- calculate_covariates(
  covariate = "edgar",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 1000, # distance units are in meters, see crs(processed_data)
  geom = "sf"
)

print(point_values_1km)

point_values_10km <- calculate_covariates(
  covariate = "edgar",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 10000,
  geom = "sf"
)

print(point_values_10km)
```

## Calculate covariates across Durham County H3 hexagons

``` r

polygon_values <- calculate_covariates(
  covariate = "edgar",
  from = processed_data,
  locs = durham_hex,
  locs_id = "h3_id",
  radius = 0,
  geom = "sf"
)
```

## Visualize the point outputs

``` r

plot_points(point_values, paste0("EDGAR Emissions", ": point extraction"))
```
