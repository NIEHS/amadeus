# USGS Hydrologic Unit Codes (HUC)

This article demonstrates a compact workflow for Hydrologic Unit Code
boundaries.

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

## Available inputs and data availability

- `download_data(dataset_name = "huc", ...)` accepts
  `region = "Lower48"` or `"Islands"`; the `"Islands"` option covers
  Hawaii, Puerto Rico, and the Virgin Islands.
- The `type` argument is `"Seamless"` or `"OceanCatchment"` for the two
  national NHDPlus v2.1 delivery types exposed by the wrapper.
- HUC data are static reference boundaries rather than time-varying
  products, and downloads arrive as large `.7z` geodatabase archives;
  automatic unzip is not supported by this wrapper.
- For HUC boundaries, the wrapper docs note that `type = "Seamless"` is
  the right choice because it contains the HUC12 layer, which can then
  be aggregated to HUC6, HUC8, HUC10, and similar levels.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "huc_workflow")
download_data(
  dataset_name = "huc",
  region = "Islands",
  type = "Seamless",
  directory_to_save = directory_to_save,
  acknowledgement = TRUE,
  unzip = TRUE
)
```

## Process one workflow-ready data product

``` r


huc_path <- 
  paste0(directory_to_save,"/NHDPlusNationalData/",
  "NHDPlusV21_National_Seamless_Flattened_HI_PR_VI_PI.gdb")

huc_field <- "HUC12"
layers <- sf::st_layers(huc_path)
processed_data <- process_covariates(
  covariate = "huc",
  path = huc_path,
  layer_name = "HUC12",
  extent =  terra::ext(-162.5, -153.5, 18, 23)
)

plot(processed_data, main = "HUC12 boundaries for Hawaii")
```

## Calculate covariates at points

``` r


df <- data.frame(
  site_id = c("site_1", "site_2", "site_3","site_4"),
  lon = c(-157.8583, -155.5319, -155.5828,-159.5),
  lat = c(21.3069, 19.5, 19.8968, 22.0)
)
example_points_sf <- sf::st_as_sf(
  df,
  coords = c("lon", "lat"),
  crs = 4326
)


point_values <- calculate_covariates(
  covariate = "huc",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  geom = "sf"
)

print(point_values)
```
