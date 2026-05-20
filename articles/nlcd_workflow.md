# MRLC National Land Cover Database (NLCD)

This article demonstrates a compact workflow for NLCD land-cover
products.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

- `download_data(dataset_name = "nlcd", ...)` accepts `product` values
  of `"Land Cover"`, `"Land Cover Change"`, `"Land Cover Confidence"`,
  `"Fractional Impervious Surface"`, `"Impervious Descriptor"`, and
  `"Spectral Change Day of Year"`.
- NLCD is a nationwide U.S. product at 30 m resolution, with `year`
  values available from 1985 through 2024.
- Downloads arrive as zipped MRLC raster bundles.
- Interpretation depends on the product: land-cover-style layers are
  class/code rasters, fractional impervious surface is a continuous
  percent-style surface, and spectral change day of year encodes the
  timing of detected change.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "nlcd_workflow")
download_data(
  dataset_name = "nlcd",
  product = "Land Cover",
  year = 2021,
  directory_to_save = directory_to_save,
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = FALSE
)
```

## Process one workflow-ready data product

``` r

data_dir <- paste0(directory_to_save, "/data_files")

processed_data <- process_covariates(
  covariate = "nlcd",
  path = data_dir,
  year = 2021
)
```

## Calculate covariates at points

``` r


 example_points_terra <- terra::spatSample(
   processed_data[[1]],
   size = 25,
   method = "regular",
   as.points = TRUE,
   na.rm = TRUE,
   values = FALSE
 )
 example_points_sf <- sf::st_as_sf(example_points_terra)
 example_points_sf$site_id <- paste0("site_", seq_len(nrow(example_points_sf)))

 point_values <- calculate_covariates(
   covariate = "nlcd",
   from = processed_data,
   locs = example_points_sf,
   locs_id = "site_id",
   mode = "terra",
   radius = 1000,
   geom = "sf",
   drop = FALSE
 )

print(point_values, n = 25)
```
