# NASA MODIS

This article demonstrates how MODIS, combined MODIS, and VIIRS-style
products move through `amadeus` via
[`download_data()`](https://niehs.github.io/amadeus/dev/reference/download_data.md),
[`process_modis_merge()`](https://niehs.github.io/amadeus/dev/reference/process_modis_merge.md),
[`process_modis_swath()`](https://niehs.github.io/amadeus/dev/reference/process_modis_swath.md),
[`process_blackmarble()`](https://niehs.github.io/amadeus/dev/reference/process_blackmarble.md),
`process_mcd14ml()`, and
[`calculate_covariates()`](https://niehs.github.io/amadeus/dev/reference/calculate_covariates.md).
MODIS downloads require a NASA EarthData token.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

Each workflow uses two small example surfaces created within the MODIS
example extent: two `sf` points in the upper half of the bounding box
for point extraction, and a subset of packaged Durham County Uber H3
resolution-8 hexagons filtered to the same upper-half extent for polygon
extraction.

## Available inputs and data availability

`download_data(dataset_name = "modis", ...)` wraps
[`download_modis()`](https://niehs.github.io/amadeus/dev/reference/download_modis.md).

[TABLE]

- `version` defaults to `"061"`, but
  [`download_modis()`](https://niehs.github.io/amadeus/dev/reference/download_modis.md)
  internally rewrites `MOD06_L2` to `6.1`, `MOD14CM1` / `MYD14CM1` to
  `005`, `MCD64CMQ` to `006`, `MCD14ML` to `6.1NRT`, and omits the
  version string for `VNP46A2` and `VNP64A1`.
- `date` can be a single day or a range. Most products must stay within
  one calendar year; cross-year ranges are supported for `MOD06_L2`,
  `MOD14CM1`, `MYD14CM1`, `MCD14ML`, `MCD64A1`, `MCD64CMQ`, and
  `VNP64A1`.
- `extent` limits the NASA CMR query to a study-area bounding box so
  only intersecting tiles are downloaded.
- Downloads are saved as raw `.hdf` or `.h5` granules, except `MCD14ML`,
  which is saved as `.txt`. The table now includes a subdataset-name
  column populated from local granule inspection
  (`terra::rast(path); names(r)`) where available, plus product-specific
  selector patterns. Use `terra::describe(path, sds = TRUE)` to inspect
  the complete subdataset list for your exact files and collection
  version.

## Live representative workflow: `MOD11A1`

The evaluated chunks below keep one compact, end-to-end `MOD11A1`
land-surface temperature example that still exercises the download,
process, calculate, and plotting stages. We will demonstate downloading,
processing with temporal aggregation, processing with daily resolution,
covariate calculating with aggregation and daily resolutions, and
plotting. The workflow for other products is similar, but the exact
subdataset names and processing parameters may differ.

``` r

directory_to_save <- file.path(tempdir(), "modis_workflow", "MOD11A1")
modis_extent <- c(-79.2, 35.8, -78.6, 36.3)
download_data(
  dataset_name = "modis",
  product = "MOD11A1",
  version = "061",
  extent = modis_extent,
  date = c("2019-08-15","2019-08-18"),
  directory_to_save = directory_to_save,
  acknowledgement = TRUE
)
```

## Process one workflow-ready land-surface temperature product

``` r

modis_files <- list.files(
  directory_to_save,
  pattern = "\\.hdf$",
  recursive = TRUE,
  full.names = TRUE
)

# Process with daily resolution, no temporal aggregation
processed_daily <- process_modis_daily(
  path = modis_files,
  date = c("2019-08-15","2019-08-18"),
  subdataset = "LST_Day_1km"
)

# Processs with temporal averaging across the 4-day window
processed_avg <- process_modis_merge(
  path = modis_files,
  date = c("2019-08-15","2019-08-18"),
  subdataset = "LST_Day_1km",
  fun_agg = "mean"
)
```

``` r

terra::plot(
  processed_avg,
  main = "MOD11A1 LST_Day_1km averaged over 4 Days (2019-08-15 to 2019-08-18)"
)

terra::plot(
  processed_daily,
  main = "MOD11A1 LST_Day_1km daily resolution (2019-08-15 to 2019-08-18)"
)
```

## Calculate covariates at points - demonstating cacluations at points with buffers

``` r

# Build two example points in the upper half of the MODIS extent and
# subset H3 hexagons to the same upper-half window.
if (!exists("modis_extent")) {
  modis_extent <- c(-79.2, 35.8, -78.6, 36.3)
}

modis_mid_lat <- mean(modis_extent[c(2, 4)])
example_points_sf <- sf::st_as_sf(
  data.frame(
    site_id = c("northwest_point", "northeast_point"),
    lon = c(-79.05, -78.78),
    lat = c(36.22, 36.18)
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

modis_upper_half <- sf::st_as_sfc(sf::st_bbox(
  c(
    xmin = modis_extent[1],
    ymin = modis_mid_lat,
    xmax = modis_extent[3],
    ymax = modis_extent[4]
  ),
  crs = sf::st_crs(4326)
))

durham_hex <- sf::st_filter(
  sf::st_transform(durham_hex, 4326),
  modis_upper_half,
  .predicate = sf::st_intersects
)

# Demonstate calculation at points with the output from the `process` step with daily resolution. The result in a WIDE format sf object

point_values_process <- calculate_covariates(
  covariate = "modis",
  from = processed_daily,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  name_covariates = names(processed_daily),
  fun_summary = "mean",
  geom = "sf",
  scale = "* 0.02 - 273.15"
) |> dplyr::select(-time)

# calculate_modis (or the calculate_covariate version ) can do the process and calculate step all in one. The result is LONG format sf object with a time column. This is the recommended workflow for most users, but the two-step process above allows users to inspect the processed rasters before calculating covariates and to use the same processed rasters for multiple calculate_covariates runs with different parameters.

point_values <- calculate_covariates(
  covariate = "modis",
  from = modis_files,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  preprocess = amadeus::process_modis_merge,
  subdataset = "LST_Day_1km",
  name_covariates = "LST_",
  fun_summary = "mean",
  geom = "terra",
  scale = "* 0.02 - 273.15"
)
```

## Calculate covariates across H3 hexagons - demonstating calculations across polygons

``` r

polygon_values <- calculate_covariates(
  covariate = "modis",
  from = modis_files,
  locs = durham_hex,
  locs_id = "h3_id",
  radius = 0,
  preprocess = amadeus::process_modis_merge,
  subdataset = "LST_Day_1km",
  name_covariates = "LST_",
  fun_summary = "mean",
  geom = "sf",
  scale = "* 0.02 - 273.15"
)
```

## Terra+Aqua fusion for better temporal coverage

[`process_modis_merge()`](https://niehs.github.io/amadeus/dev/reference/process_modis_merge.md)
[`process_modis_daily()`](https://niehs.github.io/amadeus/dev/reference/process_modis_daily.md)
and
[`calculate_modis()`](https://niehs.github.io/amadeus/dev/reference/calculate_modis.md)
support optional Terra+Aqua fusion through `path_secondary` /
`from_secondary` and `fusion_method`.

For paired products (for example `MOD13Q1` + `MYD13Q1`), this allows:

- pixel-wise mean blending where both products exist
  (`fusion_method = "mean"`)
- fallback to whichever product is available on a date
- optional priority fallback (`"primary_first"` / `"secondary_first"`)

``` r

directory_aqua <- file.path(tempdir(), "modis_workflow", "MYD11A1")

download_data(
  dataset_name = "modis",
  product = "MYD11A1",
  version = "061",
  extent = modis_extent,
  date = c("2019-08-15","2019-08-18"),
  directory_to_save = directory_aqua,
  acknowledgement = TRUE
)

aqua_files <- list.files(
  directory_aqua,
  pattern = "\\.hdf$",
  recursive = TRUE,
  full.names = TRUE
)

processed_comb <- process_modis_daily(
  path = modis_files,
  path_secondary = aqua_files,
  date = c("2019-08-15","2019-08-18"),
  subdataset = "LST_Day_1km",
  fun_agg = "mean"
)

# Process and calulate together with fusion in one step with calculate_covariates. The result is LONG format sf object with a time column.
calculated_comb <- calculate_covariates(
  covariate = "modis",
  from = modis_files,
  from_secondary = aqua_files,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  preprocess = amadeus::process_modis_daily,
  subdataset = "LST_Day_1km",
  name_covariates = "LST_",
  fun_summary = "mean",
  geom = "sf",
  scale = "* 0.02 - 273.15"
)

# Use the process stage output from the complementary fusion as input to calculate_covariates for a more traditional workflow. The result is LONG format sf object with a time column.
calculated_comb_process <- calculate_covariates(
  covariate = "modis",
  from = processed_comb,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  name_covariates = names(processed_comb),
  fun_summary = "mean",
  geom = "sf",
  scale = "* 0.02 - 273.15"
) |> dplyr::select(-time)
```

## Visualize the process and calculate results

First, we visualize the process daily rasters with the point
caclulations overlayed.

``` r

r_time <- point_values$time


raster_plot_df <- do.call(rbind, lapply(seq_len(terra::nlyr(processed_daily)), function(i) {
  r_i <- processed_daily[[i]] * 0.02 - 273.15
  r_i <- terra::aggregate(r_i, fact = 6, fun = mean, na.rm = TRUE)
  df_i <- terra::as.data.frame(r_i, xy = TRUE, na.rm = TRUE)
  names(df_i)[3] <- "lst_c"
  df_i$plot_date <- as.Date(r_time[i])
  df_i
}))

point_values_modis <- terra::project(point_values, terra::crs(processed_daily))
point_df <- cbind(
  terra::as.data.frame(point_values_modis),
  terra::geom(point_values_modis)[, c("x", "y")]
)
point_value_col <- grep("^LST_|^lst_", names(point_df), value = TRUE)[1]
point_df$point_value <- point_df[[point_value_col]]
point_df$plot_date <- as.Date(point_df$time)
point_df <- point_df[point_df$plot_date %in% unique(raster_plot_df$plot_date), ]

shared_limits <- range(
  c(raster_plot_df$lst_c, point_df$point_value),
  na.rm = TRUE,
  finite = TRUE
)

ggplot2::ggplot() +
  ggplot2::geom_raster(
    data = raster_plot_df,
    ggplot2::aes(x = x, y = y, fill = lst_c)
  ) +
  ggplot2::geom_point(
    data = point_df,
    ggplot2::aes(x = x, y = y),
    shape = 17,
    size = 4.8,
    color = "black",
    alpha = 0.95
  ) +
  ggplot2::geom_point(
    data = point_df,
    ggplot2::aes(x = x, y = y, color = point_value),
    shape = 17,
    size = 3.6,
    alpha = 0.95
  ) +
  ggplot2::facet_wrap(~plot_date, ncol = 2) +
  ggplot2::coord_equal(expand = FALSE) +
  ggplot2::scale_fill_viridis_c(option = "C", limits = shared_limits) +
  ggplot2::scale_color_viridis_c(option = "C", limits = shared_limits) +
  ggplot2::labs(
    title = "MOD11A1 daily LST with same-day point covariate overlays",
    fill = "Raster LST (C)",
    color = "Point LST (C)"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.key.width = grid::unit(2, "cm"),
    legend.title = ggplot2::element_text(face = "bold")
  )
```

## Workflow template for using .by_time to create space-time summaries

Here we demonstrate with EVI a monthly temporal aggregation by polygon.
For MODIS datasets, it is faster and recommended to do the calculate
step directly and provide the process via the `preprocess` input. A
demonstration of the recommended approach and the alternative (which
requires using terra::tapp) are below.

#### Vegetation indices: `MOD13*` or `MYD13*`

``` r

vegetation_dir <- file.path(tempdir(), "modis_workflow", "vegetation")
vegetation_window <- c("2019-01-01", "2019-04-30")
download_modis(
  product = "MOD13A2",
  version = "061",
  date = vegetation_window,
  extent = modis_extent,
  directory_to_save = vegetation_dir,
  acknowledgement = TRUE
)

vegetation_files <- list.files(
  vegetation_dir,
  pattern = "\\.hdf$",
  recursive = TRUE,
  full.names = TRUE
)

# For MODIS data, it is faster to do the process and calculate in one step with calculate_covariates and provide the process as the preprocess function.

evi_points_monthly <- calculate_covariates(
  covariate = "modis",
  from = vegetation_files, # provide the path to the files from download_modis
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  preprocess = amadeus::process_modis_merge,
  subdataset = "(EVI)",
  name_covariates = "evi_",
  .by_time = "month", # specify the temporal aggregation period for the output points
  fun_summary = "mean",
  geom = "sf",
  scale = "* 0.0001"
)

print(evi_points_monthly)

# If we want to view the processed rasters we can use process_modis_merge or process_modis_daily. 
# process_modis_merge without .by_time will give us one raster with the temporal aggregation across the whole window. process_modis_daily will give us daily rasters that we can then aggregate with terra::tapp or similar functions.
vegetation_raster <- process_modis_merge(
  path = vegetation_files,
  date = vegetation_window,
  subdataset = "(EVI)",
  fun_agg = "mean"
)

vegetation_daily_raster <- process_modis_daily(
  path = vegetation_files,
  date = vegetation_window,
  subdataset = "(EVI)"
)

terra::plot(vegetation_raster, main = "MOD13A2 EVI averaged over Jan-Apr 2019")
terra::plot(vegetation_daily_raster)
```

This same averaging pattern applies to `MOD13A1`, `MYD13A1`, and
`MYD13A2` when you use a short window that spans two valid 16-day
composites. For the monthly products `MOD13A3` and `MYD13A3`, request a
date range covering the target month so the monthly granule is included.

### Fire grids: `MOD14A1`, `MYD14A1`, `MOD14CM1`, and `MYD14CM1`

When using fire-grid products with the `FireMask` layer, the raw values
are typically interpreted as follows.

| Raw value | Meaning | Binary fire mask? |
|---:|:---|:---|
| 0 | not processed, missing input | NA / no observation |
| 1 | obsolete, not used since Collection 1 | NA |
| 2 | not processed, other reason | NA |
| 3 | non-fire water pixel | 0 |
| 4 | cloud, land or water | NA or 0, depending on analysis |
| 5 | non-fire land pixel | 0 |
| 6 | unknown, land or water | NA |
| 7 | fire, low confidence | 1, or exclude for stricter mask |
| 8 | fire, nominal confidence | 1 |
| 9 | fire, high confidence | 1 |

Given the typical interpretation of the `FireMask` layer, users often
want to create a binary fire mask where 1 indicates a fire detection and
0 indicates no fire. The exact values to include in the binary fire mask
depend on the confidence level you want to include. For example, you
might choose to include only high-confidence fires (raw value 9) or to
include both nominal and high-confidence fires (raw values 8 and 9).
Low-confidence fires (raw value 7) can be included for a more inclusive
mask, but they may also include more false positives. To create the
binary fire mask from the process or calculate amadeus functions, we
simply add a scale argument to recode the raw values to 1s and 0s based
on the confidence levels we want to include. For example, if we want to
include both nominal and high-confidence fires, we can use
`scale = " %in% c(8, 9)"` to recode raw values 8 and 9 to 1 and all
other values to 0.

``` r

fire_grid_dir <- file.path(tempdir(), "modis_workflow", "fire_grid")
fire_grid_window <- c("2019-01-01", "2019-04-30")
download_data(
  dataset_name = "modis",
  product = "MOD14A1",
  version = "061",
  date = fire_grid_window,
  extent = modis_extent,
  directory_to_save = fire_grid_dir,
  acknowledgement = TRUE
)


fire_grid_files <- list.files(
  fire_grid_dir,
  pattern = "\\.hdf$",
  recursive = TRUE,
  full.names = TRUE
)


fire_grid_points <- calculate_covariates(
  covariate = "modis",
  from = fire_grid_files,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  preprocess = amadeus::process_modis_merge,
  subdataset =  process_modis_sds("MOD14A1"),
  name_covariates = "firemask_",
  fun_summary = "mean",
  geom = "sf",
  scale = " %in% c(8, 9)" # use c(7, 8, 9) to include low-confidence fires
)

# Let's also have a look at the processed data 

fire_grid_raster <- process_modis_daily(
  path = fire_grid_files,
  date = fire_grid_window,
  subdataset = process_modis_sds("MOD14A1")
)
fire_grid_raster <- fire_grid_raster %in% c(8, 9) # use c(7, 8, 9) to include low-confidence fires

terra::plot(fire_grid_raster, main = "MOD14A1 processed raster")
```

### MAIAC aerosol: `MCD19A2`

MAIAC MCD19A2 provides information on daily atmospheric properties, of
which the most helpful are likely aerosol optical depth and smoke plume
height.

``` r

maiac_dir <- file.path(tempdir(), "modis_workflow", "maiac")
MAIAC_grid_window <- c("2019-05-01", "2019-05-30")
maiac_extent <- c(-124.5, 32.5, -114.0, 42.0) # California
download_data(
  dataset_name = "modis",
  product = "MCD19A2",
  version = "061",
  date = MAIAC_grid_window,
  extent = maiac_extent,
  directory_to_save = maiac_dir,
  acknowledgement = TRUE
)

maiac_files <- list.files(
  maiac_dir,
  pattern = "\\.hdf$",
  recursive = TRUE,
  full.names = TRUE
)

maiac_raster <- process_modis_daily(
  path = maiac_files,
  date = MAIAC_grid_window,
  subdataset = "Optical_Depth_047",
  scale = "* 0.001"
)

maiac_points <- calculate_covariates(
  covariate = "modis",
  from = maiac_files,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  preprocess = amadeus::process_modis_merge,
  subdataset = "Optical_Depth_047",
  name_covariates = "aod_047_",
  fun_summary = "mean",
  geom = "sf", 
  scale = "* 0.001"
)

terra::plot(maiac_raster, main = "MCD19A2 AOD processed raster")

maiac_PH <- process_modis_daily(
  path = maiac_files,
  date = MAIAC_grid_window,
  subdataset = "(Injection_Height)"
)

terra::plot(maiac_PH, main = "MCD19A2 Plume Injection Height processed raster")
```

### Nighttime lights: `VNP46A2`

``` r

blackmarble_dir <- file.path(tempdir(), "modis_workflow", "blackmarble")
download_data(
  dataset_name = "modis",
  product = "VNP46A2",
  date = "2019-08-15",
  extent = modis_extent,
  directory_to_save = blackmarble_dir,
  acknowledgement = TRUE
)

blackmarble_files <- list.files(
  blackmarble_dir,
  pattern = "\\.h5$",
  recursive = TRUE,
  full.names = TRUE
)

blackmarble_raster <- process_blackmarble(
  path = blackmarble_files,
  date = "2019-08-15",
  tile_df = process_blackmarble_corners(hrange = c(8, 10), vrange = c(4, 5)),
  subdataset = 3L,
  crs = "EPSG:4326"
)

blackmarble_points <- calculate_covariates(
  covariate = "modis",
  from = blackmarble_files,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  preprocess = amadeus::process_blackmarble,
  tile_df = process_blackmarble_corners(hrange = c(8, 10), vrange = c(4, 5)),
  subdataset = 3L,
  name_covariates = "blackmarble_",
  fun_summary = "mean",
  geom = "sf"
)

terra::plot(blackmarble_raster, main = "VNP46A2 processed raster")

print(blackmarble_points)
```
