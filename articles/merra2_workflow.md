# NASA MERRA-2

This article demonstrates a compact, multi-variable workflow for NASA
MERRA-2 data. Standard MERRA-2 GES DISC downloads require a NASA
EarthData token, while the public FWI collection does not.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

`download_data(dataset_name = "merra2", ...)` wraps
[`download_merra2()`](https://niehs.github.io/amadeus/reference/download_merra2.md).

`process_covariates(covariate = "merra2", variable = ...)` accepts
native layer names from the selected MERRA-2 collection. For the new
public FWI product, processable variables are `DC`, `DMC`, `FFMC`,
`ISI`, `BUI`, and `FWI` (or the raw layer name, such as
`MERRA2.CORRECTED_FWI`).

| Supported collection family | Temporal cadence | File type returned by download | Authentication / source | Representative variables you can process |
|:---|:---|:---|:---|:---|
| `inst1_2d_asm_Nx`, `inst1_2d_int_Nx`, `inst1_2d_lfo_Nx` | Hourly instantaneous | Daily `.nc4` | NASA EarthData token required | `CPT` |
| `inst3_2d_gas_Nx` | 3-hourly instantaneous | Daily `.nc4` | NASA EarthData token required | `AODANA` |
| `inst3_3d_asm_Np`, `inst3_3d_aer_Nv`, `inst3_3d_asm_Nv`, `inst3_3d_chm_Nv`, `inst3_3d_gas_Nv`, `inst6_3d_ana_Np`, `inst6_3d_ana_Nv` | 3-hourly or 6-hourly instantaneous | Daily `.nc4` | NASA EarthData token required | `AIRDENS`, `SLP` |
| `tavg1_2d_adg_Nx`, `tavg1_2d_aer_Nx`, `tavg1_2d_chm_Nx`, `tavg1_2d_csp_Nx`, `tavg1_2d_flx_Nx`, `tavg1_2d_int_Nx`, `tavg1_2d_lfo_Nx`, `tavg1_2d_lnd_Nx`, `tavg1_2d_ocn_Nx`, `tavg1_2d_rad_Nx`, `tavg1_2d_slv_Nx` | Hourly time averaged | Daily `.nc4` | NASA EarthData token required | `BCEMAN`, `COCL` |
| `tavg3_3d_mst_Ne`, `tavg3_3d_trb_Ne`, `tavg3_3d_nav_Ne`, `tavg3_3d_cld_Np`, `tavg3_3d_mst_Np`, `tavg3_3d_rad_Np`, `tavg3_3d_tdt_Np`, `tavg3_3d_trb_Np`, `tavg3_3d_udt_Np`, `tavg3_3d_odt_Np`, `tavg3_3d_qdt_Np`, `tavg3_3d_asm_Nv`, `tavg3_3d_cld_Nv`, `tavg3_3d_mst_Nv`, `tavg3_3d_rad_Nv`, `tavg3_2d_glc_Nx` | 3-hourly time averaged | Daily `.nc4` | NASA EarthData token required | `DUDTANA` |
| `statD_2d_slv_Nx` | Daily statistics | Daily `.nc4` | NASA EarthData token required | `HOURNORAIN` |
| `fwi` | Daily corrected fire weather | Daily `.nc` | Public GlobalFWI portal; no token required | `DC`, `DMC`, `FFMC`, `ISI`, `BUI`, `FWI` |

| Collection | Example `variable =` value | What the processed output looks like |
|:---|:---|:---|
| `inst1_2d_int_Nx` | `CPT` | Hourly layers with date and hour in layer names |
| `inst3_2d_gas_Nx` | `AODANA` | 3-hourly layers with date and hour in layer names |
| `inst3_3d_chm_Nv` | `AIRDENS` | 3-hourly layers; 3-D collection keeps pressure-level metadata |
| `inst6_3d_ana_Np` | `SLP` | 6-hourly layers; 3-D collection keeps pressure-level metadata |
| `statD_2d_slv_Nx` | `HOURNORAIN` | Daily layers with collection-specific timestamps |
| `tavg1_2d_chm_Nx` | `COCL` | Hourly layers with date and hour in layer names |
| `tavg3_3d_udt_Np` | `DUDTANA` | 3-hourly layers; 3-D collection keeps pressure-level metadata |
| `fwi` | `FWI` or raw layer `MERRA2.CORRECTED_FWI` | Daily layers named as `MERRA2.CORRECTED.<var>_<YYYYMMDD>` |

- `date` can be a single day or a start/end range.
- Downloads are global only; clip to a study area during processing
  rather than at download time.
- Standard GES DISC collections save companion `.xml` metadata files
  under each collection’s `metadata/` folder.
- If you need to discover additional variables inside a downloaded file,
  inspect the native layer names with `names(terra::rast(path_to_file))`
  before calling
  [`process_covariates()`](https://niehs.github.io/amadeus/reference/process_covariates.md).

## Workflow demonstration variables

The live example below processes six variables spanning standard MERRA-2
and the new public FWI product. The point and polygon extraction chunks
focus on the two FWI layers so the extracted values stay dense over the
Durham example locations and hexagons, and a separate hourly `BCEMAN`
example shows how to roll a 1-hour product up to a daily summary for
extraction.

| Collection | Variable shown in the workflow | Why it is included |
|:---|:---|:---|
| `inst1_2d_int_Nx` | `CPT` | Instantaneous meteorology / diagnostics example |
| `inst3_2d_gas_Nx` | `AODANA` | 3-hourly gas or aerosol example |
| `statD_2d_slv_Nx` | `HOURNORAIN` | Daily surface statistic example |
| `tavg1_2d_adg_Nx` | `BCEMAN` | Hourly aerosol diagnostics example aggregated to a daily summary |
| `fwi` | `FFMC` | Daily fire weather layer used in extraction demos |
| `fwi` | `FWI` | Daily fire weather layer used in extraction demos |

## Download representative requests

You can use the helper function
[`get_merra2_info()`](https://niehs.github.io/amadeus/reference/get_merra2_info.md)
to query available variables from each collection.

``` r


merra2_demo_specs <- data.frame(
  collection = c(
    "inst1_2d_int_Nx",
    "inst3_2d_gas_Nx",
    "statD_2d_slv_Nx",
    "tavg1_2d_adg_Nx",
    "fwi",
    "fwi"
  ),
  variable = c("CPT", "AODANA", "HOURNORAIN", "BCEMAN", "FFMC", "FWI"),
  date = rep("2024-08-11", 6),
  use_for_extraction = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

directory_to_save <- file.path(tempdir(), "merra2_workflow")
download_data(
  dataset_name = "merra2",
  collection = unique(merra2_demo_specs$collection),
  date = "2024-08-11",
  directory_to_save = directory_to_save,
  acknowledgement = TRUE
)

get_merra2_info(path = paste0(directory_to_save,"/inst1_2d_int_Nx"))
```

This single request intentionally mixes authenticated GES DISC
collections with the public `fwi` collection so the vignette
demonstrates both download paths in the same workflow. \## Process six
workflow-ready data products

``` r

processed_examples <- setNames(
  vector("list", nrow(merra2_demo_specs)),
  merra2_demo_specs$variable
)

find_merra2_collection_dir <- function(root, collection) {
  pattern <- if (collection == "fwi") {
    "^FWI\\..*\\.nc$"
  } else {
    paste0("^MERRA2_[0-9]{3}\\.", collection, "\\..*\\.nc4$")
  }
  files <- list.files(
    root,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(files) == 0) {
    stop("Could not locate files for collection ", collection, ".")
  }
  dirname(files[1])
}

for (i in seq_len(nrow(merra2_demo_specs))) {
  spec <- merra2_demo_specs[i, ]
  processed_examples[[spec$variable]] <- process_covariates(
    covariate = "merra2",
    variable = spec$variable,
    date = spec$date,
    path = find_merra2_collection_dir(directory_to_save, spec$collection)
  )
}
processed_summary <- data.frame(
  variable = merra2_demo_specs$variable,
  collection = merra2_demo_specs$collection,
  n_layers = vapply(processed_examples, function(x) as.integer(terra::nlyr(x)), integer(1)),
  first_output_layer = vapply(processed_examples, function(x) {
    names(x)[1]
  }, character(1)),
  stringsAsFactors = FALSE
)
knitr::kable(
  processed_summary,
  col.names = c(
    "Variable",
    "Collection",
    "Layers returned",
    "First layer name in the processed raster"
  )
)
```

## Plot one processed raster

``` r

terra::plot(
  processed_examples$FWI,
  main = "MERRA-2 corrected FWI on 2024-08-11",
  plg = list(x = "bottom", title = "FWI")
)
```

## Demonstrate hourly MERRA-2 data with `tavg1_2d_adg_Nx`

Many MERRA-2 datasets are at 1, 3, or 6 hour increments. Here, we
inspect the hourly timestamps.

``` r

bceman_hourly_layers <- data.frame(
  layer_name = names(processed_examples$BCEMAN),
  time_utc = as.character(terra::time(processed_examples$BCEMAN)),
  stringsAsFactors = FALSE
)
knitr::kable(
  head(bceman_hourly_layers, 8),
  col.names = c("Hourly layer name", "Timestamp (UTC)")
)
```

## View the daily BCEMAN summary

Process and calculate functions currently return results in the native
time resolution of the data. Here, again, that is hourly. To calculate a
daily summary we simply use an apply style function from `terra`.

``` r

bceman_daily <- terra::app(processed_examples$BCEMAN, mean, na.rm = TRUE)
names(bceman_daily) <- "BCEMAN_20240811_0000"
terra::time(bceman_daily) <- as.POSIXct("2024-08-11 00:00:00", tz = "UTC")
```

``` r

terra::plot(
  bceman_daily,
  main = "Daily mean BCEMAN from tavg1_2d_adg_Nx on 2024-08-11",
  plg = list(x = "bottom", title = "BCEMAN")
)
```

## Calculate daily BCEMAN at points

``` r



df <- data.frame(
  site_id = c("site_1", "site_2", "site_3","site_4"),
  lon = c(-78.6382, -47.8825, 116.4074, 36.8219),
  lat = c(35.7796, -15.7942, 39.9042, -1.2921)
)
example_points_sf <- sf::st_as_sf(
  df,
  coords = c("lon", "lat"),
  crs = 4326
)

bceman_point_values <- calculate_covariates(
  covariate = "merra2",
  from = bceman_daily,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  fun = "mean",
  geom = "sf"
)

print(bceman_point_values)
```

## Calculate covariates at points with dense-coverage FWI layers

``` r

point_ffmc <- calculate_covariates(
  covariate = "merra2",
  from = processed_examples$FFMC,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  fun = "mean",
  geom = "sf"
)
point_fwi <- calculate_covariates(
  covariate = "merra2",
  from = processed_examples$FWI,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  fun = "mean",
  geom = "sf"
)
point_values <- dplyr::left_join(
  point_ffmc,
  sf::st_drop_geometry(point_fwi),
  by = c("site_id", "time")
)

print(point_values)
```

## Visualize the point outputs

``` r


point_basemap <-
  sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

ggplot() + geom_sf(data = point_basemap) + geom_sf(data = point_values,aes(color = `MERRA2.CORRECTED.FWI_0`)) + ggtitle("FWI values at example points on 2024-08-11")
```
