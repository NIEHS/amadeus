# PRISM Climate Data

This article demonstrates a compact workflow for PRISM climate rasters.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

`download_data(dataset_name = "prism", ...)` combines `time`, `element`,
and `data_type`.

| `data_type` | Resolution | Supported `element` values | `time` expectations |
|----|----|----|----|
| `ts` | 4 km time series | `ppt`, `tmin`, `tmax`, `tmean`, `tdmean`, `vpdmin`, `vpdmax` | `YYYYMMDD` for daily data (1981-01-01 through yesterday), `YYYYMM` for monthly data (1981-01 through last month), or `YYYY` for annual data (1981 through last year) |
| `normals_800` | 800 m normals | `ppt`, `tmin`, `tmax`, `tmean`, `tdmean`, `vpdmin`, `vpdmax`, `solslope`, `soltotal`, `solclear`, `soltrans` | Monthly normals use `MM` or `14` for annual normal; daily normals use `MMDD` |
| `normals` | 4 km normals | `ppt`, `tmin`, `tmax`, `tmean`, `tdmean`, `vpdmin`, `vpdmax`, `solslope`, `soltotal`, `solclear`, `soltrans` | Monthly normals use `MM` or `14` for annual normal; daily normals use `MMDD` |

- Historical PRISM bundles also accept `time = "YYYY"` for years
  1895-1980 and return a zip file containing 12 monthly grids plus the
  annual grid.
- `format` is only used for `data_type = "ts"` and can be `nc`, `asc`,
  or `grib2`.
- Major constraints:
  - `sol*` elements are available for normals only, not for `ts`.
  - For normals, `format` is ignored.
  - The PRISM API always returns a zip file, even when a time-series
    format is requested.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "prism_ts_workflow")
download_data(
  dataset_name = "prism",
  time = "201005",
  element = "tmean",
  data_type = "ts",
  directory_to_save = directory_to_save,
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = FALSE
)
```

## Process one workflow-ready data product

``` r

processed_data <- process_covariates(
  covariate = "prism",
  path = list.files(
    paste0(directory_to_save,"/data_files/"),
    pattern = ".nc",
    recursive = TRUE,
    full.names = TRUE
  )[1],
  element = "tmean",
  time = "201005",
  extent = terra::ext(-114.9, -102.0, 31.3, 41.1)
)
```

## Calculate covariates at points

``` r

domain_x <- c(terra::xmin(processed_data), terra::xmax(processed_data))
domain_y <- c(terra::ymin(processed_data), terra::ymax(processed_data))
domain_dx <- diff(domain_x)
domain_dy <- diff(domain_y)

candidate_xy <- expand.grid(
  lon = seq(domain_x[1] + 0.12 * domain_dx, domain_x[2] - 0.12 * domain_dx, length.out = 5),
  lat = seq(domain_y[1] + 0.12 * domain_dy, domain_y[2] - 0.12 * domain_dy, length.out = 5)
)
example_points_sf <- sf::st_as_sf(
  candidate_xy,
  coords = c("lon", "lat"),
  crs = 4326
)
example_points_sf$site_id <- paste0("site_", seq_len(nrow(example_points_sf)))


point_values <- calculate_covariates(
  covariate = "prism",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 1000,
  geom = "sf"
)
```

## Visualize the point outputs

``` r


point_basemap <-
  sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

ggplot() +  
  geom_sf(data = point_basemap, fill = "gray80", color = "white") +
  geom_sf(data = point_values, aes(color = tmean_1000), size = 3) +
  scale_color_viridis_c() + labs(title = "PRISM tmean at example points") +
  coord_sf(xlim = domain_x, ylim = domain_y) 
```
