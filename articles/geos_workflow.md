# NASA GEOS-CF

This article demonstrates a compact workflow for NASA GEOS-CF data.
GEOS-CF downloads require a NASA EarthData token; see
`protected_datasets` for setup details.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

Each workflow uses two compact example surfaces created from the
processed GEOS extent: two points for point extraction and a small
hexagon grid for polygon extraction.

## Available inputs and data availability

`download_data(dataset_name = "geos", ...)` wraps
[`download_geos()`](https://niehs.github.io/amadeus/reference/download_geos.md).

- `collection` accepts six hourly GEOS-CF global NetCDF collections:
  `aqc_tavg_1hr_g1440x721_v1`, `chm_tavg_1hr_g1440x721_v1`,
  `met_tavg_1hr_g1440x721_x1`, `xgc_tavg_1hr_g1440x721_x1`,
  `chm_inst_1hr_g1440x721_p23`, and `met_inst_1hr_g1440x721_p23`.
- `date` can be a single day or a start/end range; each requested day
  expands to hourly `.nc4` files for every selected collection.
- Standard downloads retrieve the full 1440×721 global grid; study-area
  clipping usually happens later in
  [`process_covariates()`](https://niehs.github.io/amadeus/reference/process_covariates.md).
- NASA EarthData authentication is required; `amadeus` reads
  `NASA_EARTHDATA_TOKEN` or accepts a token explicitly.
- The wrapper validates the first requested URL before downloading and
  stops early when a requested date is unavailable.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "geos_workflow")
geos_live_extent <- c(-79.2, 35.8, -78.6, 36.3)
geos_demo_collection <- "aqc_tavg_1hr_g1440x721_v1"
geos_demo_date <- "2019-09-09"
geos_download_error <- NULL


download_data(
  dataset_name = "geos",
  collection = geos_demo_collection,
  date = geos_demo_date,
  directory_to_save = directory_to_save,
  acknowledgement = TRUE
)

geos_live_files <- list.files(
  file.path(directory_to_save, geos_demo_collection),
  pattern = "\\.nc4$",
  recursive = TRUE,
  full.names = TRUE
)
geos_live_ready <- length(geos_live_files) > 0
if (!geos_live_ready) {
  message(
    "Using packaged GEOS test files for downstream chunks because live files ",
    "are unavailable."
  )
}
```

## Process one workflow-ready data product

``` r

geos_process_path <- if (isTRUE(geos_live_ready)) {
  file.path(directory_to_save, geos_demo_collection)
} else {
  geos_sample_path
}
geos_process_date <- if (isTRUE(geos_live_ready)) {
  geos_demo_date
} else {
  "2018-01-01"
}

processed_data <- process_covariates(
  covariate = "geos",
  variable = "O3",
  date = geos_process_date,
  path = geos_process_path,
  daily_agg = TRUE,
  extent = terra::ext(
    geos_live_extent[1], geos_live_extent[3],
    geos_live_extent[2], geos_live_extent[4]
  )
)

processed_extent <- terra::ext(processed_data)
processed_bbox <- sf::st_bbox(
  c(
    xmin = terra::xmin(processed_extent),
    ymin = terra::ymin(processed_extent),
    xmax = terra::xmax(processed_extent),
    ymax = terra::ymax(processed_extent)
  ),
  crs = sf::st_crs(4326)
)
```

## Calculate covariates at points

``` r

domain_x <- c(terra::xmin(processed_extent), terra::xmax(processed_extent))
domain_y <- c(terra::ymin(processed_extent), terra::ymax(processed_extent))
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
  covariate = "geos",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 100,
  fun = "mean",
  geom = "sf"
)
```

## Visualize the point outputs

``` r

point_plot_col <- grep("^O3", names(point_values), value = TRUE)[1]
if (is.na(point_plot_col) || point_plot_col == "") {
  excluded <- c("site_id", "h3_id", attr(point_values, "sf_column"))
  fallback_cols <- setdiff(names(point_values), excluded)
  fallback_numeric <- fallback_cols[vapply(point_values[fallback_cols], is.numeric, logical(1))]
  point_plot_col <- fallback_numeric[1]
}

point_bbox <- sf::st_bbox(point_values)
point_pad_x <- max((point_bbox[["xmax"]] - point_bbox[["xmin"]]) * 0.08, 0.03)
point_pad_y <- max((point_bbox[["ymax"]] - point_bbox[["ymin"]]) * 0.08, 0.03)

point_basemap <-
  sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

ggplot2::ggplot() +
  ggplot2::geom_sf(data = point_basemap) +
  ggplot2::geom_sf(
    data = point_values,
    ggplot2::aes(color = .data[[point_plot_col]]),
    size = 2.2
  ) +
  ggplot2::coord_sf(
    xlim = c(point_bbox[["xmin"]] - point_pad_x, point_bbox[["xmax"]] + point_pad_x),
    ylim = c(point_bbox[["ymin"]] - point_pad_y, point_bbox[["ymax"]] + point_pad_y),
    expand = FALSE,
    datum = NA
  ) +
  ggplot2::scale_color_viridis_c(option = "C") +
  ggplot2::labs(
    title = "NASA GEOS-CF: point extraction",
    color = point_plot_col
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.key.width = grid::unit(2, "cm"),
    legend.title = ggplot2::element_text(face = "bold")
  )
```
