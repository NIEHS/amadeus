# NOAA HMS Smoke

This article demonstrates a compact workflow for NOAA HMS smoke plume
polygons.

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

[`download_hms()`](https://niehs.github.io/amadeus/dev/reference/download_hms.md)
exposes the main choices that determine what NOAA smoke files are
available:

- `date` accepts a single day or a start/end range in `YYYY-MM-DD`
  format, with availability beginning on `2005-08-05`.
- HMS smoke products are daily files, so multi-day requests download one
  plume file per day in the requested window.
- `data_format` supports `"Shapefile"` and `"KML"`.
- Shapefile requests download zipped archives that can be unzipped
  automatically; KML requests download raw `.kml` files directly.
- HMS does not require authentication, and availability is determined by
  the NOAA daily smoke-product archive for the dates requested.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "hms_workflow")
download_data(
  dataset_name = "hms",
  date = c("2022-06-10", "2022-06-13"),
  data_format = "Shapefile",
  directory_to_save = directory_to_save,
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = FALSE
)
```

## Process one workflow-ready data product

``` r

processed_data <- process_covariates(
  covariate = "hms",
  date = c("2022-06-10", "2022-06-13"),
  path = dirname(list.files(
    directory_to_save,
    pattern = "\\.shp$",
    recursive = TRUE,
    full.names = TRUE
  )[1]),
  extent = terra::ext(-124.8, -116.4, 41.9, 46.3)
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
  covariate = "hms",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 5000,
  geom = "sf"
)
```

## Visualize the point outputs

``` r


sf_process <- sf::st_as_sf(processed_data)
 # create 1-km buffers from points (in a projected CRS, then back to WGS84)
 buffers_5km <- example_points_sf |>
   st_transform(3857) |>
   st_buffer(dist = 5000) |>
   st_transform(4326)

 # join extracted values so fill matches each site
 buffers_5km <- merge(
   buffers_5km,
   st_drop_geometry(point_values)[, c("site_id", "light_05000", "medium_05000")],
   by = "site_id",
   all.x = TRUE
 )
 buffers_5km$smoke_class <- ifelse(
   is.na(buffers_5km$medium_05000), "No smoke",
   ifelse(
     buffers_5km$medium_05000 > 0, "Medium smoke",
     ifelse(buffers_5km$light_05000 > 0, "Light smoke", "No smoke")
   )
 )
 buffers_5km$smoke_class <- factor(
   buffers_5km$smoke_class,
   levels = c("No smoke", "Light smoke", "Medium smoke")
 )

  ggplot() +
    geom_sf(data = sf_process, fill = NA, color = "grey40") +
    geom_sf(data = buffers_5km, aes(fill = smoke_class), alpha = 0.8) +
   scale_fill_manual(
     values = c(
       "No smoke" = "#f7f7f7",
       "Light smoke" = "#fddbc7",
       "Medium smoke" = "#ef8a62"
     ),
     name = "Smoke category"
    ) +
    ggtitle("Light/medium smoke within 5km of points") +
    theme(legend.position = "bottom")
```
