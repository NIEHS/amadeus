# NASA SEDAC gROADS

This article demonstrates a compact workflow for NASA SEDAC gROADS roads
data. gROADS downloads require a NASA EarthData token.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

`download_data(dataset_name = "sedac_groads", ...)` wraps
[`download_groads()`](https://niehs.github.io/amadeus/reference/download_groads.md).

- `data_region` accepts `"Americas"`, `"Global"`, `"Africa"`, `"Asia"`,
  `"Europe"`, `"Oceania East"`, or `"Oceania West"`.
- `data_format` accepts `"Shapefile"` or `"Geodatabase"`, but the global
  release is only served as a geodatabase; if you request
  `data_region = "Global"` with `"Shapefile"`, `amadeus` automatically
  switches to geodatabase output.
- gROADS is a static road-network product, so there are no date or
  version arguments in the download wrapper.
- Downloads are packaged by region rather than by bounding box; use
  [`process_covariates()`](https://niehs.github.io/amadeus/reference/process_covariates.md)
  to clip to a local study area.
- NASA EarthData authentication is required, and each request downloads
  one zip archive that can be unzipped automatically and optionally
  deleted afterward.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "groads_workflow")
download_data(
  dataset_name = "sedac_groads",
  data_region = "Americas",
  data_format = "Shapefile",
  directory_to_save = directory_to_save,
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = FALSE
)
```

## Process one workflow-ready data product

``` r

path = list.files(
  directory_to_save,
  pattern = "\\.(shp)$",
  recursive = TRUE,
  full.names = TRUE
)[1]
print(path)

processed_data <- process_covariates(
  covariate = "groads",
  path = path,
  extent = terra::ext(-79.2, -78.6, 35.8, 36.3)
)

plot(processed_data)
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
  covariate = "groads",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 1000,
  fun = "sum",
  geom = "sf"
)
```

## Visualize the point and polygon outputs

``` r

sf_process <- sf::st_as_sf(processed_data)
 # create 1-km buffers from points (in a projected CRS, then back to WGS84)
 buffers_1km <- example_points_sf |>
   st_transform(3857) |>
   st_buffer(dist = 1000) |>
   st_transform(4326)

 # join extracted values so fill matches each site
 buffers_1km <- merge(
   buffers_1km,
   st_drop_geometry(point_values)[, c("site_id", "GRD_DENKM_01000")],
   by = "site_id",
   all.x = TRUE
 )

  ggplot() +
    geom_sf(data = sf_process, color = "grey40") +
    geom_sf(data = buffers_1km, aes(fill = GRD_DENKM_01000), alpha = 0.8) +
   scale_fill_gradientn(
     colours = c("#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b"),
     values = scales::rescale(c(0, 0.001, 0.25, 1)),
     breaks = c(0, 0.25, 0.5, 0.75, 1.0),
     labels = c("0 (none)", "0.25", "0.5", "0.75", "1.0"),
     limits = c(0, 1),
      oob = scales::squish,
      name = "Road density\n(km / km²)"
    ) +
    ggtitle("Road density within 1km of points") +
    theme(legend.position = "bottom")
```
