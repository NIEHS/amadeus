# US EPA Toxic Release Inventory (TRI)

This article demonstrates a compact workflow for EPA TRI facility
emissions data.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

[`download_tri()`](https://niehs.github.io/amadeus/dev/reference/download_tri.md)
works with EPA’s annual TRI basic data files and exposes a small set of
high-value selectors:

- `jurisdiction` supports the nationwide file (`"US"`), any two-letter
  state or territory code such as `"NC"`, or the tribal file (`"tbl"`).
- `year` accepts a single year or a start/end pair, so multi-year
  requests download one annual TRI file per year.
- TRI downloads are delivered directly as CSV files rather than zip
  archives.
- Output names reflect the jurisdiction requested: U.S.-wide files keep
  the historical `tri_raw_<year>.csv` pattern, while state and tribal
  requests append a jurisdiction suffix such as `_NC` or `_tbl`.
- TRI does not require authentication. Because these are annual
  facility-reported releases and waste-management totals, temporal
  resolution is yearly.

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "tri_workflow")
download_data(
  dataset_name = "tri",
  year = 2023L,
  jurisdiction = "US",
  directory_to_save = directory_to_save,
  acknowledgement = TRUE
)
```

## Demonstate processing and covariate calculation for a single chemical

The helper function
[`get_tri_info()`](https://niehs.github.io/amadeus/dev/reference/get_tri_info.md)
lists the available chemicals in the downloaded files, which can be used
to filter the processing and covariate calculation steps. Here we
demonstrate with Polychlorinated biphenyls (PCBs), a group of persistent
organic pollutants that were widely used in industrial applications.

``` r


chems <- get_tri_info(path = directory_to_save, type = "chemicals")

processed_pcb <- process_covariates(
  covariate = "tri",
  path = directory_to_save,
  chemical = c("Polychlorinated biphenyls"),
  year = 2023
)
# Note that extent is an option in process_covariates() to limit the domain 
```

## Calculate covariates at points

``` r

domain_x <- c(terra::xmin(processed_pcb), terra::xmax(processed_pcb))
domain_y <- c(terra::ymin(processed_pcb), terra::ymax(processed_pcb))
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


point_values_pcb <- calculate_covariates(
  covariate = "tri",
  from = processed_pcb,
  locs = example_points_sf,
  locs_id = "site_id",
  decay_range = 5000, # 5 km decay range for illustrative purposes
  use_threshold = FALSE,
  geom = "sf"
)
```

## Plot the covariates at points along with the facility locations

``` r

pcb_sf <- sf::st_as_sf(processed_pcb)
point_basemap <-
  sf::st_as_sf(maps::map("usa", plot = FALSE, fill = FALSE))

ggplot() + 
  geom_sf(data = point_basemap, fill = "gray80", color = "white") +
  geom_sf(data = pcb_sf, color = "red", size = 2) + 
  geom_sf(data = point_values_pcb, aes(color = STACK_AIR_0001336363_05000), size = 3) 
```

## Demonstate with multiple chemicals and a total emissions

``` r



processed_chems <- process_covariates(
  covariate = "tri",
  path = directory_to_save,
  year = 2023,
  variables = "ON-SITE RELEASE TOTAL"
)

point_values_chems <- calculate_covariates(
  covariate = "tri",
  from = processed_chems,
  locs = example_points_sf,
  locs_id = "site_id",
  decay_range = 5000, # 5 km decay range for illustrative purposes
  use_threshold = FALSE,
  geom = "sf"
)
```
