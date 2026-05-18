# Drought workflows (SPEI, EDDI, USDM)

This article demonstrates drought workflows in `amadeus` for SPEI, EDDI,
and USDM using
[`download_drought()`](https://niehs.github.io/amadeus/dev/reference/download_drought.md),
[`process_drought()`](https://niehs.github.io/amadeus/dev/reference/process_drought.md),
and
[`calculate_drought()`](https://niehs.github.io/amadeus/dev/reference/calculate_drought.md).

The live-download chunk below is provided as a reference and is not
evaluated during vignette builds.

## Available inputs and data availability

[TABLE]

## Optional live download examples

``` r

drought_download_dir <- file.path(tempdir(), "drought_workflow_download")
date_range <- c("2020-01-01", "2020-03-31")
download_drought(
  source = "spei",
  date = date_range,
  timescale = 1L,
  directory_to_save = drought_download_dir,
  acknowledgement = TRUE
)

download_drought(
  source = "eddi",
  date = date_range,
  timescale = 1L,
  directory_to_save = drought_download_dir,
  acknowledgement = TRUE
)

download_drought(
  source = "usdm",
  date = date_range,
  directory_to_save = drought_download_dir,
  acknowledgement = TRUE
)
```

## Process representative drought data

``` r

spei_processed <- process_drought(
  source = "spei",
  path = drought_download_dir,
  date = date_range,
  timescale = 1L
)

eddi_processed <- process_drought(
  source = "eddi",
  path = drought_download_dir,
  date = date_range,
  timescale = 1L
)

usdm_processed <- process_drought(
  source = "usdm",
  path = drought_download_dir,
  date = date_range
)
```

## Plot processed SPEI and EDDI rasters

``` r

spei_map <- terra::as.data.frame(spei_processed[[1]], xy = TRUE, na.rm = TRUE)
names(spei_map)[3] <- "value"
spei_map$source <- "SPEI"

eddi_map <- terra::as.data.frame(eddi_processed[[1]], xy = TRUE, na.rm = TRUE)
names(eddi_map)[3] <- "value"
eddi_map$source <- "EDDI"

map_df <- rbind(spei_map, eddi_map)

ggplot2::ggplot(map_df, ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_raster() +
  ggplot2::facet_wrap(~ source, ncol = 2) +
  ggplot2::scale_fill_viridis_c(option = "C") +
  ggplot2::coord_equal() +
  ggplot2::labs(
    title = "Processed drought rasters",
    subtitle = "First available date from each processed stack",
    x = "Longitude",
    y = "Latitude",
    fill = "Index"
  ) +
  ggplot2::theme_minimal()
```

## Plot processed USDM polygons

``` r

usdm_dates <- sort(unique(terra::values(usdm_processed)$date))
usdm_one_day <- usdm_processed[terra::values(usdm_processed)$date == usdm_dates[1], ]
usdm_sf <- sf::st_as_sf(usdm_one_day)

ggplot2::ggplot(usdm_sf) +
  ggplot2::geom_sf(ggplot2::aes(fill = factor(DM)), color = NA) +
  ggplot2::scale_fill_brewer(palette = "YlOrRd", na.value = "grey85") +
  ggplot2::coord_sf(datum = NA) +
  ggplot2::labs(
    title = "Processed USDM drought classes",
    subtitle = paste("Date:", usdm_dates[1]),
    x = NULL,
    y = NULL,
    fill = "DM class"
  ) +
  ggplot2::theme_minimal()
```

## Calculate drought values at sample locations

``` r

sample_locs <- data.frame(
  site_id = c("site_1", "site_2", "site_3"),
  lon = c(-115.0, -97.5, -96.0),
  lat = c(45.0, 36.0, 39.0)
)

calc_spei <- calculate_drought(
  from = spei_processed,
  locs = sample_locs,
  locs_id = "site_id",
  radius = 1000L,
  fun = "mean"
)

calc_eddi <- calculate_drought(
  from = eddi_processed,
  locs = sample_locs,
  locs_id = "site_id",
  radius = 1000L,
  fun = "mean"
)

calc_usdm <- calculate_drought(
  from = usdm_processed,
  locs = sample_locs,
  locs_id = "site_id",
  radius = 50000
)
```

## Plot calculated drought time series with ggplot

``` r

spei_col <- grep("^spei_", names(calc_spei), value = TRUE)
eddi_col <- grep("^eddi_", names(calc_eddi), value = TRUE)

calc_spei_plot <- data.frame(
  site_id = calc_spei$site_id,
  time = as.Date(calc_spei$time),
  value = calc_spei[[spei_col]],
  source = "SPEI"
)

calc_eddi_plot <- data.frame(
  site_id = calc_eddi$site_id,
  time = as.Date(calc_eddi$time),
  value = calc_eddi[[eddi_col]],
  source = "EDDI"
)

calc_all_plot <- data.table::rbindlist(
  list(calc_spei_plot, calc_eddi_plot),
  fill = TRUE
)

ggplot2::ggplot(
  calc_all_plot,
  ggplot2::aes(x = time, y = value, color = site_id, group = site_id)
) +
  ggplot2::geom_line(linewidth = 0.5) +
  ggplot2::geom_point(size = 1.3) +
  ggplot2::facet_wrap(~ source, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  ggplot2::labs(
    title = "Calculated drought covariates at sample locations",
    subtitle = "SPEI and EDDI index values",
    x = "Date",
    y = "Calculated value",
    color = "Location"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
```

## Notes

- SPEI and EDDI are raster products; USDM is a weekly polygon product,
  so processing and extraction differ by source.
- For SPEI/EDDI, `timescale` controls the accumulation period and must
  match the downloaded files.
- USDM extraction returns drought-monitor class (`DM`) values for each
  location/date.
