# US EPA Air Quality System (AQS)

This article demonstrates a compact AQS workflow focused on outcome
data. Because AQS measurements are typically used as dependent
variables, the example uses
[`download_aqs()`](https://niehs.github.io/amadeus/dev/reference/download_aqs.md)
and
[`process_aqs()`](https://niehs.github.io/amadeus/dev/reference/process_aqs.md)
directly rather than routing through
[`calculate_covariates()`](https://niehs.github.io/amadeus/dev/reference/calculate_covariates.md).

This vignette runs its live workflow when rendered locally. The
download, processing, and plotting chunks are skipped automatically on
CI, CRAN checks, and pkgdown builds; set `AMADEUS_RUN_VIGNETTES=true` to
force live execution in those environments.

## Available inputs and data availability

[`download_aqs()`](https://niehs.github.io/amadeus/dev/reference/download_aqs.md)
exposes the key availability choices for AQS downloads:

- `parameter_code` selects the EPA pollutant parameter. Common examples
  documented in `amadeus` include PM2.5 (`88101` and `88502`), PM10
  (`81102`), ozone (`44201`), NO2 (`42602`), SO2 (`42401`), and CO
  (`42101`).
- `resolution_temporal` currently supports only `"daily"`, so the
  downloadable files are daily monitor observations rather than hourly
  or annual summaries.
- `year` accepts a single year or a start/end pair, downloading one
  pre-generated EPA archive per year requested.
- AQS downloads arrive as zipped annual files and can be unzipped
  automatically with `unzip = TRUE`; the workflow then reads the
  extracted daily CSV files.
- AQS does not require authentication, but it is mainly intended for
  outcome modeling, so `amadeus` supports download and processing only
  and does not expose AQS through
  [`calculate_covariates()`](https://niehs.github.io/amadeus/dev/reference/calculate_covariates.md).

## Pollutant parameters used in this example

``` r
aqs_parameters
  pollutant parameter_code
1     PM2.5          88101
2       NO2          42602
```

## Download PM2.5 and NO2 daily AQS data

``` r

aqs_dir <- file.path(tempdir(), "aqs_workflow")

for (i in seq_len(nrow(aqs_parameters))) {
  download_aqs(
    parameter_code = aqs_parameters$parameter_code[i],
    resolution_temporal = "daily",
    year = 2022,
    directory_to_save = aqs_dir,
    acknowledgement = TRUE,
    unzip = TRUE,
    remove_zip = FALSE
  )
}
```

## Process PM2.5 and NO2 with `process_aqs()`

``` r

aqs_processed <- lapply(seq_len(nrow(aqs_parameters)), function(i) {
  csv_path <- file.path(
    aqs_dir,
    "data_files",
    sprintf("daily_%s_2022.csv", aqs_parameters$parameter_code[i])
  )

  locations <- process_aqs(
    path = csv_path,
    date = aqs_date_window,
    mode = "location",
    return_format = "sf"
  )
  locations$pollutant <- aqs_parameters$pollutant[i]

  daily_data <- process_aqs(
    path = csv_path,
    date = aqs_date_window,
    mode = "available-data",
    return_format = "data.table"
  )
  daily_data[, pollutant := aqs_parameters$pollutant[i]]

  list(
    locations = locations,
    daily = daily_data
  )
})
```

## Plot monitor locations with facets for PM2.5 and NO2

``` r

aqs_locations <- do.call(
  rbind,
  lapply(aqs_processed, `[[`, "locations")
)

aqs_time_series <- data.table::rbindlist(
  lapply(aqs_processed, `[[`, "daily"),
  fill = TRUE
)
```

``` r

ggplot2::ggplot() +
  ggplot2::geom_sf(data = aqs_locations, color = "#0072B2", size = 0.8, alpha = 0.8) +
  ggplot2::facet_wrap(~ pollutant, ncol = 2) +
  ggplot2::coord_sf(datum = NA) +
  ggplot2::labs(
    title = "AQS monitor locations for PM2.5 and NO2",
    subtitle = paste(aqs_date_window[1], "to", aqs_date_window[2]),
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.spacing = grid::unit(1, "lines")
  )
```

## Show an example time series of the downloaded AQS data

``` r

aqs_time_series[, time := as.Date(time)]

example_sites <- aqs_time_series[
  ,
  .N,
  by = .(pollutant, site_id)
][
  order(pollutant, -N, site_id)
][
  ,
  .SD[1],
  by = pollutant
]

aqs_time_series <- merge(
  aqs_time_series,
  example_sites[, .(pollutant, site_id)],
  by = c("pollutant", "site_id")
)
```

``` r

ggplot2::ggplot(
  aqs_time_series,
  ggplot2::aes(x = time, y = Arithmetic.Mean, group = site_id)
) +
  ggplot2::geom_line(color = "#D55E00", linewidth = 0.5) +
  ggplot2::geom_point(color = "#D55E00", size = 0.8) +
  ggplot2::facet_wrap(~ pollutant, scales = "free_y", ncol = 2) +
  ggplot2::scale_x_date(
    date_breaks = "2 weeks",
    date_labels = "%b %d"
  ) +
  ggplot2::labs(
    title = "Example AQS daily time series",
    subtitle = "One site per pollutant, chosen from monitors with available observations in this date window",
    x = "Date",
    y = "Arithmetic mean"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.spacing = grid::unit(1, "lines"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )
```

## Notes

- This workflow treats AQS as outcome data and therefore uses
  [`download_aqs()`](https://niehs.github.io/amadeus/dev/reference/download_aqs.md)
  and
  [`process_aqs()`](https://niehs.github.io/amadeus/dev/reference/process_aqs.md)
  directly.
- The two explicit pollutant parameters are PM2.5 (`88101`) and NO2
  (`42602`).
- `process_aqs(mode = "location")` is useful for monitor maps, while
  `process_aqs(mode = "available-data")` is useful for daily
  observational summaries such as time series.
