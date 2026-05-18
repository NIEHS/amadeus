# Downloading EPA Daily Data

This vignette demonstrates how to download and pre-process pre-generated
EPA AQS data using `amadeus` functions. It downloads daily PM2.5 data
for the period 2018-2022, joins multiple years into a single data frame,
and downloads monitor metadata.

Available datasets can be found at
<https://aqs.epa.gov/aqsweb/airdata/download_files.html>.

### Download

Start by downloading the AQS data files with `download_data`.

- `dataset_name = "aqs"`: AQS dataset name.
- `parameter_code = 88101`: Parameter code for PM2.5 local conditions.
- `resolution_temporal = "daily"`: daily temporal resolution.
- `year = c(2018, 2022)`: years of interest.
- `directory_to_save = dir`: directory to save the downloaded files.
- `acknowledgement = TRUE`: acknowledge that data files may consume
  local storage.
- `unzip = TRUE`: unzip downloaded zip files (default).
- `hash = TRUE`: generate unique SHA-1 hash for the downloaded files.

``` r

dir <- tempdir()
amadeus::download_data(
  dataset_name = "aqs",
  parameter_code = 88101,
  resolution_temporal = "daily",
  year = c(2018, 2022),
  directory_to_save = dir,
  acknowledgement = TRUE,
  unzip = TRUE,
  hash = TRUE
)
```

    [1] "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2"

### Processing data

Unzip and read in .csv files, process and join into one data frame. The
unique site identifier `ID.Code` has the structure
`State-County-Site-Parameter-POC`.

``` r

parameter_code <- 88101
startyear <- 2018
endyear <- 2022

csv_names <- sprintf(
  file.path(dir, "daily_%d_%d.csv"),
  parameter_code,
  startyear:endyear
)

for (n in seq_along(csv_names)) {
  print(paste("reading and processing file:", csv_names[n], "..."))
  data <- read.csv(csv_names[n], stringsAsFactors = FALSE)

  # Make unique site identifier: State-County-Site-Parameter-POC
  data$ID.Code <- paste(
    data$State.Code, data$County.Code,
    data$Site.Num, data$Parameter.Code,
    data$POC,
    sep = "-"
  )

  if (n == 1) {
    data_all <- data
  } else {
    data_all <- rbind(data_all, data)
  }
}
```

    [1] "reading and processing file: /tmp/RtmpFZiOmK/daily_88101_2018.csv ..."
    [1] "reading and processing file: /tmp/RtmpFZiOmK/daily_88101_2019.csv ..."
    [1] "reading and processing file: /tmp/RtmpFZiOmK/daily_88101_2020.csv ..."
    [1] "reading and processing file: /tmp/RtmpFZiOmK/daily_88101_2021.csv ..."
    [1] "reading and processing file: /tmp/RtmpFZiOmK/daily_88101_2022.csv ..."

### Downloading monitor metadata

Download and filter the AQS monitors metadata file.

``` r

monitors_zip <- file.path(dir, "aqs_monitors.zip")
download.file(
  "https://aqs.epa.gov/aqsweb/airdata/aqs_monitors.zip",
  monitors_zip
)
unzip(monitors_zip, exdir = dir)
monitors <- read.csv(
  file.path(dir, "aqs_monitors.csv"),
  stringsAsFactors = FALSE
)

# Convert State.Code to numeric to remove leading zeros
# (NAs introduced from Canadian monitors with site number "CC")
monitors$State.Code <- as.numeric(monitors$State.Code)
monitors$ID.Code <- paste(
  monitors$State.Code, monitors$County.Code,
  monitors$Site.Num, monitors$Parameter.Code,
  monitors$POC,
  sep = "-"
)

monitors_filter <- monitors[
  which(monitors$ID.Code %in% data_all$ID.Code),
]
```

### Saving results

``` r

write.csv(
  data_all,
  file.path(dir, sprintf("daily_%d_2018-2022.csv", parameter_code))
)
write.csv(
  monitors_filter,
  file.path(dir, sprintf("monitors_%d_2018-2022.csv", parameter_code))
)
```
