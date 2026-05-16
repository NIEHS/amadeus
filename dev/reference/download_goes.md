# Download NOAA GOES ADP data

The `download_goes()` function accesses and downloads NOAA GOES-16 or
GOES-18 Aerosol Detection Product (ADP) files from the NOAA Open Data
Dissemination (NODD) AWS S3 bucket. Files are in NetCDF format and
contain aerosol detection variables (e.g. `"Smoke"`, `"Dust"`) on the
GOES fixed geostationary grid.

## Usage

``` r
download_goes(
  date = c("2024-01-01", "2024-01-01"),
  satellite = "16",
  product = "ADP-C",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- date:

  character(1 or 2). Date (YYYY-MM-DD) or start and end dates.

- satellite:

  character(1). GOES satellite number: `"16"` (East, default) or `"18"`
  (West).

- product:

  character(1). ADP scan sector: `"ADP-C"` (CONUS, default), `"ADP-F"`
  (Full Disk), or `"ADP-M"` (Mesoscale).

- directory_to_save:

  character(1). Directory to save downloaded files.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed.

- download:

  logical(1). DEPRECATED. Downloads happen automatically.

- remove_command:

  logical(1). Deprecated, ignored.

- show_progress:

  logical(1). Show download progress (default `TRUE`).

- hash:

  logical(1). Return hash of downloaded files (default `FALSE`).

- max_tries:

  integer(1). Maximum retry attempts (default 20).

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2).

## Value

invisible list with download results; or hash character if `hash = TRUE`

## Note

- GOES data does not require authentication.

- GOES-16 (East) covers the Americas; GOES-18 (West) covers the western
  hemisphere and Pacific.

- ADP-C (CONUS) scans are produced approximately every 5 minutes. A
  single day may contain several hundred files.

- GOES ADP files use the GOES fixed geostationary projection. Use
  [`process_goes()`](https://niehs.github.io/amadeus/dev/reference/process_goes.md)
  to load and reproject to EPSG:4326.

## Author

Mitchell Manware

## Examples

``` r
if (FALSE) { # \dontrun{
download_goes(
  date = "2024-01-01",
  satellite = "16",
  product = "ADP-C",
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
