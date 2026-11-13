# Download NOAA GOES ADP data

Discover and download NOAA GOES-16 or GOES-18 Aerosol Detection Product
(ADP) files from the NOAA Open Data Dissemination (NODD) AWS S3 archive.
Files are in NetCDF format and contain aerosol detection variables (e.g.
`"Smoke"`, `"Dust"`) on the GOES fixed geostationary grid.

## Usage

``` r
download_goes(
  date = c("2024-01-01", "2024-01-01"),
  satellite = c("16", "18"),
  product = c("ADP-C", "ADP-F", "ADP-M"),
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

  character(1 or 2). Single date or inclusive range endpoints in
  `"YYYY-MM-DD"` format. Endpoints may be supplied in either order.

- satellite:

  character(1). GOES satellite number: `"16"` (default) or `"18"`.
  Selects one satellite per call.

- product:

  character(1). ADP scan sector: `"ADP-C"` (CONUS, default), `"ADP-F"`
  (Full Disk), or `"ADP-M"` (Mesoscale). Selects one product per call.

- directory_to_save:

  character(1). Non-empty output directory, created if needed. Files
  retain their S3 directory structure below this root; see Details.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed.

- download:

  logical(1). Deprecated. `FALSE` warns and returns a preview after
  directory creation and discovery, without downloading data files.
  Discovery still queries S3 listings in preview mode. Default is
  `TRUE`.

- remove_command:

  logical(1). Deprecated and ignored; `TRUE` warns. Default is `FALSE`.

- show_progress:

  logical(1). Show per-file download progress. Default is `TRUE`;
  discovery and summary messages may still be printed when `FALSE`.

- hash:

  logical(1). Return the combined directory MD5 hash when `TRUE`.
  Default is `FALSE`. Ignored when `download = FALSE`.

- max_tries:

  integer(1). Maximum attempts per listing or file request, including
  the initial attempt. Default is 20.

- rate_limit:

  numeric(1). Non-negative request-throttling interval in seconds,
  passed to discovery and downloads. Default is 2. Discovery uses a
  capacity-one bucket; 0 disables discovery throttling. File downloads
  follow the throttling policy in
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).

## Value

With `download = FALSE`, an invisible preview list containing `urls`,
`destfiles`, and `n_files`. Otherwise, with `hash = FALSE`, the
invisible summary from
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md),
including `success`, `failed`, and `skipped` counts. With `hash = TRUE`,
a visible combined MD5 hash for all files under `directory_to_save`,
including previously downloaded files.

The download summary always contains `success`, `failed`, `skipped`,
`failed_urls`, and `failed_files`. The two failure vectors are
`character(0)` when no files failed, including when every file was
skipped.

## Details

`satellite` and `product` are matched with
[`base::match.arg()`](https://rdrr.io/r/base/match.arg.html), allowing
case-sensitive, unambiguous partial matches. Omitted arguments select
the first listed choice. Products `"ADP-C"`, `"ADP-F"`, and `"ADP-M"`
map to S3 prefixes `"ABI-L2-ADPC"`, `"ABI-L2-ADPF"`, and
`"ABI-L2-ADPM"`.

Daily listings supply the actual file names. Files are saved under
`directory_to_save/<product-prefix>/<year>/<day-of-year>/<hour>/<filename>`.
The day of year uses three digits, such as `"001"`. Existing non-empty
destination files are skipped by
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md);
preview results include them because discovery does not filter local
files.

Discovery requests one S3 listing page of up to 1000 keys per day and
keeps keys ending in `.nc`. Continuation tokens are not followed, so
results can be incomplete when a daily listing spans multiple pages.

A successfully retrieved listing with no matching files emits a message
and contributes no rows. Errors while requesting, parsing, or processing
any listing stop discovery before downloading. Files are not required
for every requested date. If all listings are empty, `download = TRUE`
errors with `"No URLs provided for download."`, whereas
`download = FALSE` returns an empty preview.

Discovery uses a 60-second timeout per request attempt. File downloads
use
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)'s
default timeout of 3600 seconds.

## Note

- GOES data does not require authentication.

- ADP-C (CONUS) scans are produced approximately every 5 minutes. A
  single day may contain several hundred files.

- GOES ADP files use the GOES fixed geostationary projection. Use
  [`process_goes()`](https://niehs.github.io/amadeus/reference/process_goes.md)
  to load and reproject to EPSG:4326.

## See also

[`download_data()`](https://niehs.github.io/amadeus/reference/download_data.md),
[`process_goes()`](https://niehs.github.io/amadeus/reference/process_goes.md),
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md),
[`download_preview()`](https://niehs.github.io/amadeus/reference/download_preview.md)

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
