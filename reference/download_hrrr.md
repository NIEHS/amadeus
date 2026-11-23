# Download NOAA High-Resolution Rapid Refresh (HRRR) data

Downloads NOAA High-Resolution Rapid Refresh (HRRR) model data from the
NOAA HRRR public Amazon S3 archive.

The function supports the HRRR three-dimensional pressure-level,
native-level, two-dimensional surface, and sub-hourly products. Select
one product per call, with multiple dates, model cycle run times, and
forecast hours if needed.

HRRR data are downloaded as GRIB2 files and organized under a
sector-specific directory within `directory_to_save`.

## Usage

``` r
download_hrrr(
  product = NULL,
  sector = "conus",
  date = c("2018-01-01", "2018-01-01"),
  cycle_runtime = 0:23,
  forecast_hour = 0:18,
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

- product:

  character(1). Required HRRR product to download. Accepted values are
  `"3d pressure"`, `"native"`, `"2d surface"`, `"sub-hourly"`, or
  `"subhourly"`. Matching is case-insensitive. The default `NULL` must
  be replaced with a product name.

- sector:

  character(1). HRRR geographic sector to download. Currently only
  `"conus"` is supported and is the default. Matching is
  case-insensitive.

- date:

  character(1 or 2). Dates in `"YYYY-MM-DD"` format defining the
  download period. A single date downloads data for that date. When two
  dates are supplied, all dates from the earlier date through the later
  date are included. All dates must be between `"2014-07-30"` and today,
  inclusive. Dates outside this range raise an error before directory
  creation or network requests.

- cycle_runtime:

  numeric vector. Non-missing model initialization hour(s), specified as
  whole numbers from `0` through `23`. Defaults to `0:23`. Values are
  formatted as two-digit UTC cycle times when constructing the HRRR file
  names. At least one hour is required; repeated hours are requested
  once.

- forecast_hour:

  numeric vector. Non-missing forecast lead hour(s), specified as whole
  numbers from `0` through `48`. Defaults to `0:18`, including the
  zero-hour lead (`f00`) at the model initialization time. Values are
  formatted as two-digit forecast hours when constructing the HRRR file
  names. At least one hour is required; repeated hours are requested
  once.

- directory_to_save:

  character(1). Required non-empty directory path for downloaded HRRR
  files. Missing parent directories and the sector-specific subdirectory
  are created automatically.

- acknowledgement:

  logical(1). Must be `TRUE` to acknowledge the download before data are
  retrieved.

- download:

  logical(1). Deprecated. `FALSE` emits a warning and returns a download
  preview. Argument validation, directory creation, and URL construction
  still run before returning.

- remove_command:

  logical(1). Deprecated and ignored. `TRUE` emits a warning.

- show_progress:

  logical(1). Show per-file messages and progress bars during sequential
  downloads. Defaults to `TRUE`. Ignored during concurrent downloads,
  where per-file progress is disabled. Status and summary messages may
  still be printed when `FALSE`.

- hash:

  logical(1). Return a combined MD5 hash of all files recursively under
  `directory_to_save`, including existing files, instead of the download
  summary. Defaults to `FALSE`.

- max_tries:

  integer(1). Maximum attempts per file request, including the initial
  attempt. Must be at least 1. Defaults to 20.

- rate_limit:

  numeric(1). Finite, non-negative request-throttling interval in
  seconds. Defaults to 2. Passed unchanged to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
  for sequential downloads. For concurrent downloads, multiplied by the
  configured daemon count for the `"default"` compute profile before
  being passed to each worker.

## Value

If `hash = FALSE`, an invisible summary from
[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md)
containing `success`, `failed`, `skipped`, `failed_urls`, and
`failed_files`. The two failure vectors are `character(0)` when no
downloads failed, including when every file was skipped. If
`hash = TRUE`, a visible character scalar containing the combined MD5
hash of all files recursively under `directory_to_save`, including
existing files.

## Details

Dates select model initialization days. Each selected cycle run time and
forecast hour is requested for every date. URLs are constructed
directly; validation does not check whether each requested product,
cycle, and forecast hour is available in the archive.

Product names map to the file codes `prs` (3d pressure), `nat` (native),
`sfc` (2d surface), and `subh` (either sub-hourly spelling). Local
filenames include the initialization date to distinguish files from
different days. For example, surface data initialized at 12 UTC on
January 1, 2020, with a one-hour forecast lead are saved as
`conus/hrrr.20200101.t12z.wrfsfcf01.grib2` under `directory_to_save`.

Downloads use
[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md),
which skips existing non-empty destination files. Handled download
failures are recorded in the returned summary and reported with a
warning. See that helper for concurrent download setup and worker
requirements.

## Note

No authentication token is required for this public archive.

## Concurrent downloads

File downloads use caller-configured
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html) in
the `"default"` compute profile, or run sequentially when that profile
has no daemons. Configure a fixed pool with
`mirai::daemons(n, .compute = "default")` before calling this function.
The caller manages pool creation and shutdown; keep the pool size
unchanged during downloads.

Workers need compatible `amadeus` code and its dependencies, plus access
to the same destination storage as the calling process. Loading
development code in the calling process does not load it on workers; use
[`mirai::everywhere()`](https://mirai.r-lib.org/reference/everywhere.html)
to load the same code on workers when needed. Validation, URL
construction, directory setup, and hashing run in the calling process.
Only file downloads are dispatched to workers.

## See also

[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md),
[`sequence_dates()`](https://niehs.github.io/amadeus/reference/sequence_dates.md)

## Examples

``` r
if (FALSE) { # \dontrun{
download_hrrr(
  product = "2d surface",
  sector = "conus",
  date = "2020-01-01",
  cycle_runtime = 12,
  forecast_hour = 1:18,
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)

download_hrrr(
  product = "sub-hourly",
  date = c("2020-01-01", "2020-01-03"),
  cycle_runtime = 0:23,
  forecast_hour = 1:6,
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
