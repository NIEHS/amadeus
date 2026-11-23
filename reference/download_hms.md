# Download wildfire smoke data

The `download_hms()` function accesses and downloads wildfire smoke
plume coverage data from NOAA's Hazard Mapping System Fire and Smoke
Product.

## Usage

``` r
download_hms(
  data_format = "Shapefile",
  date = c("2018-01-01", "2018-01-01"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- data_format:

  character(1). "Shapefile" or "KML".

- date:

  character(1 or 2). A single date or inclusive range endpoints in
  `"YYYY-MM-DD"` format, in either order. All dates must be between
  `"2005-08-05"` and today, inclusive. Any date outside this range
  causes the entire request to fail before directory creation or network
  requests; the range is not filtered or truncated.

- directory_to_save:

  character(1). Parent directory. Shapefile archives are saved in
  `zip_files/` and extracted into `data_files/`. KML files are saved
  directly in `data_files/`.

- acknowledgement:

  logical(1). Must be TRUE to proceed.

- download:

  logical(1). DEPRECATED. Downloads happen automatically.

- remove_command:

  logical(1). Deprecated, ignored.

- unzip:

  logical(1). Extract all planned Shapefile ZIP archives, including
  existing archives, into the shared `data_files/` directory. Default is
  `TRUE`; `FALSE` skips extraction. KML files do not require extraction.

- remove_zip:

  logical(1). Remove Shapefile archives and their `zip_files/` directory
  recursively, including remaining contents, after the extraction loop
  finishes. Default is `FALSE`; requires `unzip = TRUE`. No archive
  cleanup is performed for KML downloads.

- show_progress:

  logical(1). Show per-file messages and download progress bars during
  sequential downloads. Default is `TRUE`. Ignored during concurrent
  downloads, where per-file progress is disabled. Status and summary
  messages may still be printed when `FALSE`.

- hash:

  logical(1). Return a combined MD5 hash of all files recursively under
  `data_files/`, including existing files. Default is `FALSE`; ignored
  when `download = FALSE`. With Shapefile downloads and `unzip = FALSE`,
  hashing still uses `data_files/`, which may be empty or contain older
  data.

- max_tries:

  integer(1). Maximum retry attempts (default 20)

- rate_limit:

  numeric(1). Request-throttling interval in seconds. Default is 2.
  Passed unchanged to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
  for sequential downloads. For concurrent downloads, multiplied by the
  configured daemon count for the `"default"` compute profile before
  being passed to each worker.

## Value

With `download = FALSE`, an invisible preview list containing `urls`,
`destfiles`, and `n_files`. With `download = TRUE` and `hash = FALSE`,
an invisible summary from
[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md)
containing `success`, `failed`, `skipped`, `failed_urls`, and
`failed_files`. The two failure vectors are `character(0)` when no
downloads failed, including when every file was skipped. With
`download = TRUE` and `hash = TRUE`, a visible character scalar
containing the combined MD5 hash of `data_files/`.

## Note

HMS data does not require authentication.

## Concurrent downloads

File downloads use configured daemons in the `"default"` compute
profile, or run sequentially when that profile has no configured
daemons. Daemon creation and shutdown are managed by the caller using
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html).
For concurrent downloads, configure a fixed local pool with
`mirai::daemons(n, .compute = "default")` before calling this function.
Daemon detection, counting, and task dispatch always use the `"default"`
compute profile, regardless of the active profile. Daemon counting and
rate scaling assume that the pool still contains the `n` workers
configured by that call, with no workers added separately or pool
resizing during downloads. Each worker receives one URL/destination pair
per task; the remaining request arguments are constant across tasks.

Source-specific validation, URL discovery, archive extraction, and
directory hashing run in the calling process. Only file downloads are
dispatched to workers.

## References

(????). “Hazard Mapping System Fire and Smoke Product: Hazard Mapping
System.” <https://www.ospo.noaa.gov/products/land/hms.html#about>.
<https://www.ospo.noaa.gov/products/land/hms.html#about>.

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_hms(
  data_format = "Shapefile",
  date = "2024-01-01",
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
