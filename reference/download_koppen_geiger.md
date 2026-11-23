# Download climate classification data

The `download_koppen_geiger()` function accesses and downloads climate
classification data.

## Usage

``` r
download_koppen_geiger(
  data_resolution = c("0.0083", "0.083", "0.5"),
  time_period = c("Present", "Future"),
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

- data_resolution:

  character(1). One of `"0.0083"` (default), `"0.083"`, or `"0.5"`.

- time_period:

  character(1). `"Present"` (default; 1980-2016) or `"Future"`
  (2071-2100).

- directory_to_save:

  character(1). Directory to save data.

- acknowledgement:

  logical(1). Must be TRUE to proceed.

- download:

  logical(1). DEPRECATED. Downloads happen automatically. Set to FALSE
  to skip downloading and return the planned file list.

- remove_command:

  logical(1). Deprecated, ignored.

- unzip:

  logical(1). Unzip zip files (default TRUE).

- remove_zip:

  logical(1). Remove zip files after unzipping (default FALSE).

- show_progress:

  logical(1). Show per-file messages and download progress bars during
  sequential downloads. Default is `TRUE`. Ignored during concurrent
  downloads, where per-file progress is disabled. Status and summary
  messages may still be printed when `FALSE`.

- hash:

  logical(1). Return hash of downloaded files (default FALSE)

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

When `download = TRUE` and `hash = FALSE`, an invisible list containing
`success`, `failed`, and `skipped` file counts. If the requested archive
already exists and is nonempty, no file is downloaded; the returned list
contains `success = 0`, `failed = 0`, and `skipped = 1`. The list always
contains `failed_urls` and `failed_files` character vectors, both
`character(0)` when no downloads failed, including when the file was
skipped.

When `download = FALSE`, an invisible list containing `urls`,
`destfiles`, and `n_files = 1` is returned without downloading,
regardless of `hash`.

When `download = TRUE` and `hash = TRUE`, a character(1) combined MD5
hash of the files in the `data_files` directory is returned instead of a
list.

## Details

`data_resolution` and `time_period` each select one value. Matching is
case-sensitive, and unambiguous abbreviations are accepted. If omitted,
each argument uses its first listed choice.

## Note

Köppen-Geiger data does not require authentication.

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

Beck HE, McVicar TR, Vergopolan N, Berg A, Lutsko NJ, Dufour A, Zeng Z,
Jiang X, Van Dijk AIJM, Miralles DG (2023). “High-resolution (1 km)
Köppen-Geiger maps for 1901–2099 based on constrained CMIP6
projections.” *Scientific Data*, **10**(1), 724. ISSN 2052-4463.
[doi:10.1038/s41597-023-02549-6](https://doi.org/10.1038/s41597-023-02549-6)
. <https://www.nature.com/articles/s41597-023-02549-6>. Beck HE,
Zimmermann NE, McVicar TR, Vergopolan N, Berg A, Wood EF (2018).
“Present and future Köppen-Geiger climate classification maps at 1-km
resolution.” *Scientific data*, **5**(1), 1–12.
[doi:10.1038/sdata.2018.214](https://doi.org/10.1038/sdata.2018.214) .

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_koppen_geiger(
  data_resolution = "0.0083",
  time_period = "Present",
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
