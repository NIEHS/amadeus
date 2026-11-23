# Download SPEI drought data

Download one multi-year SPEI NetCDF file for a selected accumulation
timescale and database version, using the shared download workflow.

## Usage

``` r
download_spei(
  timescale,
  version = c("2.11", "2.10", "legacy"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 3L,
  rate_limit = 2,
  ...
)
```

## Arguments

- timescale:

  integer(1). Required positive accumulation timescale in months. The
  value is padded to at least two digits in the filename; for example,
  `1L` selects `spei01.nc`. Validation does not establish whether a file
  is available for the requested timescale.

- version:

  character(1). `"2.11"` (default), `"2.10"`, or `"legacy"`. Uses
  [`base::match.arg()`](https://rdrr.io/r/base/match.arg.html), allowing
  unambiguous partial matches. `"legacy"` selects the unversioned
  `/files/` endpoint and does not identify a particular database
  release. No automatic fallback occurs.

- directory_to_save:

  character(1). Non-empty directory for downloaded files. Files retain
  their provider names directly in this directory; no version-specific
  subdirectory is created.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed, including for previews.
  Downloads and extracted files may require substantial storage. Default
  is `FALSE`.

- download:

  logical(1). Deprecated. `FALSE` warns and returns a preview after
  directory creation, without making network requests. Default is
  `TRUE`.

- remove_command:

  logical(1). Deprecated and ignored; `TRUE` warns. No command file is
  generated. Default is `FALSE`.

- hash:

  logical(1). Return a combined MD5 hash of all files under
  `directory_to_save`, including existing files, instead of the download
  summary. Default is `FALSE`; ignored when `download = FALSE`.

- show_progress:

  logical(1). Show per-file messages and download progress bars during
  sequential downloads. Default is `TRUE`. Ignored during concurrent
  downloads, where per-file progress is disabled. Status and summary
  messages may still be printed when `FALSE`.

- max_tries:

  integer(1). Maximum attempts per file request, including the initial
  attempt. Must be at least 1. Default is `3L`.

- rate_limit:

  numeric(1). Request-throttling interval in seconds. Default is 2.
  Passed unchanged to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
  for sequential downloads. For concurrent downloads, multiplied by the
  configured daemon count for the `"default"` compute profile before
  being passed to each worker.

- ...:

  Reserved for future use; currently ignored.

## Value

With `download = FALSE`, an invisible list containing `urls`,
`destfiles`, and `n_files` (one). Otherwise, an invisible download
summary from
[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md)
when `hash = FALSE`, or a visible character scalar containing the
output-directory MD5 hash when `hash = TRUE`.

The download summary always contains `success`, `failed`, `skipped`,
`failed_urls`, and `failed_files`. The two failure vectors are
`character(0)` when no files failed, including when every file was
skipped.

## Details

Version `"2.11"` uses `spei_database_2_11/nc/`, version `"2.10"` uses
`spei_database_2_10/nc/`, and `"legacy"` uses `files/` on the SPEI
server. Each call constructs one URL. Dates do not select or subset this
download; use
[`process_drought()`](https://niehs.github.io/amadeus/reference/process_drought.md)
for temporal selection after downloading.

All versions use the same local `speiNN.nc` filename. This preserves the
filename expected by drought processing, but existing-file checks do not
identify which version produced a file. Use separate output directories
when retaining multiple versions.

A preview creates the output directory and returns the URL and
destination without contacting the server. Existing files remain in the
preview. Actual downloads use
[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md),
which skips existing non-empty files and reports failures without
selecting another database version.

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

## See also

[`download_drought()`](https://niehs.github.io/amadeus/reference/download_drought.md),
[`process_drought()`](https://niehs.github.io/amadeus/reference/process_drought.md),
[`calculate_drought()`](https://niehs.github.io/amadeus/reference/calculate_drought.md)

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_spei(
  timescale = 1L,
  version = "2.11",
  directory_to_save = file.path(tempdir(), "spei"),
  acknowledgement = TRUE
)
} # }
```
