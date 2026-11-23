# Download CropScape data

Download annual USDA Cropland Data Layer archives from the [USDA
National Agricultural Statistics
Service](https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php)
or [George Mason University](https://nassgeodata.gmu.edu/CropScape/).
Archives and extracted raster data are stored in separate subdirectories
using the shared download workflow.

## Usage

``` r
download_cropscape(
  year = seq(1997, 2023),
  source = c("GMU", "USDA"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- year:

  integer(1 or more). One year, two endpoints, or a non-empty vector of
  years. Every year from the minimum through the maximum is requested,
  including gaps: `c(2018, 2023)` requests `2018:2023`. Default is
  `1997:2023`. Valid years run from 1997 for GMU or 2008 for USDA
  through the current calendar year, inclusive. Missing, fractional, or
  out-of-range years cause the entire request to raise an error before
  directory creation; years are not filtered or truncated.

- source:

  character(1). Data source, either `"GMU"` (default) or `"USDA"`. GMU
  requests use `YYYY_cdls.tar.gz` with a minimum year of 1997. USDA
  requests use `YYYY_30m_cdls.zip` with a minimum year of 2008; supply a
  compatible `year` when choosing USDA. File availability is determined
  by the selected provider.

- directory_to_save:

  character(1). Non-empty parent directory. Archives are downloaded into
  `zip_files/` and, when `unzip = TRUE`, extracted into separate
  per-archive directories under `data_files/`.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed. CropScape archives and
  extracted rasters can require substantial storage.

- download:

  logical(1). Deprecated. `FALSE` warns and returns a preview after
  directory creation, without contacting the provider. No archives are
  downloaded or extracted in preview mode. Default is `TRUE`.

- remove_command:

  logical(1). Deprecated and ignored; `TRUE` warns. No command file is
  generated. Default is `FALSE`.

- unzip:

  logical(1). Extract downloaded `.zip` or `.tar.gz` archives into
  per-archive directories under `data_files/`. Default is `TRUE`;
  `FALSE` skips extraction and creation of the per-archive directories.

- remove_zip:

  logical(1). Remove downloaded archives and their `zip_files/`
  directory, including its remaining contents, after the extraction loop
  finishes. Default is `FALSE`; requires `unzip = TRUE`. Ignored when
  `download = FALSE`.

- hash:

  logical(1). Return a combined MD5 hash of the files under
  `data_files/` instead of the download summary. Default is `FALSE`.
  Ignored when `download = FALSE`.

- show_progress:

  logical(1). Show per-file messages and download progress bars during
  sequential downloads. Default is `TRUE`. Ignored during concurrent
  downloads, where per-file progress is disabled. Status and summary
  messages may still be printed when `FALSE`.

- max_tries:

  integer(1). Maximum attempts per file download request, including the
  initial attempt. Default is 20.

- rate_limit:

  numeric(1). Request-throttling interval in seconds. Default is 2.
  Passed unchanged to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
  for sequential downloads. For concurrent downloads, multiplied by the
  configured daemon count for the `"default"` compute profile before
  being passed to each worker.

## Value

With `download = FALSE`, an invisible preview list containing `urls`,
`destfiles`, and `n_files`. Otherwise, after downloading and optional
extraction, `hash = FALSE` returns the download summary from
[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md)
invisibly. With `hash = TRUE`, returns a visible character scalar
containing the combined MD5 hash of `data_files/`.

The download summary always contains `success`, `failed`, `skipped`,
`failed_urls`, and `failed_files`. The two failure vectors are
`character(0)` when no files failed, including when every file was
skipped.

## Details

`zip_files/` and `data_files/` are sibling directories created beneath
`directory_to_save`, including for previews and when `unzip = FALSE`.
Both ZIP and compressed TAR downloads are stored in `zip_files/`. Each
archive is extracted into its own directory under `data_files/`, named
after the archive with the full `.zip` or `.tar.gz` suffix removed:
`data_files/YYYY_30m_cdls/` for USDA and `data_files/YYYY_cdls/` for
GMU. This isolates duplicate filenames across archives. Paths stored
inside each archive are preserved. Pass the corresponding per-archive
directory or an extracted TIFF path to
[`process_cropscape()`](https://niehs.github.io/amadeus/reference/process_cropscape.md).

Preview destinations refer to the archive files in `zip_files/`. Actual
downloads use
[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md),
which skips existing non-empty archives and records individual download
failures. Existing archives are still passed to extraction when
`unzip = TRUE`.

Hashing covers all files recursively under `data_files/`, including the
per-archive subdirectories and existing files, and excludes archives in
`zip_files/`. With `unzip = FALSE`, hashing still uses `data_files/`,
which may be empty or contain previously extracted data.

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

[`download_setup_dir()`](https://niehs.github.io/amadeus/reference/download_setup_dir.md),
[`download_unzip()`](https://niehs.github.io/amadeus/reference/download_unzip.md),
[`process_cropscape()`](https://niehs.github.io/amadeus/reference/process_cropscape.md)

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_cropscape(
  year = 2020,
  source = "GMU",
  directory_to_save = file.path(tempdir(), "cropscape"),
  acknowledgement = TRUE,
  download = FALSE # Preview archive URLs and zip_files/ destinations.
)
} # }
```
