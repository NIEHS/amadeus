# Download IMPROVE aerosol monitoring data

Download annual IMPROVE (Interagency Monitoring of Protected Visual
Environments) measurement files from the VIEWS/VIBE data export service
hosted at CIRA/CSU. Files are downloaded as `.txt.zip` archives and can
be extracted to pipe-delimited `.txt` files using the shared download
workflow.

## Usage

``` r
download_improve(
  year = c(2018, 2022),
  product = c("raw", "rhr2", "rhr3"),
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

- year:

  integer(1 or more). One year, two endpoints, or a non-empty vector of
  years. Every year from the minimum through the maximum is requested,
  including gaps: `c(2018, 2023)` requests `2018:2023`. Default is
  `c(2018, 2022)`. Missing and fractional years are rejected. File
  availability depends on the selected product and provider; local
  validation does not check product coverage.

- product:

  character(1). Product selector: `"raw"` (default) for aerosol
  measurements (`IMPAER`), `"rhr2"` for Regional Haze Rule II data
  (`IMPRHR2`), or `"rhr3"` for Regional Haze Rule III data (`IMPRHR3`).

- directory_to_save:

  character(1). Non-empty parent directory. Archives are downloaded into
  `zip_files/` and, when `unzip = TRUE`, extracted into `data_files/`.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed. Downloaded archives and
  extracted measurement files can require substantial storage.

- download:

  logical(1). Deprecated. `FALSE` emits a warning and returns a preview
  of the planned files without downloading archives or extracting their
  contents. Default is `TRUE`.

- remove_command:

  logical(1). Deprecated and ignored; `TRUE` warns. No command file is
  generated. Default is `FALSE`.

- unzip:

  logical(1). Extract downloaded ZIP archives into `data_files/`.
  Default is `TRUE`; `FALSE` retains the archives without extracting
  their contents.

- remove_zip:

  logical(1). Remove downloaded archives and their `zip_files/`
  directory, including remaining contents, after the extraction loop
  finishes. Default is `FALSE`. May be `TRUE` only when `unzip = TRUE`.

- show_progress:

  logical(1). Show file download progress. Default is `TRUE`. Other
  status messages may still be printed when `FALSE`.

- hash:

  logical(1). Return a combined MD5 hash of the files under
  `data_files/` instead of the download summary when `download = TRUE`.
  Ignored when `download = FALSE`. Default is `FALSE`.

- max_tries:

  integer(1). Maximum attempts per file download request, including the
  initial attempt. Default is 20.

- rate_limit:

  numeric(1). Non-negative throttling interval in seconds, passed to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
  Default is 2.

## Value

When `download = FALSE`, an invisible list containing `urls`,
`destfiles`, and `n_files` is returned regardless of `hash`. These
describe all planned downloads, including files that already exist
locally; `n_files` is the number of planned URLs.

When `download = TRUE`, after downloading and optional extraction,
`hash = FALSE` returns the download summary from
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
invisibly, including counts of successful, failed, and skipped files.
With `hash = TRUE`, returns a visible character scalar containing the
combined MD5 hash of `data_files/`.

The download summary always contains `success`, `failed`, `skipped`,
`failed_urls`, and `failed_files`. The two failure vectors are
`character(0)` when no files failed, including when every file was
skipped.

## Details

The selected product determines the filename prefix. For example,
`product = "raw"` and `year = 2022` request `IMPAER_2022.txt.zip`.
`zip_files/` and `data_files/` are sibling directories created beneath
`directory_to_save`, including when `unzip = FALSE` or
`download = FALSE`.

Preview mode creates the output directories and makes no network
requests. Individual download failures are recorded by
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
during file transfers.

All archives share `data_files/` as their extraction destination. Names
and paths stored inside each archive are preserved. Pass the directory
containing the extracted `PREFIX_YYYY.txt` files, normally
`data_files/`, as `path` to
[`process_improve()`](https://niehs.github.io/amadeus/reference/process_improve.md).

[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
skips existing non-empty archives in `zip_files/`. Those archives are
still passed to extraction when `unzip = TRUE`. Existing extracted
`.txt` files alone do not cause a download to be skipped.

Hashing covers all files recursively under `data_files/`, including
existing files, and excludes archives in `zip_files/`. With
`unzip = FALSE`, hashing still uses `data_files/`, which may be empty or
contain previously extracted data.

## Note

IMPROVE data does not require authentication. Annual downloads contain
measurement files; site metadata is resolved separately by
[`process_improve()`](https://niehs.github.io/amadeus/reference/process_improve.md).

## See also

[`download_setup_dir()`](https://niehs.github.io/amadeus/reference/download_setup_dir.md),
[`download_unzip()`](https://niehs.github.io/amadeus/reference/download_unzip.md),
[`process_improve()`](https://niehs.github.io/amadeus/reference/process_improve.md)

## Author

Insang Song, Mitchell Manware

## Examples

``` r
if (FALSE) { # \dontrun{
download_improve(
  year = 2022,
  product = "raw",
  directory_to_save = file.path(tempdir(), "improve"),
  acknowledgement = TRUE
)
} # }
```
