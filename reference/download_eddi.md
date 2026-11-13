# Download EDDI drought data

Construct EDDI ASCII-grid URLs for Tuesdays within a date interval and
download the selected accumulation timescale using the shared workflow.

## Usage

``` r
download_eddi(
  date,
  timescale,
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

- date:

  character(1 or 2). Required date or inclusive range endpoints in
  `"YYYY-MM-DD"` format, in either order. A single Tuesday selects one
  file; an interval containing no Tuesdays raises an error before
  directory creation.

- timescale:

  integer(1). Required positive accumulation timescale in months.
  Validation does not establish provider availability. The value is
  padded to at least two digits in filenames, such as `01mn` for `1L`.

- directory_to_save:

  character(1). Non-empty directory for downloaded `.asc` files. Files
  are saved directly here without archive or variable-specific
  subdirectories.

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

  logical(1). Show file download progress. Default is `TRUE`. Other
  status and summary messages may still be printed when `FALSE`.

- max_tries:

  integer(1). Maximum attempts per file request, including the initial
  attempt. Must be at least 1. Default is `3L`.

- rate_limit:

  numeric(1). Finite, non-negative request-throttling interval in
  seconds, passed to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
  Default is 2.

- ...:

  Reserved for future use; currently ignored.

## Value

With `download = FALSE`, an invisible list containing `urls`,
`destfiles`, and `n_files`. Otherwise, an invisible download summary
from
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
when `hash = FALSE`, or a visible character scalar containing the
output-directory MD5 hash when `hash = TRUE`.

The download summary always contains `success`, `failed`, `skipped`,
`failed_urls`, and `failed_files`. The two failure vectors are
`character(0)` when no files failed, including when every file was
skipped.

## Details

URLs use the FTP `Projects/EDDI/CONUS_archive/data` endpoint with one
year directory per file. Local names follow
`EDDI_ETrs_NNmn_YYYYMMDD.asc`. All requested Tuesdays are included in
the plan; remote availability is not discovered from directory listings.

A preview creates the output directory and returns all planned URLs and
destinations, including existing files, without contacting the provider.
Actual downloads use
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md),
which skips existing non-empty files and records individual transfer
failures.

## See also

[`download_drought()`](https://niehs.github.io/amadeus/reference/download_drought.md),
[`process_drought()`](https://niehs.github.io/amadeus/reference/process_drought.md),
[`calculate_drought()`](https://niehs.github.io/amadeus/reference/calculate_drought.md)

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_eddi(
  date = c("2020-01-07", "2020-01-21"),
  timescale = 1L,
  directory_to_save = file.path(tempdir(), "eddi"),
  acknowledgement = TRUE
)
} # }
```
