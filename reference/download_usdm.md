# Download USDM drought data

Download U.S. Drought Monitor ZIP archives for Tuesdays within a date
interval, with optional extraction of drought-class shapefiles.

## Usage

``` r
download_usdm(
  date,
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 3L,
  rate_limit = 2,
  unzip = TRUE,
  remove_zip = FALSE,
  ...
)
```

## Arguments

- date:

  character(1 or 2). Required date or inclusive range endpoints in
  `"YYYY-MM-DD"` format, in either order. A single Tuesday selects one
  archive; an interval containing no Tuesdays raises an error before
  directory creation.

- directory_to_save:

  character(1). Non-empty parent directory. Archives are saved in
  `zip_files/` and extracted into `data_files/`.

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

  logical(1). Return a combined MD5 hash of all files recursively under
  `data_files/`, including existing files, instead of the download
  summary. Default is `FALSE`; ignored when `download = FALSE`. With
  `unzip = FALSE`, hashing still uses `data_files/`, not the archives.

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

- unzip:

  logical(1). Extract downloaded or existing archives into
  `data_files/`. Default is `TRUE`; `FALSE` skips extraction.

- remove_zip:

  logical(1). Remove archives and their parent `zip_files/` directory
  recursively after the extraction loop finishes, including any
  remaining contents. Requires `unzip = TRUE`. Default is `FALSE`. No
  cleanup occurs during previews.

- ...:

  Reserved for future use; currently ignored.

## Value

With `download = FALSE`, an invisible list containing `urls`, archive
`destfiles`, and `n_files`. Otherwise, after optional extraction and
cleanup, an invisible summary from
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
when `hash = FALSE`, or a visible character scalar containing the
combined MD5 hash of `data_files/` when `hash = TRUE`.

The download summary always contains `success`, `failed`, `skipped`,
`failed_urls`, and `failed_files`. The two failure vectors are
`character(0)` when no files failed, including when every file was
skipped.

## Details

Each selected Tuesday maps to a `USDM_YYYYMMDD_M.zip` archive at the
provider's `data/shapefiles_m` endpoint. The URL plan does not query
directory listings or establish availability for every selected date.

Both `zip_files/` and `data_files/` are created before a preview
returns, including when extraction is disabled. Preview results include
existing archive paths and require no network requests.

[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
skips existing non-empty archives and records individual download
failures. All planned archives are subsequently passed to
[`download_unzip()`](https://niehs.github.io/amadeus/reference/download_unzip.md),
including existing archives, with the selected `unzip` flag. Paths
inside archives are preserved. Extraction errors can prevent a normal
return; download summary counts do not report extraction success. Pass
`data_files/` to
[`process_drought()`](https://niehs.github.io/amadeus/reference/process_drought.md)
for USDM processing.

## See also

[`download_drought()`](https://niehs.github.io/amadeus/reference/download_drought.md),
[`process_drought()`](https://niehs.github.io/amadeus/reference/process_drought.md),
[`calculate_drought()`](https://niehs.github.io/amadeus/reference/calculate_drought.md)

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_usdm(
  date = c("2020-01-07", "2020-01-21"),
  directory_to_save = file.path(tempdir(), "usdm"),
  acknowledgement = TRUE
)
} # }
```
