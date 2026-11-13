# Download toxic release data

The `download_tri()` function accesses and downloads toxic release data
from the U.S. Environmental Protection Agency's (EPA) Toxic Release
Inventory (TRI) Program. The EPA TRI basic data files contain annual,
facility-reported toxic chemical release and waste management
information. EPA publishes TRI basic files in multiple annual variants
under the same service endpoint: a nationwide file (`"US"`),
state-specific files identified by two-letter postal abbreviations (for
example `"AZ"` or `"NC"`), and a tribal file (`"tbl"`).

## Usage

``` r
download_tri(
  year = c(2018L, 2022L),
  jurisdiction = "US",
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

- year:

  integer(1 or more). One year, two endpoints, or a non-empty vector of
  years. Every year from the minimum through the maximum is requested,
  including gaps: `c(2018, 2023)` requests `2018:2023`. Default is
  `c(2018L, 2022L)`. Missing and fractional years are rejected. File
  availability depends on EPA coverage for the selected jurisdiction.

- jurisdiction:

  character(1). TRI file variant to download. Use `"US"` (default) for
  the nationwide file, a two-letter state or territory code such as
  `"AZ"` or `"NC"`, or `"tbl"` for the tribal file. Leading and trailing
  whitespace is removed and case is normalized, so `" nc "` and `"TBL"`
  are accepted. Validation accepts any two-letter alphabetic code or
  `"tbl"`; it does not check a list of supported jurisdiction codes.

- directory_to_save:

  character(1). Non-empty output directory path. CSV files are saved
  directly in this directory, which is created if missing, including
  when `download = FALSE`.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed.

- download:

  logical(1). Deprecated. `FALSE` emits a warning and returns a preview
  of the planned files without downloading CSV files. Default is `TRUE`.

- remove_command:

  logical(1). Deprecated and ignored; `TRUE` emits a warning. Default is
  `FALSE`.

- show_progress:

  logical(1). Show file download progress. Default is `TRUE`. Other
  status messages may still be printed when `FALSE`.

- hash:

  logical(1). Return a combined MD5 hash of the files under
  `directory_to_save` instead of the download summary when
  `download = TRUE`. Ignored when `download = FALSE`. Default is
  `FALSE`.

- max_tries:

  integer(1). Maximum attempts per file download request, including the
  initial attempt. Default is 20.

- rate_limit:

  numeric(1). Request-throttling interval in seconds, passed to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
  Controls the average request rate, rather than guaranteeing a minimum
  gap between requests. Default is 2.

## Value

When `download = FALSE`, an invisible list containing `urls`,
`destfiles`, and `n_files` is returned regardless of `hash`. The file
count is the number of planned URLs, including files that already exist
locally.

When `download = TRUE` and `hash = FALSE`, returns the download summary
from
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
invisibly, including `success`, `failed`, and `skipped` file counts. The
summary always contains `failed_urls` and `failed_files` character
vectors, both `character(0)` when no downloads failed, including when
every file was skipped.

When `download = TRUE` and `hash = TRUE`, returns a visible character
scalar containing the combined MD5 hash of the files under
`directory_to_save`.

## Details

File names depend on the selected jurisdiction. For example, requests
for 2020 produce `tri_raw_2020.csv` for `"US"`, `tri_raw_2020_NC.csv`
for `"NC"`, and `tri_raw_2020_TBL.csv` for `"tbl"`. Tribal download URLs
use lowercase `tbl`, while local file names use uppercase `TBL`.

The output directory is created before returning a preview or
downloading. Preview mode makes no network requests. Individual download
failures are recorded by
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
during file transfers.

Previews include all planned files, including those already present
locally. During downloading,
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
skips existing non-empty files; missing or empty destination files are
downloaded.

Hashing includes all files recursively under `directory_to_save`,
including existing files and files unrelated to the requested years or
jurisdiction.

## Note

TRI data does not require authentication. State and tribal downloads are
saved with jurisdiction-specific file names, while the U.S.-wide
download keeps the historical `tri_raw_<year>.csv` naming pattern.

## References

United States Environmental Protection Agency (2024). “TRI Basic Data
Files: Calendar Years 1987 – Present.”
<https://www.epa.gov/toxics-release-inventory-tri-program/tri-data-action-0>.

## See also

[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md),
[`process_tri()`](https://niehs.github.io/amadeus/reference/process_tri.md)

## Author

Mariana Kassien, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_tri(
  year = 2021L,
  directory_to_save = tempdir(),
  jurisdiction = "NC",
  acknowledgement = TRUE
)
} # }
```
