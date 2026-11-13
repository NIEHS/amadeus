# Download drought index data

Dispatch to
[`download_spei()`](https://niehs.github.io/amadeus/reference/download_spei.md),
[`download_eddi()`](https://niehs.github.io/amadeus/reference/download_eddi.md),
or
[`download_usdm()`](https://niehs.github.io/amadeus/reference/download_usdm.md).
The selected function validates its arguments, constructs download URLs,
and handles previews, downloads, optional extraction, and return values.

## Usage

``` r
download_drought(
  source = c("spei", "eddi", "usdm"),
  date = c("2020-01-01", "2020-12-31"),
  timescale = 1L,
  directory_to_save = NULL,
  acknowledgement = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 3L,
  rate_limit = 2,
  unzip = TRUE,
  remove_zip = FALSE,
  download = TRUE,
  remove_command = FALSE,
  version = c("2.11", "2.10", "legacy"),
  ...
)
```

## Arguments

- source:

  character(1). `"spei"` (default), `"eddi"`, or `"usdm"`. Uses
  [`base::match.arg()`](https://rdrr.io/r/base/match.arg.html), allowing
  unambiguous partial matches.

- date:

  character(1 or 2). One date or inclusive range endpoints in
  `"YYYY-MM-DD"` format, in either order. EDDI and USDM select Tuesdays
  within the interval and stop if none occur. Default is
  `c("2020-01-01", "2020-12-31")`. Ignored for SPEI, which downloads a
  complete multi-year file.

- timescale:

  integer(1). Positive accumulation timescale in months, used by SPEI
  and EDDI. Default is `1L`. Ignored for USDM. Input validation does not
  establish whether the provider has a file for that timescale.

- directory_to_save:

  character(1). Non-empty output directory. SPEI and EDDI save files
  directly here. USDM uses `zip_files/` for archives and `data_files/`
  for extracted files beneath this directory.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed, including for previews.
  Downloads and extracted files may require substantial storage. Default
  is `FALSE`.

- hash:

  logical(1). Return a combined MD5 hash of the output directory instead
  of the download summary. Uses `directory_to_save` for SPEI/EDDI and
  `data_files/` for USDM, including existing files. Default is `FALSE`;
  ignored when `download = FALSE`.

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

  logical(1). Extract USDM archives into `data_files/`. Default is
  `TRUE`. Ignored for SPEI and EDDI.

- remove_zip:

  logical(1). Remove USDM archives and their `zip_files/` directory
  recursively after extraction, including remaining contents. Requires
  `unzip = TRUE` for USDM. Default is `FALSE`. Ignored for SPEI and
  EDDI.

- download:

  logical(1). Deprecated. `FALSE` warns and returns a preview after
  directory creation, without making network requests. Default is
  `TRUE`.

- remove_command:

  logical(1). Deprecated and ignored; `TRUE` warns. No command file is
  generated. Default is `FALSE`.

- version:

  character(1). SPEI selection: `"2.11"` (default), `"2.10"`, or
  `"legacy"`. The last choice uses the unversioned `/files/` endpoint;
  its release is not identified by the URL. Uses
  [`base::match.arg()`](https://rdrr.io/r/base/match.arg.html). No
  automatic fallback between versions occurs. Ignored for EDDI and USDM.

- ...:

  Additional arguments forwarded to the selected source function. Its
  `...` argument is currently reserved and ignored.

## Value

The selected source function's result, with visibility preserved:

- With `download = FALSE`, an invisible list containing `urls`,
  `destfiles`, and `n_files`.

- With `download = TRUE` and `hash = FALSE`, an invisible download
  summary containing `success`, `failed`, `skipped`, `failed_urls`, and
  `failed_files`. The two failure vectors are `character(0)` when no
  downloads failed, including when every file was skipped.

- With `download = TRUE` and `hash = TRUE`, a visible character scalar
  containing the combined MD5 hash of the source's output directory.

Counts describe file downloads, not extraction or scientific validity.
Validation or extraction failures can prevent a normal return.

## Details

Only arguments relevant to the selected source are forwarded. Common
validation occurs once in the source function; this wrapper does not
repeat validation, directory creation, or download execution.

Previews create the output directories but make no network requests.
They include existing destination files. Actual downloads use
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md),
which skips existing non-empty files and reports individual failures.
See the source functions for endpoint limitations and extraction
behavior.

SPEI retains the flat `speiNN.nc` filename across versions for
compatibility with
[`process_drought()`](https://niehs.github.io/amadeus/reference/process_drought.md).
Existing-file checks do not distinguish versions. The `date` argument
does not subset SPEI downloads; temporal selection is performed during
processing.

## Note

No authentication is required. SPEI and EDDI provide raster inputs; USDM
provides polygon inputs for the downstream drought workflow.

## See also

[`download_spei()`](https://niehs.github.io/amadeus/reference/download_spei.md),
[`download_eddi()`](https://niehs.github.io/amadeus/reference/download_eddi.md),
[`download_usdm()`](https://niehs.github.io/amadeus/reference/download_usdm.md),
[`process_drought()`](https://niehs.github.io/amadeus/reference/process_drought.md),
[`calculate_drought()`](https://niehs.github.io/amadeus/reference/calculate_drought.md)

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_drought(
  source = "spei",
  timescale = 1L,
  version = "2.11",
  directory_to_save = "./data/drought",
  acknowledgement = TRUE
)
download_drought(
  source = "usdm",
  date = c("2020-01-07", "2020-03-31"),
  directory_to_save = "./data/drought",
  acknowledgement = TRUE
)
} # }
```
