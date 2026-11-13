# Download NEI onroad emissions data

Download onroad emissions CSV archives from the U.S. Environmental
Protection Agency's National Emissions Inventory (NEI). Archives and
extracted data are stored in separate subdirectories using the shared
download workflow.

## Usage

``` r
download_nei(
  year = c(2017L, 2020L),
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

  integer(1 or more). Selected inventory years. Supported values are
  2008, 2011, 2014, 2017, 2020, and 2023. Default is `c(2017L, 2020L)`.
  Each value selects an individual inventory; `c(2017, 2023)` requests
  only those two years, without filling gaps. The download plan
  preserves the supplied order and repeated years. Unsupported years are
  omitted with a warning that lists the retained years. If no supported
  years remain, the function stops before creating directories or
  contacting the provider.

- directory_to_save:

  character(1). Non-empty parent directory. Archives are downloaded into
  `zip_files/` and, when `unzip = TRUE`, extracted into separate
  per-archive directories under `data_files/`.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed. Downloaded archives and
  extracted CSV files can require substantial storage.

- download:

  logical(1). Deprecated. `FALSE` warns and returns a preview after
  directory creation, without contacting the provider. No archives are
  downloaded or extracted in preview mode. Default is `TRUE`.

- remove_command:

  logical(1). Deprecated and ignored; `TRUE` warns. No command file is
  generated. Default is `FALSE`.

- unzip:

  logical(1). Extract downloaded ZIP archives into per-archive
  directories under `data_files/`. Default is `TRUE`; `FALSE` skips
  extraction and creation of the per-archive directories.

- remove_zip:

  logical(1). Remove downloaded archives and their `zip_files/`
  directory, including remaining contents, after the extraction loop
  finishes. Default is `FALSE`. May be `TRUE` only when `unzip = TRUE`.
  No removal occurs when `download = FALSE`.

- show_progress:

  logical(1). Show file download progress. Default is `TRUE`. Other
  status messages may still be printed when `FALSE`.

- hash:

  logical(1). Return a combined MD5 hash of the files under
  `data_files/` instead of the download summary. Default is `FALSE`.
  Ignored when `download = FALSE`.

- max_tries:

  integer(1). Maximum attempts per file download request, including the
  initial attempt. Default is 20.

- rate_limit:

  numeric(1). Non-negative throttling interval in seconds, passed to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
  Default is 2.

## Value

With `download = FALSE`, an invisible preview list containing `urls`,
`destfiles`, and `n_files`. Otherwise, after downloading and optional
extraction, `hash = FALSE` returns the download summary from
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
invisibly. With `hash = TRUE`, returns a visible character scalar
containing the combined MD5 hash of `data_files/`.

The download summary always contains `success`, `failed`, `skipped`,
`failed_urls`, and `failed_files`. The two failure vectors are
`character(0)` when no files failed, including when every file was
skipped.

## Details

A year-specific lookup selects the provider's archive path and filename.
NEI releases use different version directories and naming conventions.

`zip_files/` and `data_files/` are sibling directories created beneath
`directory_to_save`, including for previews and when `unzip = FALSE`.
Each archive is extracted into a directory named after the archive with
its `.zip` suffix removed. For example, the 2020 archive is saved as
`zip_files/2020nei_onroad_byregion.zip` and extracted into
`data_files/2020nei_onroad_byregion/`. This isolates duplicate filenames
across inventories. Paths stored inside each archive are preserved.

Preview destinations refer to the archives in `zip_files/`, including
files that already exist locally. For actual downloads,
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
skips existing non-empty archives; they are still passed to extraction
when `unzip = TRUE`. Preview mode makes no network requests. Individual
download failures are recorded by the shared runner during file
transfers.

Hashing covers all files recursively under `data_files/`, including the
per-archive subdirectories and existing files, and excludes archives in
`zip_files/`. With `unzip = FALSE`, hashing still uses `data_files/`,
which may be empty or contain previously extracted data.

Download support covers more inventory years than
[`process_nei()`](https://niehs.github.io/amadeus/reference/process_nei.md),
which currently accepts only 2017 and 2020.

## Note

NEI data does not require authentication.

## References

United States Environmental Protection Agency (2024). “Air Emissions
Inventories.” <https://www.epa.gov/air-emissions-inventories>.

## See also

[`download_setup_dir()`](https://niehs.github.io/amadeus/reference/download_setup_dir.md),
[`download_unzip()`](https://niehs.github.io/amadeus/reference/download_unzip.md),
[`process_nei()`](https://niehs.github.io/amadeus/reference/process_nei.md)

## Author

Kyle Messier, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_nei(
  year = c(2017L, 2020L),
  directory_to_save = file.path(tempdir(), "nei"),
  acknowledgement = TRUE,
  download = FALSE # Preview selected inventory URLs and archive paths.
)
} # }
```
