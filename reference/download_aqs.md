# Download air quality data

The `download_aqs()` function accesses and downloads Air Quality System
(AQS) data from the U.S. Environmental Protection Agency's (EPA)
Pre-Generated Data Files.

## Usage

``` r
download_aqs(
  parameter_code = 88101,
  resolution_temporal = "daily",
  year = c(2018, 2022),
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

- parameter_code:

  integer(1). EPA pollutant parameter code. See Details for a short list
  of common codes.

- resolution_temporal:

  character(1). "daily" or "hourly".

- year:

  integer(1 or more). One year, two endpoints, or a non-empty vector of
  years. Every year from the minimum through the maximum is requested,
  including gaps: `c(2018, 2023)` requests `2018:2023`. Default is
  `c(2018, 2022)`. Missing and fractional years are rejected.

- directory_to_save:

  character(1). Parent directory. Archives are saved in `zip_files/` and
  extracted data in `data_files/`.

- acknowledgement:

  logical(1). Must be TRUE to proceed.

- download:

  logical(1). DEPRECATED. Downloads happen automatically.

- remove_command:

  logical(1). Deprecated, ignored.

- unzip:

  logical(1). Extract all planned ZIP archives, including existing
  archives, into `data_files/`. Default is `TRUE`; `FALSE` skips
  extraction and CSV layout normalization.

- remove_zip:

  logical(1). Remove archives and their `zip_files/` directory
  recursively, including remaining contents, after extraction and CSV
  layout normalization. Default is `FALSE`; requires `unzip = TRUE`.

- show_progress:

  logical(1). Show download progress (default TRUE)

- hash:

  logical(1). Return a combined MD5 hash of all files recursively under
  `data_files/`, including existing files. Default is `FALSE`; ignored
  when `download = FALSE`. With `unzip = FALSE`, hashing still uses
  `data_files/`, which may be empty or contain previously extracted
  data.

- max_tries:

  integer(1). Maximum retry attempts (default 20)

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2)

## Value

With `download = FALSE`, an invisible preview list containing `urls`,
`destfiles`, and `n_files`. With `download = TRUE` and `hash = FALSE`,
an invisible summary from
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
containing `success`, `failed`, `skipped`, `failed_urls`, and
`failed_files`. The two failure vectors are `character(0)` when no
downloads failed, including when every file was skipped. With
`download = TRUE` and `hash = TRUE`, a visible character scalar
containing the combined MD5 hash of `data_files/`.

## Details

Common AQS parameter codes include:

- `88101` — PM2.5 - Local Conditions

- `88502` — Acceptable PM2.5 AQI & Speciation Mass

- `81102` — PM10 Total 0-10um STP

- `44201` — Ozone

- `42602` — Nitrogen dioxide (NO2)

- `42401` — Sulfur dioxide (SO2)

- `42101` — Carbon monoxide

This list is not exhaustive; for the full official table, see the linked
EPA AQS parameter code table.

Downloaded and existing archives are extracted into the shared
`data_files/` directory. The CSV layout is then normalized, followed by
optional archive removal and hashing of the resulting data directory.

## Note

AQS data does not require authentication. AQS measurements are generally
intended for use as dependent variables, so the package supports
download and processing for AQS but does not expose AQS through
[`calculate_covariates()`](https://niehs.github.io/amadeus/reference/calculate_covariates.md).

## References

U.S. Environmental Protection Agency (2023). “Air Quality System Data
Mart \[internet database\].”
<https://www.epa.gov/outdoor-air-quality-data>.

## See also

[EPA AQS Parameter
Codes](https://aqs.epa.gov/aqsweb/documents/codetables/parameters.csv)

## Author

Mariana Kassien, Insang Song, Mitchell Manware

## Examples

``` r
if (FALSE) { # \dontrun{
download_aqs(
  parameter_code = 88101,
  resolution_temporal = "daily",
  year = 2023,
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
