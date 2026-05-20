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
  url_aqs_download = "https://aqs.epa.gov/aqsweb/airdata/",
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

  character(1). Currently only "daily" is supported.

- year:

  integer(1 or 2). Year or start/end years for downloading data.

- url_aqs_download:

  character(1). URL to the AQS pre-generated datasets.

- directory_to_save:

  character(1). Directory to save data.

- acknowledgement:

  logical(1). Must be TRUE to proceed.

- download:

  logical(1). DEPRECATED. Downloads happen automatically.

- remove_command:

  logical(1). Deprecated, ignored.

- unzip:

  logical(1). Unzip zip files (default TRUE).

- remove_zip:

  logical(1). Remove zip files after unzipping (default FALSE).

- show_progress:

  logical(1). Show download progress (default TRUE)

- hash:

  logical(1). Return hash of downloaded files (default FALSE)

- max_tries:

  integer(1). Maximum retry attempts (default 20)

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2)

## Value

invisible list with download results; or hash character if hash=TRUE

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
