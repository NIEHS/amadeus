# Download road emissions data

The `download_nei()` function accesses and downloads road emissions data
from the U.S Environmental Protection Agency's (EPA) National Emissions
Inventory (NEI).

## Usage

``` r
download_nei(
  epa_certificate_path = NULL,
  certificate_url = paste0("http://cacerts.digicert.com/",
    "DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt"),
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

- epa_certificate_path:

  TO BE DEPRECATED. Certificate path.

- certificate_url:

  TO BE DEPRECATED. Certificate URL.

- year:

  integer(1). Available years of NEI data.

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

## Note

NEI data does not require authentication.

## References

United States Environmental Protection Agency (2024). “Air Emissions
Inventories.” <https://www.epa.gov/air-emissions-inventories>.

## Author

Kyle Messier, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_nei(
  year = c(2017L, 2020L),
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
