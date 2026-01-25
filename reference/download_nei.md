# Download road emissions data

The `download_nei()` function accesses and downloads road emissions data
from the [U.S Environmental Protection Agency's (EPA) National Emissions
Inventory
(NEI)](https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei).

## Usage

``` r
download_nei(
  epa_certificate_path = NULL,
  certificate_url =
    "http://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt",
  year = c(2017L, 2020L),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE,
  hash = FALSE
)
```

## Arguments

- epa_certificate_path:

  character(1). Path to the certificate file for EPA DataCommons.
  Default is 'extdata/cacert_gaftp_epa.pem' under the package
  installation path. Use
  [`system.file()`](https://rdrr.io/r/base/system.file.html) to get the
  full path.

- certificate_url:

  character(1). URL to certificate file. See notes for details.

- year:

  integer(1) Available years of NEI data. Default is `c(2017L, 2020L)`.

- directory_to_save:

  character(1). Directory to save data. Two sub-directories will be
  created for the downloaded zip files ("/zip_files") and the unzipped
  data files ("/data_files").

- acknowledgement:

  logical(1). By setting `TRUE` the user acknowledges that the data
  downloaded using this function may be very large and use lots of
  machine storage and memory.

- download:

  logical(1). `FALSE` will generate a \*.txt file containing all
  download commands. By setting `TRUE` the function will download all of
  the requested data files.

- remove_command:

  logical(1). Remove (`TRUE`) or keep (`FALSE`) the text file containing
  download commands.

- unzip:

  logical(1). Unzip the downloaded zip files. Default is `FALSE`.

- hash:

  logical(1). By setting `TRUE` the function will return an
  [`rlang::hash_file()`](https://rlang.r-lib.org/reference/hash.html)
  hash character corresponding to the downloaded files. Default is
  `FALSE`.

## Value

- For `hash = FALSE`, NULL

- For `hash = TRUE`, an
  [`rlang::hash_file`](https://rlang.r-lib.org/reference/hash.html)
  character.

- Zip and/or data files will be downloaded and stored in respective
  sub-directories within `directory_to_save`.

## Note

For EPA Data Commons certificate errors, follow the steps below:

1.  Click Lock icon in the address bar at https://gaftp.epa.gov

2.  Click Show Certificate

3.  Access Details

4.  Find URL with \*.crt extension Currently we bundle the
    pre-downloaded crt and its PEM (which is accepted in wget command)
    file in ./inst/extdata. The instruction above is for certificate
    updates in the future.

## References

United States Environmental Protection Agency (2024). “Air Emissions
Inventories.” <https://www.epa.gov/air-emissions-inventories>.

## Author

Ranadeep Daw, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_nei(
  year = c(2017L, 2020L),
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
} # }
```
