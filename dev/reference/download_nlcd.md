# Download National Land Cover Database (NLCD) data

Downloads NLCD data products from the Multi-Resolution Land
Characteristics (MRLC) Consortium. NLCD provides nationwide land cover
and land cover change information for the United States at a 30m
resolution.

## Usage

``` r
download_nlcd(
  product = "Land Cover",
  year = 2021,
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

- product:

  character(1). NLCD product type. One of:

  - "Land Cover" (default)

  - "Land Cover Change"

  - "Land Cover Confidence"

  - "Fractional Impervious Surface"

  - "Impervious Descriptor"

  - "Spectral Change Day of Year"

- year:

  integer(1). Year of NLCD data (1985-2024). Default is 2021.

- directory_to_save:

  character(1). Directory to save downloaded files.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed with download.

- download:

  logical(1). DEPRECATED. Downloads now happen automatically. Set to
  FALSE to skip downloading (generates file list only).

- remove_command:

  logical(1). Deprecated, ignored.

- unzip:

  logical(1). Unzip downloaded files? Default is `TRUE`.

- remove_zip:

  logical(1). Remove zip files after extraction? Default is `FALSE`.

- show_progress:

  logical(1). Show download progress? Default is `TRUE`.

- hash:

  logical(1). Return hash of downloaded files? Default is `FALSE`.

- max_tries:

  integer(1). Maximum download retry attempts. Default is 20.

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2)

## Value

invisible NULL; or hash character if hash=TRUE

## References

Dewitz J (2023). “National Land Cover Database (NLCD) 2021 Products.”
[doi:10.5066/P9JZ7AO3](https://doi.org/10.5066/P9JZ7AO3) .

## Author

Mitchell Manware, Insang Song, Kyle Messier

## Examples

``` r
if (FALSE) { # \dontrun{
# Download 2021 Land Cover
download_nlcd(
  product = "Land Cover",
  year = 2021,
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)

# Download Land Cover Change for 2019
download_nlcd(
  product = "Land Cover Change",
  year = 2019,
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = TRUE
)
} # }
```
