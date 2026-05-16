# Download roads data

The `download_groads()` function accesses and downloads roads data from
NASA's Global Roads Open Access Data Set (gROADS).

## Usage

``` r
download_groads(
  data_region = c("Americas", "Global", "Africa", "Asia", "Europe", "Oceania East",
    "Oceania West"),
  data_format = c("Shapefile", "Geodatabase"),
  nasa_earth_data_token = NULL,
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

- data_region:

  character(1). Data region.

- data_format:

  character(1). "Shapefile" or "Geodatabase".

- nasa_earth_data_token:

  character(1) or NULL. NASA EarthData authentication token. Can be a
  token string, a path to a file containing the token, or `NULL` to read
  from the `NASA_EARTHDATA_TOKEN` environment variable.

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

gROADS data is hosted on NASA EarthData and requires a valid NASA
EarthData token for authentication. Set the `NASA_EARTHDATA_TOKEN`
environment variable or pass the token directly via
`nasa_earth_data_token`. Use
[`setup_nasa_token()`](https://niehs.github.io/amadeus/dev/reference/setup_nasa_token.md)
for setup.

## References

Center For International Earth Science Information
Network-CIESIN-Columbia University, Information Technology Outreach
Services-ITOS-University Of Georgia (2013). “Global Roads Open Access
Data Set, Version 1 (gROADSv1).”
[doi:10.7927/H4VD6WCT](https://doi.org/10.7927/H4VD6WCT) .
<https://data.nasa.gov/dataset/global-roads-open-access-data-set-version-1-groadsv1>.

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_groads(
  data_region = "Americas",
  data_format = "Shapefile",
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
