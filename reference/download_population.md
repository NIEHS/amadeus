# Download population density data

The `download_population()` function accesses and downloads population
density data from NASA's UN WPP-Adjusted Population Density.

## Usage

``` r
download_population(
  data_resolution = "60 minute",
  data_format = c("GeoTIFF", "ASCII", "netCDF"),
  year = "2020",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2,
  nasa_earth_data_token = NULL
)
```

## Arguments

- data_resolution:

  character(1). Available resolutions.

- data_format:

  character(1). "ASCII", "GeoTIFF", or "netCDF".

- year:

  character(1). Available years or "all".

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

- nasa_earth_data_token:

  character(1). NASA EarthData bearer token. If NULL (default), reads
  from the `NASA_EARTHDATA_TOKEN` environment variable via
  [`get_token()`](https://niehs.github.io/amadeus/reference/get_token.md).

## Value

invisible list with download results; or hash character if hash=TRUE

## Note

Population data may require NASA EarthData authentication depending on
access method.

## References

Center For International Earth Science Information
Network-CIESIN-Columbia University (2017). “Gridded Population of the
World, Version 4 (GPWv4): Population Density, Revision 11.”
[doi:10.7927/H49C6VHW](https://doi.org/10.7927/H49C6VHW) .
<https://earthdata.nasa.gov/data/catalog/sedac-ciesin-sedac-gpwv4-popdens-r11-4.11>.

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
# RECOMMENDED: Set up token once (persists across sessions)
setup_nasa_token()

download_population(
  data_resolution = "30 second",
  data_format = "GeoTIFF",
  year = "2020",
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
