# Download population density data

The `download_population()` function accesses and downloads population
density data from NASA's UN WPP-Adjusted Population Density.

## Usage

``` r
download_population(
  data_resolution = c("60 minute", "30 second", "2.5 minute", "15 minute", "30 minute"),
  data_format = c("GeoTIFF", "ASCII", "netCDF"),
  year = c("2020", "2000", "2005", "2010", "2015", "all"),
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

  character(1). One of `"60 minute"` (default), `"30 second"`,
  `"2.5 minute"`, `"15 minute"`, or `"30 minute"`.

- data_format:

  character(1). Requested format: `"GeoTIFF"` (default), `"ASCII"`, or
  `"netCDF"`.

- year:

  character(1). One year: `"2020"` (default), `"2000"`, `"2005"`,
  `"2010"`, or `"2015"`; use `"all"` for all available years.

- directory_to_save:

  character(1). Directory to save data.

- acknowledgement:

  logical(1). Must be TRUE to proceed.

- download:

  logical(1). DEPRECATED. Downloads happen automatically. Set to FALSE
  to skip downloading and return the planned file list.

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

When `download = TRUE` and `hash = FALSE`, an invisible list containing
`success`, `failed`, and `skipped` file counts. If the requested archive
already exists and is nonempty, no file is downloaded; the returned list
contains `success = 0`, `failed = 0`, and `skipped = 1`. The list always
contains `failed_urls` and `failed_files` character vectors, both
`character(0)` when no downloads failed, including when the file was
skipped.

When `download = FALSE`, an invisible list containing `urls`,
`destfiles`, and `n_files = 1` is returned without downloading,
regardless of `hash`.

When `download = TRUE` and `hash = TRUE`, a character(1) combined MD5
hash of the files in the `data_files` directory is returned instead of a
list.

## Details

`data_resolution`, `data_format`, and `year` each select one value.
Matching is case-sensitive, and unambiguous abbreviations are accepted.
If omitted, each argument uses its first listed choice.

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
