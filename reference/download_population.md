# Download population density data

The `download_population()` function accesses and downloads population
density data from [NASA's UN WPP-Adjusted Population Density,
v4.11](https://earthdata.nasa.gov/data/catalog/sedac-ciesin-sedac-gpwv4-apdens-wpp-2015-r11-4.11).

## Usage

``` r
download_population(
  data_resolution = "60 minute",
  data_format = c("GeoTIFF", "ASCII", "netCDF"),
  year = "2020",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  hash = FALSE
)
```

## Arguments

- data_resolution:

  character(1). Available resolutions are 30 second (approx. 1 km), 2.5
  minute (approx. 5 km), 15 minute (approx. 30 km), 30 minute (approx.
  55 km), and 60 minute (approx. 110 km).

- data_format:

  character(1). Individual year data can be downloaded as `"ASCII"` or
  `"GeoTIFF"`. "all" years is downloaded as `"netCDF"`.

- year:

  character(1). Available years are `2000`, `2005`, `2010`, `2015`, and
  `2020`, or `"all"` for all years.

- directory_to_save:

  character(1). Directory to save data. Two sub-directories will be
  created for the downloaded zip files ("/zip_files") and the unzipped
  shapefiles ("/data_files").

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

  logical(1). Unzip zip files. Default is `TRUE`.

- remove_zip:

  logical(1). Remove zip files from directory_to_download. Default is
  `FALSE`.

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

## References

Center For International Earth Science Information
Network-CIESIN-Columbia University (2017). “Gridded Population of the
World, Version 4 (GPWv4): Population Density, Revision 11.”
[doi:10.7927/H49C6VHW](https://doi.org/10.7927/H49C6VHW) ,
<https://earthdata.nasa.gov/data/catalog/sedac-ciesin-sedac-gpwv4-popdens-r11-4.11>.

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_population(
  data_resolution = "30 second",
  data_format = "GeoTIFF",
  year = "2020",
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
} # }
```
