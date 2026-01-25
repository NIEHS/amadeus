# Download wildfire smoke data

The `download_hms()` function accesses and downloads wildfire smoke
plume coverage data from [NOAA's Hazard Mapping System Fire and Smoke
Product](https://www.ospo.noaa.gov/products/land/hms.html#0).

## Usage

``` r
download_hms(
  data_format = "Shapefile",
  date = c("2018-01-01", "2018-01-01"),
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

- data_format:

  character(1). "Shapefile" or "KML".

- date:

  character(1 or 2). length of 10. Date or start/end dates for
  downloading data. Format "YYYY-MM-DD" (ex. January 1, 2018 =
  `"2018-01-01"`). NOAA HMS data is available from August 5, 2005
  through present day. Data is unavailable for August 10, 2005.

- directory_to_save:

  character(1). Directory to save data. If `data_format = "Shapefile"`,
  two sub-directories will be created for the downloaded zip files
  ("/zip_files") and the unzipped shapefiles ("/data_files"). If
  `data_format = "KML"`, a single sub-directory ("/data_files") will be
  created.

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

  logical(1). Unzip zip files. Default is `TRUE`. (Ignored if
  `data_format = "KML"`.)

- remove_zip:

  logical(1). Remove zip files from directory_to_download. Default is
  `FALSE`. (Ignored if `data_format = "KML"`.)

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

(????). “Hazard Mapping System Fire and Smoke Product: Hazard Mapping
System.” <https://www.ospo.noaa.gov/products/land/hms.html#about>.
<https://www.ospo.noaa.gov/products/land/hms.html#about>.

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_hms(
  data_format = "Shapefile",
  date = "2024-01-01",
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
} # }
```
