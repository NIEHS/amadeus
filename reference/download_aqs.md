# Download air quality data

The `download_aqs()` function accesses and downloads Air Quality System
(AQS) data from the [U.S. Environmental Protection Agency's (EPA)
Pre-Generated Data
Files](https://aqs.epa.gov/aqsweb/airdata/download_files.html).

## Usage

``` r
download_aqs(
  parameter_code = 88101,
  resolution_temporal = "daily",
  year = c(2018, 2022),
  url_aqs_download = "https://aqs.epa.gov/aqsweb/airdata/",
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

- parameter_code:

  integer(1). length of 5. EPA pollutant parameter code. For details,
  please refer to [AQS parameter
  codes](https://aqs.epa.gov/aqsweb/documents/codetables/parameters.html)

- resolution_temporal:

  character(1). Name of column containing POC values. Currently, no
  value other than `"daily"` works.

- year:

  integer(1 or 2). length of 4. Year or start/end years for downloading
  data.

- url_aqs_download:

  character(1). URL to the AQS pre-generated datasets.

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
  download commands. Default is FALSE.

- unzip:

  logical(1). Unzip zip files. Default `TRUE`.

- remove_zip:

  logical(1). Remove zip file from directory_to_download. Default
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

- Zip and/or data files will be downloaded and stored in
  `directory_to_save`.

## References

U.S. Environmental Protection Agency (2023). “Air Quality System Data
Mart \[internet database\].”
<https://www.epa.gov/outdoor-air-quality-data>.

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
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
} # }
```
