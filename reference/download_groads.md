# Download roads data

The `download_groads()` function accesses and downloads roads data from
[NASA's Global Roads Open Access Data Set (gROADS), v1
(1980-2010)](https://data.nasa.gov/dataset/global-roads-open-access-data-set-version-1-groadsv1).

## Usage

``` r
download_groads(
  data_region = c("Americas", "Global", "Africa", "Asia", "Europe", "Oceania East",
    "Oceania West"),
  data_format = c("Shapefile", "Geodatabase"),
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

- data_region:

  character(1). Data can be downloaded for `"Global"`, `"Africa"`,
  `"Asia"`, `"Europe"`, `"Americas"`, `"Oceania East"`, and
  `"Oceania West"`.

- data_format:

  character(1). Data can be downloaded as `"Shapefile"` or
  `"Geodatabase"`. (Only `"Geodatabase"` available for `"Global"`
  region).

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
Network-CIESIN-Columbia University, Information Technology Outreach
Services-ITOS-University Of Georgia (2013). “Global Roads Open Access
Data Set, Version 1 (gROADSv1).”
[doi:10.7927/H4VD6WCT](https://doi.org/10.7927/H4VD6WCT) ,
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
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
} # }
```
