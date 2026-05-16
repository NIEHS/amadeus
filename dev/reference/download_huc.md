# Download National Hydrography Dataset (NHD) data

NHDPlus data provides the most comprehensive and high-resolution
hydrography data. This function downloads **national** dataset from
NHDPlus Version 2.1 on USGS Amazon S3 storage.

## Usage

``` r
download_huc(
  region = c("Lower48", "Islands"),
  type = c("Seamless", "OceanCatchment"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- region:

  character(1). One of `c("Lower48", "Islands")`. When `"Islands"` is
  selected, the data will be downloaded for Hawaii, Puerto Rico, and
  Virgin Islands.

- type:

  character(1). One of `c("Seamless", "OceanCatchment")`.

- directory_to_save:

  character(1). Directory to download files.

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

  logical(1). Unzip the downloaded compressed files. Default is `FALSE`.
  Supports ".7z" extraction via archive.

- hash:

  logical(1). By setting `TRUE` the function will return an
  [`rlang::hash_file()`](https://rlang.r-lib.org/reference/hash.html)
  hash character corresponding to the downloaded files. Default is
  `FALSE`.

- show_progress:

  logical(1). Show download progress. Default is `TRUE`.

- max_tries:

  integer(1). Maximum download retry attempts. Default is `20`.

- rate_limit:

  numeric(1). Minimum seconds between requests. Default is `2`.

## Value

- For `hash = FALSE`, NULL

- For `hash = TRUE`, an
  [`rlang::hash_file`](https://rlang.r-lib.org/reference/hash.html)
  character.

- Downloaded files will be stored in `directory_to_save`.

## Note

For HUC, set `type = "Seamless"`. HUC12 layer presents in the seamless
geodatabase. Users can aggregate HUC12 layer to make HUC6, HUC8, HUC10,
etc. For whom wants to download a specific region, please visit [Get
NHDPlus
Data](https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data#ListofAreas)

## References

U.S. Geological Survey (2023). “National Hydrography Dataset (NHD) –
USGS National Map Downloadable Data Collection.”
<https://www.usgs.gov/national-hydrography>.

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_huc(
  region = "Lower48",
  type = "Seamless",
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
} # }
```
