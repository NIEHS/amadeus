# Download PRISM data

Accesses and downloads Oregon State University's PRISM data from the
PRISM Climate Group Web Service

## Usage

``` r
download_prism(
  time,
  element = c("ppt", "tmin", "tmax", "tmean", "tdmean", "vpdmin", "vpdmax", "solslope",
    "soltotal", "solclear", "soltrans"),
  data_type = c("ts", "normals_800", "normals"),
  format = c("nc", "asc", "grib2"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- time:

  character(1). Length of 2, 4, 6, or 8. Time period for time series or
  normals. According to the PRISM Web Service Guide, acceptable formats
  include (disclaimer: the following is a direct quote; minimal
  formatting is applied): **Time Series**:

  - `YYYYMMDD` for daily data (between yesterday and January 1st, 1981)
    – returns a single grid in a .zip file

  - `YYYYMM` for monthly data (between last month and January 1981) –
    returns a single grid in a .zip file

  - `YYYY` for annual data (between last year and 1981) - returns a
    single grid in a .zip file

  - `YYYY` for historical data (between 1980 and 1895) - returns a
    single zip file containing 12 monthly grids for `YYYY` plus the
    annual.

  **Normals**:

  - Monthly normal: date is `MM` (i.e., 04 for April) or the value 14,
    which returns the annual normal

  - Daily normal: date is `MMDD` (i.e., 0430 for April 30)

- element:

  character(1). Data element. One of
  `c("ppt", "tmin", "tmax", "tmean", "tdmean", "vpdmin", "vpdmax")` For
  normals, `c("solslope", "soltotal", "solclear", "soltrans")` are also
  accepted.

- data_type:

  character(1). Data type.

  - `"ts"`: 4km resolution time series.

  - `"normals_800"`: 800m resolution normals.

  - `"normals"`: 4km resolution normals.

- format:

  character(1). Data format. Only applicable for `data_type = "ts"`.

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

  logical(1). Unzip the downloaded zip file to extract the data files
  (nc, grib2, etc.) into `directory_to_save`. Default is `TRUE`. The
  PRISM API always returns a zip regardless of the requested format.

- remove_zip:

  logical(1). Remove the zip file after unzipping. Default is `FALSE`.
  Only applies when `unzip = TRUE`.

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

- .bil (normals) or single grid files depending on the format choice
  will be stored in `directory_to_save`.

## References

Daly C, Taylor GH, Gibson WP, Parzybok TW, Johnson GL, Pasteris PA
(2000). “HIGH-QUALITY SPATIAL CLIMATE DATA SETS FOR THE UNITED STATES
AND BEYOND.” *Transactions of the ASAE*, **43**(6), 1957–1962. ISSN
2151-0059. [doi:10.13031/2013.3101](https://doi.org/10.13031/2013.3101)
.
<http://elibrary.asabe.org/abstract.asp??JID=3&AID=3101&CID=t2000&v=43&i=6&T=1>.

- [PRISM Climate Group](https://prism.oregonstate.edu/)

- [PRISM Web Service
  Guide](https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf)

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_prism(
  time = "202104",
  element = "ppt",
  data_type = "ts",
  format = "nc",
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE
)
} # }
```
