# Download CropScape data

Accesses and downloads United States Department of Agriculture CropScape
Cropland Data Layer data from the [USDA National Agricultural Statistics
Service](https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php)
or the [George Mason University
website](https://nassgeodata.gmu.edu/CropScape/).

## Usage

``` r
download_cropscape(
  year = seq(1997, 2023),
  source = c("USDA", "GMU"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- year:

  integer(1). Year of the data to download.

- source:

  character(1). Data source, one of `c("USDA", "GMU")`.

  - `"USDA"` will download the national data from the USDA website
    (available in 2008-last year).

  - `"GMU"` will download the data from the George Mason University
    website (available in 1997-last year).

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

- Yearly comma-separated value (CSV) files will be stored in
  `directory_to_save`.

## Note

JSON files should be found at STAC catalog of OpenLandMap

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_cropscape(
  year = 2020,
  source = "USDA",
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
} # }
```
