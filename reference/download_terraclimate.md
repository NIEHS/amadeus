# Download TerraClimate data

The `download_terraclimate` function accesses and downloads climate and
water balance data from the [University of California Merced Climatology
Lab's TerraClimate
dataset](https://www.climatologylab.org/terraclimate.html).

## Usage

``` r
download_terraclimate(
  variables = NULL,
  year = c(2018, 2022),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  hash = FALSE
)
```

## Arguments

- variables:

  character(1). Variable(s) name(s). See [TerraClimate Direct
  Downloads](https://climate.northwestknowledge.net/TERRACLIMATE/index_directDownloads.php)
  for variable names and acronym codes.

- year:

  integer(1 or 2). length of 4. Year or start/end years for downloading
  data.

- directory_to_save:

  character(1). Directory(s) to save downloaded data files.

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

- netCDF (.nc) files will be stored in a variable-specific folder within
  `directory_to_save`.

## References

Abatzoglou JT, Dobrowski SZ, Parks SA, Hegewisch KC (2018).
“TerraClimate, a high-resolution global dataset of monthly climate and
climatic water balance from 1958–2015.” *Scientific data*, **5**(1),
1–12.

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_terraclimate(
  variables = "Precipitation",
  year = 2023,
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE
)
} # }
```
