# Download land cover data

The `download_nlcd()` function accesses and downloads annual land cover
data from the [Multi-Resolution Land Characteristics (MRLC) Consortium's
National Land Cover Database (NLCD) products data
base](https://www.mrlc.gov/data/project/annual-nlcd).

## Usage

``` r
download_nlcd(
  product = "Land Cover",
  year = 2021,
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

- product:

  character(1). "Land Cover", "Land Cover Change", "Land Cover
  Confidence", "Fractional Impervious Surface", "Impervious Descriptor",
  or "Spectral Change Day of Year ".

- year:

  integer(1). Available years for Coterminous United States range from
  1985 to 2023.

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

Dewitz J (2023). “National Land Cover Database (NLCD) 2021 Products.”
[doi:10.5066/P9JZ7AO3](https://doi.org/10.5066/P9JZ7AO3) .  
Dewitz J (2024). “National Land Cover Database (NLCD) 2019 Products
(ver. 3.0, February 2024).”
[doi:10.5066/P9KZCM54](https://doi.org/10.5066/P9KZCM54) .

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_nlcd(
  product = "Land Cover",
  year = 2021,
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE # NOTE: download skipped for examples
)
} # }
```
