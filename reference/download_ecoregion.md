# Download ecoregion data

The `download_ecoregion()` function accesses and downloads United States
Ecoregions data from the [U.S. Environmental Protection Agency's (EPA)
Ecorgions](https://www.epa.gov/eco-research/ecoregions). Level 3 data,
where all pieces of information in the higher levels are included, are
downloaded.

## Usage

``` r
download_ecoregion(
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
  download commands.

- unzip:

  logical(1). Unzip zip files. Default `TRUE`.

- remove_zip:

  logical(1). Remove zip file from `directory_to_download`. Default
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

Omernik JM, Griffith GE (2014). “Ecoregions of the Conterminous United
States: Evolution of a Hierarchical Spatial Framework.” *Environmental
Management*, **54**(6), 1249–1266. ISSN 0364-152X, 1432-1009,
[doi:10.1007/s00267-014-0364-1](https://doi.org/10.1007/s00267-014-0364-1)
, <https://link.springer.com/article/10.1007/s00267-014-0364-1>.

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_ecoregion(
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
} # }
```
