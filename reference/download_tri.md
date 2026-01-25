# Download toxic release data

The `download_tri()` function accesses and downloads toxic release data
from the [U.S. Environmental Protection Agency's (EPA) Toxic Release
Inventory (TRI)
Program](https://www.epa.gov/toxics-release-inventory-tri-program/tri-data-action-0).

## Usage

``` r
download_tri(
  year = c(2018L, 2022L),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  hash = FALSE
)
```

## Arguments

- year:

  integer(1 or 2). length of 4. Year or start/end years for downloading
  data.

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

- Comma-separated value (CSV) files will be stored in
  `directory_to_save`.

## References

United States Environmental Protection Agency (2024). “TRI Basic Data
Files: Calendar Years 1987 – Present.”
<https://www.epa.gov/toxics-release-inventory-tri-program/tri-data-action-0>.

## Author

Mariana Kassien, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_tri(
  year = 2021L,
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE
)
} # }
```
