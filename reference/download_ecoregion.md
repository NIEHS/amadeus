# Download ecoregion data

The `download_ecoregion()` function accesses and downloads United States
Ecoregions data from the U.S. Environmental Protection Agency's (EPA)
Ecoregions.

## Usage

``` r
download_ecoregion(
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- directory_to_save:

  character(1). Directory to save data.

- acknowledgement:

  logical(1). Must be TRUE to proceed.

- download:

  logical(1). DEPRECATED. Downloads happen automatically. Set to FALSE
  to skip downloading and return the planned file list.

- remove_command:

  logical(1). Deprecated, ignored.

- unzip:

  logical(1). Unzip zip files (default TRUE).

- remove_zip:

  logical(1). Remove zip files after unzipping (default FALSE).

- show_progress:

  logical(1). Show download progress (default TRUE)

- hash:

  logical(1). Return hash of downloaded files (default FALSE)

- max_tries:

  integer(1). Maximum retry attempts (default 20)

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2)

## Value

When `download = TRUE` and `hash = FALSE`, an invisible list containing
`success`, `failed`, and `skipped` file counts. If the requested archive
already exists and is nonempty, no file is downloaded; the returned list
contains `success = 0`, `failed = 0`, and `skipped = 1`. The list always
contains `failed_urls` and `failed_files` character vectors, both
`character(0)` when no downloads failed, including when the file was
skipped.

When `download = FALSE`, an invisible list containing `urls`,
`destfiles`, and `n_files = 1` is returned without downloading,
regardless of `hash`.

When `download = TRUE` and `hash = TRUE`, a character(1) combined MD5
hash of the files in the `data_files` directory is returned instead of a
list.

## Note

Ecoregion data does not require authentication.

## References

Omernik JM, Griffith GE (2014). “Ecoregions of the Conterminous United
States: Evolution of a Hierarchical Spatial Framework.” *Environmental
Management*, **54**(6), 1249–1266. ISSN 0364-152X, 1432-1009.
[doi:10.1007/s00267-014-0364-1](https://doi.org/10.1007/s00267-014-0364-1)
. <https://link.springer.com/article/10.1007/s00267-014-0364-1>.

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_ecoregion(
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
