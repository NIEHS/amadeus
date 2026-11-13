# Download National Land Cover Database (NLCD) data

Downloads NLCD data products from the Multi-Resolution Land
Characteristics (MRLC) Consortium. NLCD provides nationwide land cover
and land cover change information for the United States at a 30m
resolution.

## Usage

``` r
download_nlcd(
  product = "Land Cover",
  year = 2021,
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

- product:

  character(1). NLCD product type. One of:

  - "Land Cover" (default)

  - "Land Cover Change"

  - "Land Cover Confidence"

  - "Fractional Impervious Surface"

  - "Impervious Descriptor"

  - "Spectral Change Day of Year"

- year:

  integer(1 or more). One year, two endpoints, or a non-empty vector of
  years. Every year from the minimum through the maximum is requested,
  including gaps: `c(2018, 2023)` requests `2018:2023`. Requested years
  must be within 1985-2024. Default is 2021. Missing, fractional, and
  out-of-range years are rejected. Any year outside these fixed bounds
  causes the entire request to fail; years are not filtered or
  truncated.

- directory_to_save:

  character(1). Parent directory. Archives are saved in `zip_files/` and
  extracted data in `data_files/`.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed with download.

- download:

  logical(1). DEPRECATED. Downloads now happen automatically. Set to
  FALSE to skip downloading (generates file list only).

- remove_command:

  logical(1). Deprecated, ignored.

- unzip:

  logical(1). Extract all planned yearly archives, including existing
  archives, into the shared `data_files/` directory. Default is `TRUE`;
  `FALSE` skips extraction.

- remove_zip:

  logical(1). Remove archives and their `zip_files/` directory
  recursively, including remaining contents, after the extraction loop
  finishes. Default is `FALSE`; requires `unzip = TRUE`.

- show_progress:

  logical(1). Show download progress? Default is `TRUE`.

- hash:

  logical(1). Return a combined MD5 hash of all files recursively under
  `data_files/`, including existing files. Default is `FALSE`; ignored
  when `download = FALSE`. With `unzip = FALSE`, hashing still uses
  `data_files/`, which may be empty or contain previously extracted
  data.

- max_tries:

  integer(1). Maximum download retry attempts. Default is 20.

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2)

## Value

When `download = TRUE` and `hash = FALSE`, an invisible list containing
`success`, `failed`, and `skipped` file counts. If all requested
archives already exist and are nonempty, no files are downloaded; the
returned list contains `success = 0`, `failed = 0`, and `skipped` equal
to the number of requested files. The list always contains `failed_urls`
and `failed_files` character vectors, both `character(0)` when no
downloads failed, including when every file was skipped.

When `download = FALSE`, an invisible list containing `urls`,
`destfiles`, and `n_files` is returned without downloading, regardless
of `hash`.

When `download = TRUE` and `hash = TRUE`, a character(1) combined MD5
hash of the files in the `data_files` directory is returned instead of a
list.

## References

Dewitz J (2023). “National Land Cover Database (NLCD) 2021 Products.”
[doi:10.5066/P9JZ7AO3](https://doi.org/10.5066/P9JZ7AO3) .

## Author

Mitchell Manware, Insang Song, Kyle Messier

## Examples

``` r
if (FALSE) { # \dontrun{
# Download 2021 Land Cover
download_nlcd(
  product = "Land Cover",
  year = 2021,
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)

# Download Land Cover Change for 2019
download_nlcd(
  product = "Land Cover Change",
  year = 2019,
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  unzip = TRUE,
  remove_zip = TRUE
)
} # }
```
