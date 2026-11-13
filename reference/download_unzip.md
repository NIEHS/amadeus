# Extract downloaded archives

Extract one or more downloaded `.zip`, `.7z`, `.tar`, or `.gz` archives,
including `.tar.gz` files, into shared or separate directories.

## Usage

``` r
download_unzip(file_name, directory_to_unzip, unzip = TRUE, remove = FALSE)
```

## Arguments

- file_name:

  character vector. One or more non-missing, non-empty archive paths, in
  extraction order.

- directory_to_unzip:

  character vector. One shared extraction directory or one directory per
  archive, matched by position to `file_name`. Paths must be non-missing
  and non-empty; other vector lengths are rejected.

- unzip:

  logical(1). Extract the supplied archives. Default is `TRUE`. `FALSE`
  skips path validation, extraction, and removal.

- remove:

  logical(1). Run
  [`download_remove_zips()`](https://niehs.github.io/amadeus/reference/download_remove_zips.md)
  once for the complete archive vector after the extraction loop
  finishes. Default is `FALSE`; ignored when `unzip = FALSE`.

## Value

Invisible `NULL`, for both single and multiple archives and when
`unzip = FALSE`. Extracted paths and per-file statuses are not returned.

## Details

Archives are extracted sequentially. ZIP files use
[`utils::unzip()`](https://rdrr.io/r/utils/unzip.html); `.7z`, `.tar`,
and `.gz` files use
[`archive::archive_extract()`](https://archive.r-lib.org/reference/archive_extract.html).
Paths stored inside each archive are preserved beneath its corresponding
extraction directory. An extraction error stops the remaining
extractions and prevents cleanup; files already extracted remain on
disk. Warnings from the extraction functions are not converted to
errors.

## Note

When `remove = TRUE`, the existing cleanup helper removes the supplied
archives and recursively deletes their immediate parent directories,
including any remaining contents. Removal occurs after the extraction
loop, not between individual archives.

## See also

[`download_remove_zips()`](https://niehs.github.io/amadeus/reference/download_remove_zips.md),
[`utils::unzip()`](https://rdrr.io/r/utils/unzip.html),
[`archive::archive_extract()`](https://archive.r-lib.org/reference/archive_extract.html)
