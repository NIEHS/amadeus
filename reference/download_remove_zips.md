# Remove zip files

Remove supplied archive files and recursively delete their parent
directories.

## Usage

``` r
download_remove_zips(remove = FALSE, download_name)
```

## Arguments

- remove:

  logical(1). Confirm removal. Default is FALSE.

- download_name:

  character vector. Archive file paths to remove.

## Value

Invisible `NULL`; optionally removes archives and their directories.

## Note

With `remove = TRUE`, each immediate parent directory returned by
`dirname(download_name)` is deleted recursively, including files that
were not listed in `download_name`. This helper does not check whether
extraction occurred. Call it only after all required extractions from
those directories have finished, with extracted data stored outside the
directories removed.
