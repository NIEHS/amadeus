# Set up download directories

Create missing download directories, including parent directories, and
return their paths with trailing forward slashes.

## Usage

``` r
download_setup_dir(directory, zip = FALSE)
```

## Arguments

- directory:

  character(1). Directory path for downloads.

- zip:

  logical(1). If `TRUE`, also create `zip_files` and `data_files`
  subdirectories within `directory`. Default is `FALSE`.

## Value

If `zip = FALSE`, a character scalar containing `directory` with a
trailing forward slash. If `zip = TRUE`, a character vector containing
the paths to `zip_files` and `data_files`, in that order, each with a
trailing forward slash. Paths are returned whether the directories
already existed or were newly created.

## See also

[`download_sanitize_path()`](https://niehs.github.io/amadeus/reference/download_sanitize_path.md)

## Examples

``` r
directory <- file.path(tempdir(), "amadeus_downloads")
download_setup_dir(directory)
#> [1] "/tmp/Rtmp9SZkBx/amadeus_downloads/"
download_setup_dir(directory, zip = TRUE)
#> [1] "/tmp/Rtmp9SZkBx/amadeus_downloads/zip_files/" 
#> [2] "/tmp/Rtmp9SZkBx/amadeus_downloads/data_files/"
```
