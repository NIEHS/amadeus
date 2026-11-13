# Preview planned downloads

Print a message reporting the number of planned downloads and return
their URLs and destination paths without downloading files.

## Usage

``` r
download_preview(urls, destfiles)
```

## Arguments

- urls:

  character vector. Planned download URLs.

- destfiles:

  character vector. Corresponding destination file paths, in the same
  order and of the same length as `urls`.

## Value

A list, returned visibly, containing:

- `urls`: the supplied download URLs.

- `destfiles`: the supplied destination file paths.

- `n_files`: the number of supplied URLs, calculated as `length(urls)`.

## Details

The file count is based on the supplied URLs; remote availability and
existing local files are not checked. The calling download function
handles the `download = FALSE` condition and exits using
`return(invisible(download_preview(urls, destfiles)))`.

## See also

[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)

## Author

Mitchell Manware

## Examples

``` r
download_preview(
  urls = "https://example.com/data.zip",
  destfiles = file.path(tempdir(), "data.zip")
)
#> Skipping download. Found 1 files available for download.
#> $urls
#> [1] "https://example.com/data.zip"
#> 
#> $destfiles
#> [1] "/tmp/Rtmp9SZkBx/data.zip"
#> 
#> $n_files
#> [1] 1
#> 
```
