# Report download totals and failed filenames

Print the numbers of successful, failed, and skipped downloads. When any
downloads failed, emit a warning listing their destination basenames.
Used by the sequential downloader and by the parent process after
concurrent download results have been combined.

## Usage

``` r
download_report(list)
```

## Arguments

- list:

  list. Download summary containing numeric(1) fields `success`,
  `failed`, and `skipped`, and a character vector `failed_files`
  containing the basenames of failed destinations. Other fields, such as
  `failed_urls`, are not used by this helper.

## Value

Used for its messaging side effects; callers should ignore the return
value. The calling downloader returns the download summary.

## Details

The summary message is always emitted when this helper is called. A
warning is emitted only when `failed > 0`, with `call. = FALSE`. This
helper does not validate the summary or inspect downloaded files.

## See also

[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md),
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)

## Author

Mitchell Manware

## Examples

``` r
amadeus:::download_report(list(
  success = 2,
  failed = 0,
  skipped = 1,
  failed_files = character(0)
))
#> 
#> === Download Summary ===
#> 2 succeeded, 0 failed, 1 skipped
```
