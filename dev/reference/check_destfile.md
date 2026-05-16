# Check if destination file exists or is 0 bytes.

Check if destination file exists or is 0 bytes. If either condition is
met, the function returns `TRUE` to allow the download to proceed.

## Usage

``` r
check_destfile(destfile)
```

## Arguments

- destfile:

  character(1). Destination file path.

## Value

logical(1)
