# Create hash of downloaded files.

Create a combined md5sum hash based on the files in a specified
directory.

## Usage

``` r
download_hash(hash = TRUE, dir = NULL)
```

## Arguments

- hash:

  logical(1). Create hash of downloaded files.

- dir:

  character(1). Directory path.

## Value

character(1) Combined 128-bit md5sum of download files.
