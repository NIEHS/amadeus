# Create hash of downloaded files.

Create a combined MD5 hash from file contents recursively under a
specified directory, including files in nested subdirectories and
previously saved files.

## Usage

``` r
download_hash(hash = TRUE, dir = NULL)
```

## Arguments

- hash:

  logical(1). Create hash of downloaded files.

- dir:

  character(1). Root directory to hash. Sibling directories are not
  included; hashing `data_files/` therefore excludes sibling
  `zip_files/`.

## Value

With `hash = TRUE`, a character(1) combined MD5 hash. With
`hash = FALSE`, invisible `NULL`.
