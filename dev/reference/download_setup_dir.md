# Setup directory

Create `directory` if it does not already exist.

If directory does not exist, the directory will be created.

## Usage

``` r
download_setup_dir(directory, zip = FALSE)
```

## Arguments

- directory:

  character(1) directory path

- zip:

  logical(1). Should sub-directories be created for zip files and data
  files? If `TRUE`, a vector of sub-directoy names will be returned.

## Value

NULL; if `zip = TRUE` a vector of directories for zip files and data
files
