# Get MODIS product subdataset lookup information

Returns a lookup table of available MODIS product and subdataset
selectors from locally downloaded MODIS/VIIRS-style HDF/H5 files. This
helper uses metadata inspection (`terra::describe(..., sds = TRUE)` and
layer names) and does not read raster values into memory.

## Usage

``` r
get_modis_info(path = NULL, include_file = FALSE, ...)
```

## Arguments

- path:

  character(1+) Path(s) to MODIS file(s) and/or directory(ies)
  containing `.hdf`/`.h5` files.

- include_file:

  logical(1). If `TRUE`, include a `file` column showing the source file
  for each product-subdataset row. Default `FALSE`.

- ...:

  Placeholders.

## Value

a `data.frame` with MODIS product and subdataset selectors.

## Author

Kyle Messier

## Examples

``` r
if (FALSE) { # \dontrun{
get_modis_info(path = "./data/modis")
get_modis_info(path = "./data/modis", include_file = TRUE)
} # }
```
