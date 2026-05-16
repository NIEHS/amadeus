# Get MERRA2 variable lookup information

Returns a lookup table of available MERRA2 collection and variable
selectors from locally downloaded MERRA2 netCDF files. This helper
inspects layer metadata only and does not read raster values into
memory.

## Usage

``` r
get_merra2_info(path = NULL, include_file = FALSE, ...)
```

## Arguments

- path:

  character(1+) Path(s) to MERRA2 file(s) and/or directory(ies)
  containing MERRA2 `.nc4` files (and optional FWI `.nc` files).

- include_file:

  logical(1). If `TRUE`, include a `file` column showing the source file
  for each collection-variable row. Default `FALSE`.

- ...:

  Placeholders.

## Value

a `data.frame` with MERRA2 collection and variable selectors.

## Author

Kyle Messier

## Examples

``` r
if (FALSE) { # \dontrun{
get_merra2_info(path = "./data/merra2")
get_merra2_info(path = "./data/merra2", include_file = TRUE)
} # }
```
