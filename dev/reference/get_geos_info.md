# Get GEOS variable lookup information

Returns a lookup table of available GEOS collection and variable
selectors from locally downloaded GEOS-CF netCDF files. This helper
inspects layer metadata only and does not read raster values into
memory.

## Usage

``` r
get_geos_info(path = NULL, include_file = FALSE, ...)
```

## Arguments

- path:

  character(1+) Path(s) to GEOS file(s) and/or directory(ies) containing
  GEOS-CF `.nc4` files.

- include_file:

  logical(1). If `TRUE`, include a `file` column showing the source file
  for each collection-variable row. Default `FALSE`.

- ...:

  Placeholders.

## Value

a `data.frame` with GEOS collection and variable selectors.

## Author

Kyle Messier

## Examples

``` r
if (FALSE) { # \dontrun{
get_geos_info(path = "./data/geos")
get_geos_info(path = "./data/geos", include_file = TRUE)
} # }
```
