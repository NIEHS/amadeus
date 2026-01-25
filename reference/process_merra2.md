# Process meteorological and atmospheric data

The `process_merra2()` function imports and cleans raw atmospheric
composition data, returning a single `SpatRaster` object.

## Usage

``` r
process_merra2(
  date = c("2018-01-01", "2018-01-10"),
  variable = NULL,
  path = NULL,
  extent = NULL,
  ...
)
```

## Arguments

- date:

  character(1 or 2). Date (1) or start and end dates (2). Format
  YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").

- variable:

  character(1). MERRA2 variable name(s).

- path:

  character(1). Directory with downloaded netCDF (.nc4) files.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Placeholders.

## Value

a `SpatRaster` object;

## Note

Layer names of the returned `SpatRaster` object contain the variable,
pressure level, date, and hour. Pressure level values utilized for layer
names are taken directly from raw data and are not edited to retain
pressure level information.

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
merra2 <- process_merra2(
  date = c("2024-01-01", "2024-01-10"),
  variable = "CPT",
  path = "./data/inst1_2d_int_Nx"
)
} # }
```
