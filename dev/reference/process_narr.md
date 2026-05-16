# Process meteorological data

The `process_narr()` function imports and cleans raw meteorological
data, returning a single `SpatRaster` object.

## Usage

``` r
process_narr(
  date = "2023-09-01",
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

  character(1). Variable name acronym. See [List of Variables in NARR
  Files](https://ftp.cpc.ncep.noaa.gov/NARR/fixed/merged_land_AWIP32corrected.pdf)
  for variable names and acronym codes.

- path:

  character(1). Directory with downloaded netCDF (.nc) files.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Placeholders.

## Value

a `SpatRaster` object

## Note

Layer names of the returned `SpatRaster` object contain the variable
acronym, pressure level, and date.

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
process_narr(
  date = c("2018-01-01", "2018-01-10"),
  variable = "weasd",
  path = "./tests/testdata/narr/weasd"
)
} # }
```
