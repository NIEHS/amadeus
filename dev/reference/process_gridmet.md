# Process gridMET data

The `process_gridmet()` function imports and cleans raw gridded surface
meteorological data, returning a single `SpatRaster` object.

## Usage

``` r
process_gridmet(
  date = c("2023-09-01", "2023-09-10"),
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

  character(1). Variable name or acronym code. See [gridMET Generate
  Wget File](https://www.climatologylab.org/wget-gridmet.html) for
  variable names and acronym codes. (Note: variable "Burning Index" has
  code "bi" and variable "Energy Release Component" has code "erc").

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
acronym, and date.

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
gridmet <- process_gridmet(
  date = c("2023-01-01", "2023-01-10"),
  variable = "Precipitation",
  path = "./data/pr"
)
} # }
```
