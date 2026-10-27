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

## Details

EDDI uses the same native GRIDMET multi-year workflow as SPEI. Use
`eddi14d`, `eddi30d`, `eddi90d`, `eddi180d`, `eddi270d`, `eddi1y`,
`eddi2y`, or `eddi5y`; `eddi` defaults to `eddi30d`. Both indices retain
the native 1/24-degree grid and five-day observation dates. Select
dataset `gridmet` in the wrapper functions to use these products; the
standalone drought source aliases continue to select their separate
products. GRIDMET SPEI uses native five-day observations, not daily
values. Use `variable = "spei"` (alias for `"spei30d"`) or an explicit
accumulation code: `"spei14d"`, `"spei30d"`, `"spei90d"`, `"spei180d"`,
`"spei270d"`, `"spei1y"`, `"spei2y"`, or `"spei5y"`. Set `path` to the
code directory created by
[`download_gridmet()`](https://niehs.github.io/amadeus/reference/download_gridmet.md)
(e.g., spei30d). The multi-year file is subset using its observation
dates; no daily interpolation is performed. Layer names retain the
accumulation period (e.g., spei30d_20200105).

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
