# Process TerraClimate data

The `process_terraclimate()` function imports and cleans climate and
water balance data, returning a single `SpatRaster` object.

## Usage

``` r
process_terraclimate(
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

  character(1). Variable name or acronym code. See [TerraClimate Direct
  Downloads](https://climate.northwestknowledge.net/TERRACLIMATE/index_directDownloads.php)
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
acronym, year, and month.

TerraClimate data has monthly temporal resolution, so the first day of
each month is used as a placeholder temporal value.

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
terraclimate <- process_terraclimate(
  date = c("2023-01-01", "2023-01-10"),
  variable = "Precipitation",
  path = "./data/ppt"
)
} # }
```
