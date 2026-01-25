# Process elevation data

The `process_gmted()` function imports and cleans raw elevation data,
returning a single `SpatRaster` object.

## Usage

``` r
process_gmted(variable = NULL, path = NULL, extent = NULL, ...)
```

## Arguments

- variable:

  vector(1). Vector containing the GMTED statistic first and the
  resolution second. (Example: variable = c("Breakline Emphasis", "7.5
  arc-seconds")).

  - Statistic options: "Breakline Emphasis", "Systematic Subsample",
    "Median Statistic", "Minimum Statistic", "Mean Statistic", "Maximum
    Statistic", "Standard Deviation Statistic"

  - Resolution options: "30 arc-seconds", "15 arc-seconds", "7.5
    arc-seconds"

- path:

  character(1). Directory with downloaded GMTED "\*\_grd" folder
  containing .adf files.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Placeholders.

## Value

a `SpatRaster` object

## Note

`SpatRaster` layer name indicates selected variable and resolution, and
year of release (2010).

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
gmted <- process_gmted(
  variable = c("Breakline Emphasis", "7.5 arc-seconds"),
  path = "./data/be75_grd"
)
} # }
```
