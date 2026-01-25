# Process toxic release data

This function imports and cleans raw toxic release data, returning a
single `SpatVector` (points) object for the selected `year`.

## Usage

``` r
process_tri(
  path = NULL,
  year = 2018,
  variables = c(1, 13, 12, 14, 20, 34, 36, 47, 48, 49),
  extent = NULL,
  ...
)
```

## Arguments

- path:

  character(1). Path to the directory with TRI CSV files

- year:

  integer(1). Single year to select.

- variables:

  integer. Column index of TRI data.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Placeholders.

## Value

a `SpatVector` object (points) in `year` `year` is stored in a field
named `"year"`.

## Note

Visit [TRI Data and
Tools](https://www.epa.gov/toxics-release-inventory-tri-program/tri-toolbox)
to view the available years and variables.

## References

https://www.epa.gov/toxics-release-inventory-tri-program/tri-toolbox

## Author

Insang Song, Mariana Kassien

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
tri <- process_tri(
  path = "./data",
  year = 2020,
  variables = c(1, 13, 12, 14, 20, 34, 36, 47, 48, 49)
)
} # }
```
