# Process wildfire smoke data

The `process_hms()` function imports and cleans raw wildfire smoke plume
coverage data, returning a single `SpatVector` object.

## Usage

``` r
process_hms(date = "2018-01-01", path = NULL, extent = NULL, ...)
```

## Arguments

- date:

  character(1 or 2). Date (1) or start and end dates (2). Format
  YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").

- path:

  character(1). Directory with downloaded NOAA HMS data files.

- extent:

  numeric(4) or SpatExtent giving the extent of the output if `NULL`
  (default), the entire data is returned

- ...:

  Placeholders.

## Value

a `SpatVector` or character object

## Note

`process_hms()` will return a character object if there are no wildfire
smoke plumes present for the selected dates and density. The returned
character will contain the density value and the sequence of dates for
which no wildfire smoke plumes were detected (see "Examples"). If
multiple density polygons overlap, the function will return the highest
density value.

## Author

Mitchell Manware

## Examples

``` r
hms <- process_hms(
  date = c("2018-12-30", "2019-01-01"),
  path = "../tests/testdata/hms/"
)
#> Smoke plume polygons absent from 2018-12-30 to 2019-01-01. Returning vector of dates.
```
