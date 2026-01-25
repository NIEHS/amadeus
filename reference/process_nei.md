# Process road emissions data

The `process_nei()` function imports and cleans raw road emissions data,
returning a single `SpatVector` object.

NEI data comprises multiple csv files where emissions of 50+ pollutants
are recorded at county level. With raw data files, this function will
join a combined table of NEI data and county boundary, then perform a
spatial join to target locations.

## Usage

``` r
process_nei(path = NULL, county = NULL, year = c(2017, 2020), ...)
```

## Arguments

- path:

  character(1). Directory with NEI csv files.

- county:

  `SpatVector`/`sf`. County boundaries.

- year:

  integer(1) Year to use. Currently only 2017 or 2020 is accepted.

- ...:

  Placeholders.

## Value

a `SpatVector` object

## Note

Base files for `county` argument can be downloaded directly from [U.S.
Census
Bureau](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
or by using `tigris` package. This function does not reproject census
boundaries. Users should be aware of the coordinate system of census
boundary data for other analyses.

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
nei <- process_nei(
  path = "./data",
  county = system.file("gpkg/nc.gpkg", package = "sf"),
  year = 2017
)
} # }
```
