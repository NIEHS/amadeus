# Check input assumptions

Check if all of `"lon"`, `"lat"`, and `"time"` (only if
`check_time = TRUE`) then convert inputs into a `SpatVector` object.

## Usage

``` r
process_conformity(locs = NULL, check_time = FALSE, locs_epsg = "EPSG:4326")
```

## Arguments

- locs:

  Data. [sf](https://r-spatial.github.io/sf/reference/st_as_sf.html),
  [SpatVector](https://rspatial.github.io/terra/reference/vect.html), or
  [data.frame](https://rdrr.io/r/base/data.frame.html)

- check_time:

  logical(1). Whether `"time"` exists in column names.

- locs_epsg:

  character(1). `"{authority}:{code}"` or Well-Known Text format for
  coordinate reference system definition.

## Value

a `SpatVector` object

## Author

Insang Song
