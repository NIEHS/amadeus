# Process Black Marble corners

Tile corner generator for Black Marble products.

Black Marble products are in HDF5 format and are read without
georeference with typical R geospatial packages. This function generates
a `data.frame` of corner coordinates for assignment.

## Usage

``` r
process_blackmarble_corners(hrange = c(5, 11), vrange = c(3, 6))
```

## Arguments

- hrange:

  integer(2). Both should be in 0-35.

- vrange:

  integer(2). Both should be in 0-17.

## Value

`data.frame` with xmin, xmax, ymin, and ymax fields

## References

- [Wang, Z. (2022). Black Marble User Guide (Version 1.3).
  NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)

## Author

Insang Song

## Examples

``` r
process_blackmarble_corners(hrange = c(1, 2), vrange = c(1, 2))
#>     tile xmin xmax ymin ymax
#> 1 h01v01 -170 -160   70   80
#> 2 h01v02 -170 -160   60   70
#> 3 h02v01 -160 -150   70   80
#> 4 h02v02 -160 -150   60   70
```
