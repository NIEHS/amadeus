# Convert a `data.table` to an `sftime`

Convert a `data.table` object to an `sftime`. `x` must be a `data.table`
object with "lon", "lat", and "time" columns to describe the longitude,
latitude, and time-orientation, respectively, of `x`.

## Usage

``` r
dt_as_mysftime(x, lonname, latname, timename, crs)
```

## Arguments

- x:

  a `data.table`

- lonname:

  character for longitude column name

- latname:

  character for latitude column name

- timename:

  character for time column name

- crs:

  coordinate reference system

## Value

an `sftime` object

## Author

Eva Marques
