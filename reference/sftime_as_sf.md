# Convert an `sftime` to an `sf`

Convert an `sftime` object to an `sf` object. `x` must contain a
time-defining column, identified in `timename`.

## Usage

``` r
sftime_as_sf(x, keeptime = TRUE)
```

## Arguments

- x:

  an `sftime` object

- keeptime:

  boolean: TRUE if user wants to keep time column as simple column
  (default = TRUE)

## Value

an `sf` object

## Author

Eva Marques
