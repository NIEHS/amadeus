# Convert an `sftime` to a `mysftime`

Convert an `sftime` object to a `mysftime` object. `x` must contain a
time-defining column, identified in `timename`.

## Usage

``` r
sftime_as_mysftime(x, timename)
```

## Arguments

- x:

  an `sftime` object

- timename:

  character: name of time column in `x`

## Value

an `sftime` object with specific format

## See also

[check_mysftime](https://niehs.github.io/amadeus/dev/reference/check_mysftime.md)

## Author

Eva Marques
