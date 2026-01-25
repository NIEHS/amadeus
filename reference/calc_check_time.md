# Check time values

Check the time values within calculated covariates `data.frame`

## Usage

``` r
calc_check_time(covar, POSIXt = TRUE)
```

## Arguments

- covar:

  data.frame(1). Calculated covariates `data.frame`.

- POSIXt:

  logical(1). Should the time values in `covar` be of class `POSIXt`? If
  `FALSE`, the time values will be checked for integer class (year and
  year-month).

## Value

NULL; returns a stop error if `time` is wrong class
