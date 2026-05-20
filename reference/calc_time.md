# Prepare time values

Prepare the time values for covariate calculation based on type of time
value.

## Usage

``` r
calc_time(time, format, dataset = NULL, layer_name = NULL, layer_time = NULL)
```

## Arguments

- time:

  Time value

- format:

  Type of time to return in the `$time` column. Can be "timeless" (ie.
  Ecoregions data), "date" (ie. NARR data), "hour" (ie. GEOS data),
  "year" (ie. SEDAC population data), or "yearmonth" (ie. TerraClimate
  data).

## Value

a `Date`, `POSIXt`, or `integer` object based on `format =`
