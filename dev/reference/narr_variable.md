# Sort NOAA NARR variables

Determine whether a NOAA NARR variable selected for download is a
monolevel or pressure level variable. Monolevel variables are derived
from https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/,
and pressure level variables are derived from
https://downloads.psl.noaa.gov//Datasets/NARR/Dailies/pressure/.

## Usage

``` r
narr_variable(variable)
```

## Arguments

- variable:

  character(1). User-selected NARR variable

## Value

list with URL base and vector of months (blank for monolevel)
