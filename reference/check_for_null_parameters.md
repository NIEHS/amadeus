# Check parameters

Check that all parameters have been assigned a value.

## Usage

``` r
check_for_null_parameters(parameters)
```

## Arguments

- parameters:

  parameters passed to function (called by `mget(ls())`.)

## Value

NULL; returns a stop error if one or more function parameters other than
'extent' are NULL
