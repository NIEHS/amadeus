# Parse netCDF day codes from layer names

Parse day-code suffixes from netCDF layer names such as
`"precipitation_amount_day=43101"` and convert to `Date`.

## Usage

``` r
process_parse_ncdf_day_codes(
  layer_names,
  source = "gridmet",
  origin = "1900-01-01"
)
```

## Arguments

- layer_names:

  character. Layer names.

- source:

  character(1). Source label used in error messages.

- origin:

  character(1). Date origin for numeric day codes.

## Value

Date vector.
