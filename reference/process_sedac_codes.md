# Process population resolution code

Convert full length resolution name to/from resolution code.

## Usage

``` r
process_sedac_codes(string, invert = FALSE)
```

## Arguments

- string:

  character(1). Resolution name or code.

- invert:

  logical(1). Default = FALSE. `invert = TRUE` assumes `string` provides
  resolution code, and returns full length resolution.

## Value

character
