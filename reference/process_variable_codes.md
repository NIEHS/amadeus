# Filter gridMET and terraClimate variable names and variable codes

Check user defined variables for gridMET and TerraClimate functions.

## Usage

``` r
process_variable_codes(variables, source = c("gridmet", "terraclimate"))
```

## Arguments

- variables:

  character(1). Data variables. (Passed from download\_\* or
  process\_\*).

- source:

  character(1). Data source for selected variables ("gridMET" or
  "TerraClimate").

## Value

character
