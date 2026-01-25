# Process elevation statistic and resolution codes

Identify the GMTED statistic and resolution based on the file path.
Convert statistic and resolution to/from full string to/from statistic
and resolution code.

## Usage

``` r
process_gmted_codes(
  string,
  statistic = FALSE,
  resolution = FALSE,
  invert = FALSE
)
```

## Arguments

- string:

  character(1). File path to GMTED data file.

- statistic:

  logical(1). Matches statistic to statistic code.

- resolution:

  logical(1). Matches resolution to resolution code.

- invert:

  logical(1). Default = FALSE. `invert = TRUE` assumes `string` provides
  statistic or resolution code, and returns full length statistic or
  resolution.

## Value

character
