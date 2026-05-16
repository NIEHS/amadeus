# Set column names

Apply standard column names to calculated covariates consistent with the
requirements of the `beethoven` package. Column names follow fixed
format of 3 character data genre, 2 - 15 character variable code, 1
digit temporal lag, and 5 digit buffer radius (in meters). Variable code
character range is required to retain interpretable column names across
datasets.

## Usage

``` r
calc_setcolumns(from, lag, dataset, locs_id)
```

## Arguments

- from:

  data.frame(1) or SpatVector(1). Calculated covariates as returned from
  `calc_covariates()` or a source specific covariate function.

- lag:

  integer(1). Temporal lag.

- dataset:

  character(1). Covariate parent dataset.

- locs_id:

  character(1). Column containing identifier for each unique coordinate
  location.

## Value

a data.frame or SpatVector object (depending on `from`)

## Note

`beethoven` utilizes point, 1km, and 10km radius buffer distance for
covariate calculation, and therefore the buffer radius column is padded
to 5 digits. If provided a buffer radius greater than 5 digits,
`calc_setcolumns()` will expand to the number of digits. (ie. buffer
radius of 100km = CCC_CCCCC_I_100000).
