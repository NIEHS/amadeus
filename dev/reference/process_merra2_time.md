# Process MERRA2 time steps

Identify the time step of data observations based on MERRA2 collection
and filter to time values in `from`.

## Usage

``` r
process_merra2_time(collection, from)
```

## Arguments

- collection:

  character(1). MERRA2 collection name.

- from:

  SpatRaster(1). Object to extract time values from.

## Value

character
