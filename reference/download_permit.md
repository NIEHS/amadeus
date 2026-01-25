# Check data download acknowledgement

Return an error if the `acknowledgement = FALSE`.

## Usage

``` r
download_permit(acknowledgement)
```

## Arguments

- acknowledgement:

  logical(1). Whether to start downloading

## Value

NULL; returns a stop error if the acknowledgement is FALSE

## Note

The `acknowledgement` parameter is designed to help users avoid
accidentally initiating a very large data download that may take a very
long time to run or exceed machine capabilities.
