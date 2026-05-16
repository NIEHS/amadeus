# Process GEOS-CF and MERRA2 collection codes

Identify the GEOS-CF or MERRA2 collection based on the file path.

## Usage

``` r
process_collection(
  path,
  source,
  collection = FALSE,
  date = FALSE,
  datetime = FALSE
)
```

## Arguments

- path:

  character(1). File path to data file.

- source:

  character(1). "geos" for GEOS-CF or "merra2" for MERRA2

- collection:

  logical(1). Identifies and returns collection name(s) based on
  provided file path(s).

- date:

  logical(1). Identifies and returns date sequence (YYYYMMDD) based on
  provided file path(s).

- datetime:

  logical(1). Identifies and returns date time sequence
  (YYYYMoMoDDHHMiMi) based on provided file path(s).

## Value

character
