# Collapse listed NLCD values while filling in NA for sites outside data.

Collapse listed NLCD values while filling in NA for sites outside data.

## Usage

``` r
collapse_nlcd(
  data,
  mode = c("terra", "exact"),
  locs = NULL,
  locs_id = "site_id"
)
```

## Arguments

- data:

  Buffered values from NLCD data.

- mode:

  "exact" or "terra"

- locs:

  extraction locations.

- locs_id:

  character(1). Name of unique identifier.
