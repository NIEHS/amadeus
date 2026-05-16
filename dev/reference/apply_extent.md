# Apply extent to the processed data

User-defined extent is used to filter the data.

## Usage

``` r
apply_extent(data, extent, geom)
```

## Arguments

- data:

  sf/terra object.

- extent:

  numeric(4). Extent to filter the data. Should be ordered as c(xmin,
  xmax, ymin, ymax).

- geom:

  character(1 or 2). Geometry type for if `data` is `data.frame`. One of
  "geometry" or c("lon", "lat").

## Value

sf/terra object with the extent applied.
