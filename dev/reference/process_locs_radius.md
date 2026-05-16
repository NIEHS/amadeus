# Process locations buffer

Create circular buffer around locations based on user defined radius.

Creates a circular buffer around points if `radius` is \> 0. Returns
points if `radius` is 0.

## Usage

``` r
process_locs_radius(locs, radius)
```

## Arguments

- locs:

  SpatVector(1). SpatVector object with point geometry

- radius:

  integer(1). Circular buffer size (meters).

## Value

a `SpatVector` object
