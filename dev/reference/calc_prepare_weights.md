# Prepare optional weighting raster

Prepare optional weighting raster

## Usage

``` r
calc_prepare_weights(from, weights = NULL)
```

## Arguments

- from:

  SpatRaster(1). Template raster.

- weights:

  NULL, SpatRaster, SpatVector/sf polygon, or file path.

## Value

NULL or single-layer SpatRaster aligned to `from`.
