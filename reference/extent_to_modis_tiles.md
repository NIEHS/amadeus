# Convert spatial extent to MODIS sinusoidal tile codes

Returns the set of MODIS sinusoidal grid tile codes (e.g. `"h08v04"`)
whose geographic footprint overlaps the supplied bounding box.

## Usage

``` r
extent_to_modis_tiles(extent)
```

## Arguments

- extent:

  numeric(4). Bounding box `c(xmin, ymin, xmax, ymax)` in decimal
  degrees (EPSG:4326).

## Value

character vector of tile codes in `"hXXvYY"` format, ordered by
increasing v then h.

## Details

The MODIS sinusoidal grid divides the globe into 18 x 36 tiles, each
nominally covering 10 degrees of latitude. Because the sinusoidal
projection compresses longitude at high latitudes, the geographic
lon/lat bounding boxes of tiles are *not* simple 10-degree squares —
they can be significantly wider in geographic longitude near the poles.

This function uses the official NASA MODLAND sinusoidal tile bounding
coordinates table (`sn_bound_10deg.txt`,
<https://modis-land.gsfc.nasa.gov/pdf/sn_bound_10deg.txt>) bundled in
`inst/extdata/`. It returns every non-fill tile whose geographic
bounding box overlaps the requested extent.

Horizontal tile numbers (h) range from 0 to 35 (west to east); vertical
tile numbers (v) range from 0 to 17 (north to south).

## See also

[`download_modis`](https://niehs.github.io/amadeus/reference/download_modis.md)

## Author

Kyle Messier

## Examples

``` r
extent_to_modis_tiles(c(-125, 22, -64, 50))
#>  [1] "h09v03" "h10v03" "h11v03" "h12v03" "h13v03" "h14v03" "h08v04" "h09v04"
#>  [9] "h10v04" "h11v04" "h12v04" "h13v04" "h07v05" "h08v05" "h09v05" "h10v05"
#> [17] "h11v05" "h12v05" "h13v05" "h06v06" "h07v06" "h08v06" "h09v06" "h10v06"
#> [25] "h11v06" "h12v06"
```
