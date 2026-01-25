# Process MODIS sub-datasets

Selected MODIS sinusoidal grid product subdataset name selector. Four
presets are supported. `custom_sel` supersedes presets of `product`
values.

## Usage

``` r
process_modis_sds(
  product = c("MOD11A1", "MOD13A2", "MOD09GA", "MCD19A2"),
  custom_sel = NULL,
  ...
)
```

## Arguments

- product:

  character(1). Product code.

- custom_sel:

  character(1). Custom filter. If this value is not NULL, preset filter
  is overridden.

- ...:

  Placeholders.

## Value

A character object that conforms to the regular expression. Details of
regular expression in R can be found in
[regexp](https://rdrr.io/r/base/regex.html).

## Note

Preset product codes and associated variables include

- "MOD11A1" - Land surface temperature (LST)

- "MOD13A2" - Normalized Difference Vegetation Index (NDVI)

- "MOD09GA" - Surface reflectance, and

- "MCD19A2" - Aerosol optical depth (AOD).

For a full list of available MODIS product codes, see the "Short Name"
column at [NASA LP DAAC Search Data
Catalog](https://www.earthdata.nasa.gov/centers/lp-daac). When utilizing
a product code from this "Short Name" column, **do not include** the
version number following the period. For example, if "Short Name" =
MCD12C1.006, then `product = "MCD12C1"`.

## See also

[calculate_modis](https://niehs.github.io/amadeus/reference/calculate_modis.md)

## Author

Insang Song

## Examples

``` r
process_modis_sds(product = "MOD09GA")
#> [1] "(sur_refl_b0)"
```
