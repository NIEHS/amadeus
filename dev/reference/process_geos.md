# Process atmospheric composition data

The `process_geos()` function imports and cleans raw atmospheric
composition data, returning a single `SpatRaster` object.

## Usage

``` r
process_geos(
  date = c("2018-01-01", "2018-01-10"),
  variable = NULL,
  path = NULL,
  extent = NULL,
  daily_agg = FALSE,
  fun = "mean",
  ...
)
```

## Arguments

- date:

  character(1 or 2). Date (1) or start and end dates (2). Format
  YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").

- variable:

  character(1). GEOS-CF variable name(s). See *Notes* for
  collection-specific variable-name guidance.

- path:

  character(1). Directory with downloaded netCDF (.nc4) files.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- daily_agg:

  logical(1). If `TRUE`, aggregate sub-daily layers to daily values
  using `fun`. Default `FALSE` preserves the original hourly output.
  Aggregation groups layers by variable/level and date so that
  pressure-level structure is preserved. Not meaningful for collections
  that are already daily.

- fun:

  character(1). Aggregation function passed to
  [`terra::tapp()`](https://rspatial.github.io/terra/reference/tapp.html)
  (e.g. `"mean"`, `"max"`, `"min"`, `"sum"`). Ignored when
  `daily_agg = FALSE`.

- ...:

  Placeholders.

## Value

a `SpatRaster` object;

## Details

GEOS-CF netCDF collections currently supported by
[`download_geos()`](https://niehs.github.io/amadeus/dev/reference/download_geos.md)
are: `"aqc_tavg_1hr_g1440x721_v1"`, `"chm_tavg_1hr_g1440x721_v1"`,
`"met_tavg_1hr_g1440x721_x1"`, `"xgc_tavg_1hr_g1440x721_x1"`,
`"chm_inst_1hr_g1440x721_p23"`, and `"met_inst_1hr_g1440x721_p23"`.

## Note

Layer names of the returned `SpatRaster` object contain the variable,
pressure level, date, and hour when `daily_agg = FALSE` (default). When
`daily_agg = TRUE`, layer names contain the variable, pressure level,
and date only, and
[`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
is set to midnight UTC of each date.

Collection-specific variable names accepted by `variable`:

|  |  |
|----|----|
| **Collection** | **Variables** |
| `aqc_tavg_1hr_g1440x721_v1` | `no2`, `co`, `so2`, `pm25_rh35_gcc`, `o3` |
| `chm_tavg_1hr_g1440x721_v1` | `ocpi`, `bcpo`, `pm25soa_rh35_gc`, `dst4`, `prpe`, `macr`, `pm25ss_rh35_gcc`, `hno4`, `ch4`, `nh3`, `h2o2`, `rcho`, `hno3`, `dst1`, `pan`, `pm25oc_rh35_gcc`, `c3h8`, `soas`, `no`, `tolu`, `mvk`, `xyle`, `isop`, `noy`, `sala`, `so2`, `co`, `n2o5`, `eoh`, `o3`, `acet`, `c2h6`, `mek`, `nit`, `benz`, `soap`, `alk4`, `ocpo`, `ald2`, `hcho`, `pm25_rh35_gocar`, `dst3`, `pm25su_rh35_gcc`, `pm25_rh35_gcc`, `pm25ni_rh35_gcc`, `pm25bc_rh35_gcc`, `dst2`, `pm25du_rh35_gcc`, `bcpi`, `no2`, `salc`, `nh4` |
| `met_tavg_1hr_g1440x721_x1` | `zl`, `zpbl`, `ps`, `v2m`, `v`, `q2m`, `u`, `t2m`, `troppb`, `q`, `t`, `v10m`, `t10m`, `u2m`, `q10m`, `ts`, `slp`, `cldtt`, `phis`, `tprec`, `u10m`, `rh` |
| `xgc_tavg_1hr_g1440x721_x1` | `wetdepflx_nh4`, `aod550_dst6`, `wetdepflx_dst1`, `tropcol_io`, `totcol_o3`, `tropcol_hcho`, `drydepflx_bcpi`, `aod550_cloud`, `aod550_dst5`, `wetdepflx_hcho`, `aod550_salc`, `aod550_dust`, `wetdepflx_so2`, `wetdepflx_salc`, `wetdepflx_dst3`, `drydepflx_nit`, `wetdepflx_so4`, `aod550_sala`, `aod550_dst1`, `tropcol_co`, `wetdepflx_bcpi`, `drydepflx_sala`, `wetdepflx_nh3`, `tropcol_no2`, `wetdepflx_nit`, `aod550_sulfate`, `wetdepflx_ocpi`, `drydepflx_hcho`, `drydepflx_dst4`, `tropcol_so2`, `drydepflx_ocpi`, `tropcol_o3`, `drydepflx_nh4`, `aod550_dst7`, `totcol_co`, `totcol_so2`, `totcol_io`, `drydepflx_nh3`, `wetdepflx_sala`, `wetdepflx_dst4`, `drydepflx_o3`, `drydepflx_hno3`, `aod550_dst4`, `aod550_oc`, `totcol_no2`, `drydepflx_dst2`, `tropcol_bro`, `wetdepflx_bcpo`, `drydepflx_bcpo`, `wetdepflx_dst2`, `drydepflx_dst1`, `aod550_dst2`, `aod550_bc`, `aod550_dst3`, `wetdepflx_ocpo`, `drydepflx_dst3`, `drydepflx_salc`, `wetdepflx_hno3`, `drydepflx_ocpo`, `drydepflx_no2`, `totcol_hcho`, `totcol_bro` |
| `chm_inst_1hr_g1440x721_p23` | `pm25soa_rh35_gc`, `pm25ss_rh35_gcc`, `so2`, `co`, `o3`, `pm25oc_rh35_gcc`, `pm25du_rh35_gcc`, `noy`, `no2`, `pm25ni_rh35_gcc`, `pm25bc_rh35_gcc`, `pm25_rh35_gcc`, `pm25su_rh35_gcc` |
| `met_inst_1hr_g1440x721_p23` | `omega`, `t`, `eth`, `q`, `epv`, `rh`, `slp`, `airdens`, `ps`, `h`, `th`, `v`, `u`, `airvol_chem` |

`variable` matching is case-insensitive (for example, `"o3"` matches
`"O3"`).

Reference: NASA GEOS-CF OpenDAP catalog
<https://opendap.nccs.nasa.gov/dods/gmao/geos-cf/assim>.

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
geos <- process_geos(
  date = c("2024-01-01", "2024-01-10"),
  variable = "O3",
  path = "./data/aqc_tavg_1hr_g1440x721_v1"
)
## daily mean across all sub-daily layers per variable/level
geos_daily <- process_geos(
  date = c("2024-01-01", "2024-01-10"),
  variable = "O3",
  path = "./data/aqc_tavg_1hr_g1440x721_v1",
  daily_agg = TRUE,
  fun = "mean"
)
} # }
```
