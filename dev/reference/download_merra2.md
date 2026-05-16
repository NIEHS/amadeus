# Download meteorological and atmospheric data

The `download_merra2()` function accesses and downloads various
meteorological and atmospheric collections from [NASA's Modern-Era
Retrospective analysis for Research and Applications, Version 2
(MERRA-2) model](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/), and
the daily corrected Global Fire Weather Index (FWI) product derived from
MERRA-2 weather inputs.

## Usage

``` r
download_merra2(
  collection = c("inst1_2d_asm_Nx", "inst1_2d_int_Nx", "inst1_2d_lfo_Nx",
    "inst3_3d_asm_Np", "inst3_3d_aer_Nv", "inst3_3d_asm_Nv", "inst3_3d_chm_Nv",
    "inst3_3d_gas_Nv", "inst3_2d_gas_Nx", "inst6_3d_ana_Np", "inst6_3d_ana_Nv",
    "statD_2d_slv_Nx", "tavg1_2d_adg_Nx", "tavg1_2d_aer_Nx", "tavg1_2d_chm_Nx",
    "tavg1_2d_csp_Nx", "tavg1_2d_flx_Nx", "tavg1_2d_int_Nx", "tavg1_2d_lfo_Nx",
    "tavg1_2d_lnd_Nx", "tavg1_2d_ocn_Nx", "tavg1_2d_rad_Nx", "tavg1_2d_slv_Nx",
    "tavg3_3d_mst_Ne", "tavg3_3d_trb_Ne", "tavg3_3d_nav_Ne", "tavg3_3d_cld_Np", 
    
    "tavg3_3d_mst_Np", "tavg3_3d_rad_Np", "tavg3_3d_tdt_Np", "tavg3_3d_trb_Np",
    "tavg3_3d_udt_Np", "tavg3_3d_odt_Np", "tavg3_3d_qdt_Np", "tavg3_3d_asm_Nv",
    "tavg3_3d_cld_Nv", "tavg3_3d_mst_Nv", "tavg3_3d_rad_Nv", "tavg3_2d_glc_Nx", "fwi"),
  nasa_earth_data_token = NULL,
  date = c("2018-01-01", "2018-01-01"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- collection:

  character(1). MERRA-2 data collection file name, or `"fwi"` for the
  daily corrected Global Fire Weather Index product
  (`MERRA2.CORRECTED`).

- nasa_earth_data_token:

  character(1) or NULL. NASA EarthData authentication token.

- date:

  character(1 or 2). length of 10. Date or start/end dates for
  downloading data. Format "YYYY-MM-DD" (ex. January 1, 2018 =
  `"2018-01-01"`).

- directory_to_save:

  character(1). Directory to save data.

- acknowledgement:

  logical(1). By setting `TRUE` the user acknowledges that the data
  downloaded using this function may be very large and use lots of
  machine storage and memory.

- download:

  logical(1). DEPRECATED. Downloads happen automatically.

- remove_command:

  logical(1). Deprecated, ignored.

- hash:

  logical(1). By setting `TRUE` the function will return an
  [`rlang::hash_file()`](https://rlang.r-lib.org/reference/hash.html)
  hash character corresponding to the downloaded files. Default is
  `FALSE`.

- show_progress:

  logical(1). Show download progress (default TRUE)

- max_tries:

  integer(1). Maximum retry attempts (default 20)

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2)

## Value

invisible list with download results; or hash character if hash=TRUE

## Note

Due to NASA data access policies, standard MERRA-2 GES DISC downloads
require a valid NASA Earthdata token for authentication. Use
[`setup_nasa_token()`](https://niehs.github.io/amadeus/dev/reference/setup_nasa_token.md)
for setup. The `"fwi"` collection is hosted on the public GlobalFWI
portal and does not require Earthdata authentication.

## Author

Mitchell Manware, Insang Song, Kyle Messier

## Examples

``` r
if (FALSE) { # \dontrun{
download_merra2(
  collection = "inst1_2d_int_Nx",
  date = "2024-01-01",
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
