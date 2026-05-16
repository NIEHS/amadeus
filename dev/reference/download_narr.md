# Download meteorological data

The `download_narr` function accesses and downloads daily meteorological
data from NOAA's North American Regional Reanalysis (NARR) model via the
NOAA Physical Sciences Laboratory (PSL) NARR Dailies server
(<https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/>).

## Usage

``` r
download_narr(
  variables = NULL,
  year = c(2018, 2022),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- variables:

  character. Variable(s) name acronym. See the *Available NARR
  Variables* section below for the complete list of supported
  abbreviations.

- year:

  integer(1 or 2). Year or start/end years for downloading data.

- directory_to_save:

  character(1). Directory to save downloaded data files.

- acknowledgement:

  logical(1). Must be TRUE to proceed with download.

- download:

  logical(1). DEPRECATED. Downloads happen automatically.

- remove_command:

  logical(1). DEPRECATED, ignored.

- show_progress:

  logical(1). Show download progress (default TRUE)

- hash:

  logical(1). Return hash of downloaded files (default FALSE)

- max_tries:

  integer(1). Maximum download retry attempts (default 20)

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2)

## Value

invisible list with download results; or hash character if hash=TRUE

## Note

"Pressure levels" variables contain variable values at 29 atmospheric
levels, ranging from 1000 hPa to 100 hPa. All pressure levels data will
be downloaded for each variable.

The 88 variables supported by this function represent the complete set
of variables available as individual NetCDF files on the PSL NARR
Dailies server. The NARR archive also contains additional variables
(e.g., cloud water mixing ratio, ice mixing ratio, surface friction
velocity, momentum fluxes, and static land/soil properties) that are
only present in the raw merged GRIB files (`merged_AWIP32.YYYYMMDDHH`)
available at <https://ftp.cpc.ncep.noaa.gov/NARR/>. Those variables
cannot be downloaded with this function.

## Available NARR Variables

The `variables` argument accepts one or more of the following
abbreviations. Variables are grouped into three categories that
determine the source URL path used for download.

**Monolevel variables** (single vertical level, surface / near-surface
fields):

- `acpcp`:

  Convective precipitation

- `air.2m`:

  Air temperature at 2 m

- `air.sfc`:

  Air temperature at surface

- `albedo`:

  Surface albedo

- `apcp`:

  Total accumulated precipitation

- `bgrun`:

  Baseflow-groundwater runoff

- `bmixl.hl1`:

  Blackadar mixing length scale at hybrid level 1

- `cape`:

  Convective available potential energy

- `ccond`:

  Canopy conductance

- `cdcon`:

  Convective cloud cover

- `cdlyr`:

  Non-convective cloud cover

- `cfrzr`:

  Categorical freezing rain

- `cicep`:

  Categorical ice pellets

- `cin`:

  Convective inhibition

- `cnwat`:

  Plant canopy surface water

- `crain`:

  Categorical rain

- `csnow`:

  Categorical snow

- `dlwrf`:

  Downward longwave radiation flux

- `dpt.2m`:

  Dew point temperature at 2 m

- `dswrf`:

  Downward shortwave radiation flux

- `evap`:

  Evaporation

- `gflux`:

  Ground heat flux

- `hcdc`:

  High cloud cover

- `hgt.tropo`:

  Geopotential height at tropopause

- `hlcy`:

  Storm relative helicity

- `hpbl`:

  Planetary boundary layer height

- `lcdc`:

  Low cloud cover

- `lftx4`:

  Best (4-layer) lifted index

- `lhtfl`:

  Latent heat net flux

- `mcdc`:

  Mid-cloud cover

- `mconv.hl1`:

  Horizontal moisture divergence at hybrid level 1

- `mslet`:

  Mean sea level pressure (ETA model reduction)

- `mstav`:

  Moisture availability

- `pevap`:

  Potential evaporation

- `pottmp.hl1`:

  Potential temperature at hybrid level 1

- `pottmp.sfc`:

  Potential temperature at surface

- `prate`:

  Precipitation rate

- `pres.sfc`:

  Surface pressure

- `pres.tropo`:

  Pressure at tropopause

- `prmsl`:

  Pressure reduced to mean sea level

- `pr_wtr`:

  Precipitable water

- `rcq`:

  Specific humidity tendency from all physics

- `rcs`:

  Snowfall water equivalent tendency

- `rcsol`:

  Solar radiative heating rates

- `rct`:

  Temperature tendency from all physics

- `rhum.2m`:

  Relative humidity at 2 m

- `shtfl`:

  Sensible heat net flux

- `shum.2m`:

  Specific humidity at 2 m

- `snod`:

  Snow depth

- `snohf`:

  Snow phase-change heat flux

- `snom`:

  Snow melt

- `snowc`:

  Snow cover

- `soilm`:

  Soil moisture content (0–200 cm layer)

- `ssrun`:

  Storm surface runoff

- `tcdc`:

  Total cloud cover

- `tke.hl1`:

  Turbulent kinetic energy at hybrid level 1

- `ulwrf.ntat`:

  Upward longwave radiation flux at nominal top of atmosphere

- `ulwrf.sfc`:

  Upward longwave radiation flux at surface

- `ustm`:

  U-component of storm motion

- `uswrf.ntat`:

  Upward shortwave radiation flux at nominal top of atmosphere

- `uswrf.sfc`:

  Upward shortwave radiation flux at surface

- `uwnd.10m`:

  U-component of wind at 10 m

- `veg`:

  Vegetation fraction

- `vis`:

  Visibility

- `vstm`:

  V-component of storm motion

- `vvel.hl1`:

  Vertical velocity at hybrid level 1

- `vwnd.10m`:

  V-component of wind at 10 m

- `vwsh.tropo`:

  Vertical wind shear at tropopause

- `wcconv`:

  Convective wetting of vegetation canopy

- `wcinc`:

  Wetting of vegetation canopy

- `wcuflx`:

  U-component of convective canopy moisture flux

- `wcvflx`:

  V-component of convective canopy moisture flux

- `weasd`:

  Water-equivalent accumulated snow depth

- `wvconv`:

  Convective column moisture convergence

- `wvinc`:

  Column moisture increase

- `wvuflx`:

  U-component of vertically-integrated moisture flux

- `wvvflx`:

  V-component of vertically-integrated moisture flux

**Pressure level variables** (29 atmospheric pressure levels from 1000
to 100 hPa; all levels are downloaded together):

- `air`:

  Air temperature

- `hgt`:

  Geopotential height

- `omega`:

  Vertical velocity (pressure / omega)

- `shum`:

  Specific humidity

- `tke`:

  Turbulent kinetic energy

- `uwnd`:

  U-component of wind

- `vwnd`:

  V-component of wind

**Subsurface (soil) variables** (4 soil layers):

- `soill`:

  Liquid volumetric soil moisture (non-frozen fraction)

- `soilw`:

  Volumetric soil moisture content

- `tsoil`:

  Soil temperature

## References

Mesinger F, DiMego G, Kalnay E, Mitchell K, Shafran PC, Ebisuzaki W,
Jović D, Woollen J, Rogers E, Berbery EH, Ek MB, Fan Y, Grumbine R,
Higgins W, Li H, Lin Y, Manikin G, Parrish D, Shi W (2006). “North
American Regional Reanalysis.” *Bulletin of the American Meteorological
Society*, **87**(3), 343–360. ISSN 0003-0007, 1520-0477.
[doi:10.1175/BAMS-87-3-343](https://doi.org/10.1175/BAMS-87-3-343) .

## Author

Mitchell Manware, Insang Song, Kyle Messier

## Examples

``` r
if (FALSE) { # \dontrun{
download_narr(
  variables = c("weasd", "omega"),
  year = 2023,
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)

# Multiple years
download_narr(
  variables = c("air.2m", "rhum.2m"),
  year = c(2020, 2022),
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
