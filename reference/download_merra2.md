# Download meteorological and atmospheric data

The `download_merra2()` function accesses and downloads various
meteorological and atmospheric collections from [NASA's Modern-Era
Retrospective analysis for Research and Applications, Version 2
(MERRA-2) model](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/).

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
    "tavg3_3d_cld_Nv", "tavg3_3d_mst_Nv", "tavg3_3d_rad_Nv", "tavg3_2d_glc_Nx"),
  date = c("2018-01-01", "2018-01-01"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  hash = FALSE
)
```

## Arguments

- collection:

  character(1). MERRA-2 data collection file name.

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

  logical(1). `FALSE` will generate a \*.txt file containing all
  download commands. By setting `TRUE` the function will download all of
  the requested data files.

- remove_command:

  logical(1). Remove (`TRUE`) or keep (`FALSE`).

- hash:

  logical(1). By setting `TRUE` the function will return an
  [`rlang::hash_file()`](https://rlang.r-lib.org/reference/hash.html)
  hash character corresponding to the downloaded files. Default is
  `FALSE`. the text file containing download commands.

## Value

- For `hash = FALSE`, NULL

- For `hash = TRUE`, an
  [`rlang::hash_file`](https://rlang.r-lib.org/reference/hash.html)
  character.

- netCDF (.nc4) files will be stored in a collection-specific folder
  within `directory_to_save`.

## References

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
inst1_2d\_ asm\_ Nx:
2d,3-Hourly,Instantaneous,Single-Level,Assimilation,Single-Level
Diagnostics V5.12.4.”
[doi:10.5067/3Z173KIE2TPD](https://doi.org/10.5067/3Z173KIE2TPD) ,
<https://disc.gsfc.nasa.gov/datasets/M2I1NXASM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
inst1_2d\_ int\_ Nx:
2d,1-Hourly,Instantaneous,Single-Level,Assimilation,Vertically
Integrated Diagnostics V5.12.4.”
[doi:10.5067/G0U6NGQ3BLE0](https://doi.org/10.5067/G0U6NGQ3BLE0) ,
<https://disc.gsfc.nasa.gov/datasets/M2I1NXINT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
inst1_2d\_ lfo\_ Nx:
2d,1-Hourly,Instantaneous,Single-Level,Assimilation,Land Surface
Forcings V5.12.4.”
[doi:10.5067/RCMZA6TL70BG](https://doi.org/10.5067/RCMZA6TL70BG) ,
<https://disc.gsfc.nasa.gov/datasets/M2I1NXLFO_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
inst3_3d\_ asm\_ Np:
3d,3-Hourly,Instantaneous,Pressure-Level,Assimilation,Assimilated
Meteorological Fields V5.12.4.”
[doi:10.5067/QBZ6MG944HW0](https://doi.org/10.5067/QBZ6MG944HW0) ,
<https://disc.gsfc.nasa.gov/datasets/M2I3NPASM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
inst3_3d\_ aer\_ Nv:
3d,3-Hourly,Instantaneous,Model-Level,Assimilation,Aerosol Mixing Ratio
V5.12.4.”
[doi:10.5067/LTVB4GPCOTK2](https://doi.org/10.5067/LTVB4GPCOTK2) ,
<https://disc.gsfc.nasa.gov/datasets/M2I3NVAER_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
inst3_3d\_ asm\_ Nv:
3d,3-Hourly,Instantaneous,Model-Level,Assimilation,Assimilated
Meteorological Fields V5.12.4.”
[doi:10.5067/WWQSXQ8IVFW8](https://doi.org/10.5067/WWQSXQ8IVFW8) ,
<https://disc.gsfc.nasa.gov/datasets/M2I3NVASM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
inst3_3d\_ chm\_ Nv:
3d,3-Hourly,Instantaneous,Model-Level,Assimilation,Carbon Monoxide and
Ozone Mixing Ratio V5.12.4.”
[doi:10.5067/HO9OVZWF3KW2](https://doi.org/10.5067/HO9OVZWF3KW2) ,
<https://disc.gsfc.nasa.gov/datasets/M2I3NVCHM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
inst3_3d\_ gas\_ Nv:
3d,3-Hourly,Instantaneous,Model-Level,Assimilation,Aerosol Mixing Ratio
Analysis Increments V5.12.4.”
[doi:10.5067/96BUID8HGGX5](https://doi.org/10.5067/96BUID8HGGX5) ,
<https://disc.gsfc.nasa.gov/datasets/M2I3NVGAS_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
inst3_2d\_ gas\_ Nx:
2d,3-Hourly,Instantaneous,Single-Level,Assimilation,Aerosol Optical
Depth Analysis V5.12.4.”
[doi:10.5067/HNGA0EWW0R09](https://doi.org/10.5067/HNGA0EWW0R09) ,
<https://disc.gsfc.nasa.gov/datasets/M2I3NXGAS_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
inst6_3d\_ ana\_ Np:
3d,6-Hourly,Instantaneous,Pressure-Level,Analysis,Analyzed
Meteorological Fields V5.12.4.”
[doi:10.5067/A7S6XP56VZWS](https://doi.org/10.5067/A7S6XP56VZWS) ,
<https://disc.gsfc.nasa.gov/datasets/M2I6NPANA_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
inst6_3d\_ ana\_ Nv:
3d,6-Hourly,Instantaneous,Model-Level,Analysis,Analyzed Meteorological
Fields V5.12.4.”
[doi:10.5067/IUUF4WB9FT4W](https://doi.org/10.5067/IUUF4WB9FT4W) ,
<https://disc.gsfc.nasa.gov/datasets/M2I6NVANA_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
statD_2d\_ slv\_ Nx: 2d,Monthly,Aggregated
Statistics,Single-Level,Assimilation,Single-Level Diagnostics V5.12.4.”
[doi:10.5067/KVIMOMCUO83U](https://doi.org/10.5067/KVIMOMCUO83U) ,
<https://disc.gsfc.nasa.gov/datasets/M2SMNXSLV_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
statD_2d\_ slv\_ Nx: 2d,Daily,Aggregated
Statistics,Single-Level,Assimilation,Single-Level Diagnostics V5.12.4.”
[doi:10.5067/9SC1VNTWGWV3](https://doi.org/10.5067/9SC1VNTWGWV3) ,
<https://disc.gsfc.nasa.gov/datasets/M2SDNXSLV_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg1_2d\_ adg\_ Nx:
2d,3-Hourly,Time-averaged,Single-Level,Assimilation,Aerosol Diagnostics
(extended) V5.12.4.”
[doi:10.5067/HM00OHQBHKTP](https://doi.org/10.5067/HM00OHQBHKTP) ,
<https://disc.gsfc.nasa.gov/datasets/M2T1NXADG_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg1_2d\_ aer\_ Nx:
2d,1-Hourly,Time-averaged,Single-Level,Assimilation,Aerosol Diagnostics
V5.12.4.”
[doi:10.5067/KLICLTZ8EM9D](https://doi.org/10.5067/KLICLTZ8EM9D) ,
<https://disc.gsfc.nasa.gov/datasets/M2T1NXAER_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg1_2d\_ chm\_ Nx:
2d,3-Hourly,Time-Averaged,Single-Level,Assimilation,Carbon Monoxide and
Ozone Diagnostics V5.12.4.”
[doi:10.5067/3RQ5YS674DGQ](https://doi.org/10.5067/3RQ5YS674DGQ) ,
<https://disc.gsfc.nasa.gov/datasets/M2T1NXCHM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg1_2d\_ csp\_ Nx:
2d,1-Hourly,Time-averaged,Single-Level,Assimilation,COSP Satellite
Simulator V5.12.4.”
[doi:10.5067/H0VVAD8F6MX5](https://doi.org/10.5067/H0VVAD8F6MX5) ,
<https://disc.gsfc.nasa.gov/datasets/M2T1NXCSP_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg1_2d\_ flx\_ Nx:
2d,1-Hourly,Time-Averaged,Single-Level,Assimilation,Surface Flux
Diagnostics V5.12.4.”
[doi:10.5067/7MCPBJ41Y0K6](https://doi.org/10.5067/7MCPBJ41Y0K6) ,
<https://disc.gsfc.nasa.gov/datasets/M2T1NXFLX_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg1_2d\_ int\_ Nx:
2d,1-Hourly,Time-averaged,Single-Level,Assimilation,Vertically
Integrated Diagnostics V5.12.4.”
[doi:10.5067/Q5GVUVUIVGO7](https://doi.org/10.5067/Q5GVUVUIVGO7) ,
<https://disc.gsfc.nasa.gov/datasets/M2T1NXINT_5.12.4/summary>.

Pawson S (2020). “MERRA-2 tavg1_2d\_ lfo\_ Nx:
2d,1-Hourly,Time-Averaged,Single-Level,Assimilation,Land Surface
Forcings V5.12.4.”
[doi:10.5067/L0T5GEG1NYFA](https://doi.org/10.5067/L0T5GEG1NYFA) ,
<https://disc.gsfc.nasa.gov/datasets/M2T1NXLFO_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg1_2d\_ lnd\_ Nx:
2d,1-Hourly,Time-Averaged,Single-Level,Assimilation,Land Surface
Diagnostics V5.12.4.”
[doi:10.5067/RKPHT8KC1Y1T](https://doi.org/10.5067/RKPHT8KC1Y1T) ,
<https://disc.gsfc.nasa.gov/datasets/M2T1NXLND_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg1_2d\_ ocn\_ Nx:
2d,1-Hourly,Time-Averaged,Single-Level,Assimilation,Ocean Surface
Diagnostics V5.12.4.”
[doi:10.5067/Y67YQ1L3ZZ4R](https://doi.org/10.5067/Y67YQ1L3ZZ4R) ,
<https://disc.gsfc.nasa.gov/datasets/M2T1NXOCN_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg1_2d\_ rad\_ Nx:
2d,1-Hourly,Time-Averaged,Single-Level,Assimilation,Radiation
Diagnostics V5.12.4.”
[doi:10.5067/Q9QMY5PBNV1T](https://doi.org/10.5067/Q9QMY5PBNV1T) ,
<https://disc.gsfc.nasa.gov/datasets/M2T1NXRAD_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg1_2d\_ slv\_ Nx:
2d,1-Hourly,Time-Averaged,Single-Level,Assimilation,Single-Level
Diagnostics V5.12.4.”
[doi:10.5067/VJAFPLI1CSIV](https://doi.org/10.5067/VJAFPLI1CSIV) ,
<https://disc.gsfc.nasa.gov/datasets/M2T1NXSLV_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ mst\_ Ne: 3d,3-Hourly,Time-Averaged,Model-Level
Edge,Assimilation,Moist Processes Diagnostics V5.12.4.”
[doi:10.5067/JRUZ3SJ3ZJ72](https://doi.org/10.5067/JRUZ3SJ3ZJ72) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NEMST_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ trb\_ Ne: 3d,3-Hourly,Time-Averaged,Model-Level
Edge,Assimilation,Turbulence Diagnostics V5.12.4.”
[doi:10.5067/4I7ZI35QRH8K](https://doi.org/10.5067/4I7ZI35QRH8K) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NETRB_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ nav\_ Ne: 3d,3-Hourly,Time-Averaged, Vertical Coordinates
V5.12.4.”
[doi:10.5067/N5WAKNS1UYQN](https://doi.org/10.5067/N5WAKNS1UYQN) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NENAV_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ cld\_ Np:
3d,3-Hourly,Time-Averaged,Pressure-Level,Assimilation,Cloud Diagnostics
V5.12.4.”
[doi:10.5067/TX10URJSKT53](https://doi.org/10.5067/TX10URJSKT53) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NPCLD_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ mst\_ Np:
3d,3-Hourly,Time-Averaged,Pressure-Level,Assimilation,Moist Processes
Diagnostics V5.12.4.”
[doi:10.5067/0TUFO90Q2PMS](https://doi.org/10.5067/0TUFO90Q2PMS) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NPMST_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ rad\_ Np:
3d,3-Hourly,Time-Averaged,Pressure-Level,Assimilation,Radiation
Diagnostics V5.12.4.”
[doi:10.5067/3UGE8WQXZAOK](https://doi.org/10.5067/3UGE8WQXZAOK) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NPRAD_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ tdt\_ Np:
3d,3-Hourly,Time-Averaged,Pressure-Level,Assimilation,Temperature
Tendencies V5.12.4.”
[doi:10.5067/9NCR9DDDOPFI](https://doi.org/10.5067/9NCR9DDDOPFI) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NPTDT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ trb\_ Np:
3d,3-Hourly,Time-Averaged,Pressure-Level,Assimilation,Turbulence
Diagnostics V5.12.4.”
[doi:10.5067/ZRRJPGWL8AVL](https://doi.org/10.5067/ZRRJPGWL8AVL) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NPTRB_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ udt\_ Np:
3d,3-Hourly,Time-Averaged,Pressure-Level,Assimilation,Wind Tendencies
V5.12.4.”
[doi:10.5067/CWV0G3PPPWFW](https://doi.org/10.5067/CWV0G3PPPWFW) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NPUDT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ odt\_ Np:
3d,3-Hourly,Time-Averaged,Pressure-Level,Assimilation,Ozone Tendencies
V5.12.4.”
[doi:10.5067/S0LYTK57786Z](https://doi.org/10.5067/S0LYTK57786Z) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NPODT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ qdt\_ Np:
3d,3-Hourly,Time-Averaged,Pressure-Level,Assimilation,Moist Tendencies
V5.12.4.”
[doi:10.5067/A9KWADY78YHQ](https://doi.org/10.5067/A9KWADY78YHQ) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NPQDT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ asm\_ Nv:
3d,3-Hourly,Time-Averaged,Model-Level,Assimilation,Assimilated
Meteorological Fields V5.12.4.”
[doi:10.5067/SUOQESM06LPK](https://doi.org/10.5067/SUOQESM06LPK) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NVASM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ cld\_ Nv:
3d,3-Hourly,Time-Averaged,Model-Level,Assimilation,Cloud Diagnostics
V5.12.4.”
[doi:10.5067/F9353J0FAHIH](https://doi.org/10.5067/F9353J0FAHIH) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NVCLD_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ mst\_ Nv:
3d,3-Hourly,Time-Averaged,Model-Level,Assimilation,Moist Processes
Diagnostics V5.12.4.”
[doi:10.5067/ZXTJ28TQR1TR](https://doi.org/10.5067/ZXTJ28TQR1TR) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NVMST_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_3d\_ rad\_ Nv:
3d,3-Hourly,Time-Averaged,Model-Level,Assimilation,Radiation Diagnostics
V5.12.4.”
[doi:10.5067/7GFQKO1T43RW](https://doi.org/10.5067/7GFQKO1T43RW) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NVRAD_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavg3_2d\_ glc\_ Nx:
2d,3-Hourly,Time-Averaged,Single-Level,Assimilation,Land Ice Surface
Diagnostics V5.12.4.”
[doi:10.5067/9ETB4TT5J6US](https://doi.org/10.5067/9ETB4TT5J6US) ,
<https://disc.gsfc.nasa.gov/datasets/M2T3NXGLC_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instM_2d\_ asm\_ Nx: 2d,Monthly
mean,Single-Level,Assimilation,Single-Level Diagnostics V5.12.4.”
[doi:10.5067/5ESKGQTZG7FO](https://doi.org/10.5067/5ESKGQTZG7FO) ,
<https://disc.gsfc.nasa.gov/datasets/M2IMNXASM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instM_2d\_ int\_ Nx: 2d,Monthly
mean,Instantaneous,Single-Level,Assimilation,Vertically Integrated
Diagnostics V5.12.4.”
[doi:10.5067/KVTU1A8BWFSJ](https://doi.org/10.5067/KVTU1A8BWFSJ) ,
<https://disc.gsfc.nasa.gov/datasets/M2IMNXINT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instM_2d\_ lfo\_ Nx: 2d,Monthly
mean,Instantaneous,Single-Level,Assimilation,Land Surface Forcings
V5.12.4.”
[doi:10.5067/11F99Y6TXN99](https://doi.org/10.5067/11F99Y6TXN99) ,
<https://disc.gsfc.nasa.gov/datasets/M2IMNXLFO_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instM_2d\_ gas\_ Nx: 2d,Monthly
mean,Instantaneous,Single-Level,Assimilation,Aerosol Optical Depth
Analysis V5.12.4.”
[doi:10.5067/XOGNBQEPLUC5](https://doi.org/10.5067/XOGNBQEPLUC5) ,
<https://disc.gsfc.nasa.gov/datasets/M2IMNXGAS_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instM_3d\_ asm\_ Np: 3d,Monthly
mean,Instantaneous,Pressure-Level,Assimilation,Assimilated
Meteorological Fields V5.12.4.”
[doi:10.5067/2E096JV59PK7](https://doi.org/10.5067/2E096JV59PK7) ,
<https://disc.gsfc.nasa.gov/datasets/M2IMNPASM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instM_3d\_ ana\_ Np: 3d,Monthly
mean,Instantaneous,Pressure-Level,Analysis,Analyzed Meteorological
Fields V5.12.4.”
[doi:10.5067/V92O8XZ30XBI](https://doi.org/10.5067/V92O8XZ30XBI) ,
<https://disc.gsfc.nasa.gov/datasets/M2IMNPANA_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ adg\_ Nx: 2d,Monthly
mean,Time-averaged,Single-Level,Assimilation,Aerosol Diagnostics
(extended) V5.12.4.”
[doi:10.5067/RZIK2TV7PP38](https://doi.org/10.5067/RZIK2TV7PP38) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXADG_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ aer\_ Nx: 2d,Monthly
mean,Time-averaged,Single-Level,Assimilation,Aerosol Diagnostics
V5.12.4.”
[doi:10.5067/FH9A0MLJPC7N](https://doi.org/10.5067/FH9A0MLJPC7N) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXAER_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ chm\_ Nx: 2d,Monthly
mean,Time-Averaged,Single-Level,Assimilation,Carbon Monoxide and Ozone
Diagnostics V5.12.4.”
[doi:10.5067/WMT31RKEXK8I](https://doi.org/10.5067/WMT31RKEXK8I) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXCHM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ csp\_ Nx: 2d,Monthly
mean,Time-averaged,Single-Level,Assimilation,COSP Satellite Simulator
V5.12.4.”
[doi:10.5067/BZPOTGJOQKLU](https://doi.org/10.5067/BZPOTGJOQKLU) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXCSP_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ flx\_ Nx: 2d,Monthly
mean,Time-Averaged,Single-Level,Assimilation,Surface Flux Diagnostics
V5.12.4.”
[doi:10.5067/0JRLVL8YV2Y4](https://doi.org/10.5067/0JRLVL8YV2Y4) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXFLX_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ int\_ Nx: 2d,Monthly
mean,Time-Averaged,Single-Level,Assimilation,Vertically Integrated
Diagnostics V5.12.4.”
[doi:10.5067/FQPTQ4OJ22TL](https://doi.org/10.5067/FQPTQ4OJ22TL) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXINT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ lfo\_ Nx: 2d,Monthly
mean,Time-Averaged,Single-Level,Assimilation,Land Surface Forcings
V5.12.4.”
[doi:10.5067/5V7K6LJD44SY](https://doi.org/10.5067/5V7K6LJD44SY) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXLFO_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ lnd\_ Nx: 2d,Monthly
mean,Time-Averaged,Single-Level,Assimilation,Land Surface Diagnostics
V5.12.4.”
[doi:10.5067/8S35XF81C28F](https://doi.org/10.5067/8S35XF81C28F) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXLND_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ ocn\_ Nx: 2d,Monthly
mean,Time-Averaged,Single-Level,Assimilation,Ocean Surface Diagnostics
V5.12.4.”
[doi:10.5067/4IASLIDL8EEC](https://doi.org/10.5067/4IASLIDL8EEC) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXOCN_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ rad\_ Nx: 2d,Monthly
mean,Time-Averaged,Single-Level,Assimilation,Radiation Diagnostics
V5.12.4.”
[doi:10.5067/OU3HJDS973O0](https://doi.org/10.5067/OU3HJDS973O0) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXRAD_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ slv\_ Nx: 2d,Monthly
mean,Time-Averaged,Single-Level,Assimilation,Single-Level Diagnostics
V5.12.4.”
[doi:10.5067/AP1B0BA5PD2K](https://doi.org/10.5067/AP1B0BA5PD2K) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXSLV_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_2d\_ glc\_ Nx: 2d,Monthly
mean,Time-Averaged,Single-Level,Assimilation,Land Ice Surface
Diagnostics V5.12.4.”
[doi:10.5067/5W8Q3I9WUFGX](https://doi.org/10.5067/5W8Q3I9WUFGX) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNXGLC_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_3d\_ cld\_ Np: 3d,Monthly
mean,Time-Averaged,Pressure-Level,Assimilation,Cloud Diagnostics
V5.12.4.”
[doi:10.5067/J9R0LXGH48JR](https://doi.org/10.5067/J9R0LXGH48JR) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNPCLD_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_3d\_ mst\_ Np: 3d,Monthly
mean,Time-Averaged,Pressure-Level,Assimilation,Moist Processes
Diagnostics V5.12.4.”
[doi:10.5067/ZRZGD0DCK1CG](https://doi.org/10.5067/ZRZGD0DCK1CG) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNPMST_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_3d\_ rad\_ Np: 3d,Monthly
mean,Time-Averaged,Pressure-Level,Assimilation,Radiation Diagnostics
V5.12.4.”
[doi:10.5067/H3YGROBVBGFJ](https://doi.org/10.5067/H3YGROBVBGFJ) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNPRAD_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_3d\_ tdt\_ Np: 3d,Monthly
mean,Time-Averaged,Pressure-Level,Assimilation,Temperature Tendencies
V5.12.4.”
[doi:10.5067/VILT59HI2MOY](https://doi.org/10.5067/VILT59HI2MOY) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNPTDT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_3d\_ trb\_ Np: 3d,Monthly
mean,Time-Averaged,Pressure-Level,Assimilation,Turbulence Diagnostics
V5.12.4.”
[doi:10.5067/2YOIQB5C3ACN](https://doi.org/10.5067/2YOIQB5C3ACN) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNPTRB_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_3d\_ udt\_ Np: 3d,Monthly
mean,Time-Averaged,Pressure-Level,Assimilation,Wind Tendencies V5.12.4.”
[doi:10.5067/YSR6IA5057XX](https://doi.org/10.5067/YSR6IA5057XX) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNPUDT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_3d\_ odt\_ Np: 3d,Monthly
mean,Time-Averaged,Pressure-Level,Assimilation,Ozone Tendencies
V5.12.4.”
[doi:10.5067/Z2KCWAV4GPD2](https://doi.org/10.5067/Z2KCWAV4GPD2) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNPODT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgM_3d\_ qdt\_ Np: 3d,Monthly
mean,Time-Averaged,Pressure-Level,Assimilation,Moist Tendencies
V5.12.4.”
[doi:10.5067/2ZTU87V69ATP](https://doi.org/10.5067/2ZTU87V69ATP) ,
<https://disc.gsfc.nasa.gov/datasets/M2TMNPQDT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
const_2d\_ asm\_ Nx: 2d, constants.”
[doi:10.5067/ME5QX6Q5IGGU](https://doi.org/10.5067/ME5QX6Q5IGGU) ,
<https://disc.gsfc.nasa.gov/datasets/M2C0NXASM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instU_2d\_ asm\_ Nx:
2d,Diurnal,Instantaneous,Single-Level,Assimilation,Single-Level
Diagnostics V5.12.4.”
[doi:10.5067/BOJSTZAO2L8R](https://doi.org/10.5067/BOJSTZAO2L8R) ,
<https://disc.gsfc.nasa.gov/datasets/M2IUNXASM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instU_2d\_ int\_ Nx:
2d,Diurnal,Instantaneous,Single-Level,Assimilation,Vertically Integrated
Diagnostics V5.12.4.”
[doi:10.5067/DGAB3HFEYMLY](https://doi.org/10.5067/DGAB3HFEYMLY) ,
<https://disc.gsfc.nasa.gov/datasets/M2IUNXINT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instU_2d\_ lfo\_ Nx:
2d,Diurnal,Instantaneous,Single-Level,Assimilation,Land Surface Forcings
V5.12.4.”
[doi:10.5067/FC3BVJ88Y8A2](https://doi.org/10.5067/FC3BVJ88Y8A2) ,
<https://disc.gsfc.nasa.gov/datasets/M2IUNXLFO_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instU_2d\_ gas\_ Nx:
2d,Diurnal,Instantaneous,Single-Level,Assimilation,Aerosol Optical Depth
Analysis V5.12.4.”
[doi:10.5067/TVJ4MHBED39L](https://doi.org/10.5067/TVJ4MHBED39L) ,
<https://disc.gsfc.nasa.gov/datasets/M2IUNXGAS_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instU_3d\_ asm\_ Np:
3d,Diurnal,Instantaneous,Pressure-Level,Assimilation,Assimilated
Meteorological Fields V5.12.4.”
[doi:10.5067/6EGRBNEBMIYS](https://doi.org/10.5067/6EGRBNEBMIYS) ,
<https://disc.gsfc.nasa.gov/datasets/M2IUNPASM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
instU_3d\_ ana\_ Np:
3d,Diurnal,Instantaneous,Pressure-Level,Analysis,Analyzed Meteorological
Fields V5.12.4.”
[doi:10.5067/TRD91YO9S6E7](https://doi.org/10.5067/TRD91YO9S6E7) ,
<https://disc.gsfc.nasa.gov/datasets/M2IUNPANA_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ adg\_ Nx:
2d,Diurnal,Time-averaged,Single-Level,Assimilation,Aerosol Diagnostics
(extended) V5.12.4.”
[doi:10.5067/YZJJXZTFCX6B](https://doi.org/10.5067/YZJJXZTFCX6B) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXADG_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ aer\_ Nx:
2d,Diurnal,Time-averaged,Single-Level,Assimilation,Aerosol Diagnostics
V5.12.4.”
[doi:10.5067/KPUMVXFEQLA1](https://doi.org/10.5067/KPUMVXFEQLA1) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXAER_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ chm\_ Nx:
2d,Diurnal,Time-Averaged,Single-Level,Assimilation,Carbon Monoxide and
Ozone Diagnostics V5.12.4.”
[doi:10.5067/5KFZ6GXRHZKN](https://doi.org/10.5067/5KFZ6GXRHZKN) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXCHM_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ csp\_ Nx:
2d,Diurnal,Time-averaged,Single-Level,Assimilation,COSP Satellite
Simulator V5.12.4.”
[doi:10.5067/9PH5QU4CL9E8](https://doi.org/10.5067/9PH5QU4CL9E8) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXCSP_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ flx\_ Nx:
2d,Diurnal,Time-Averaged,Single-Level,Assimilation,Surface Flux
Diagnostics V5.12.4.”
[doi:10.5067/LUHPNWAKYIO3](https://doi.org/10.5067/LUHPNWAKYIO3) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXFLX_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ int\_ Nx:
2d,Diurnal,Time-Averaged,Single-Level,Assimilation,Vertically Integrated
Diagnostics V5.12.4.”
[doi:10.5067/R2MPVU4EOSWT](https://doi.org/10.5067/R2MPVU4EOSWT) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXINT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ lfo\_ Nx:
2d,Diurnal,Time-Averaged,Single-Level,Assimilation,Land Surface Forcings
V5.12.4.”
[doi:10.5067/BTSNKAJND3ME](https://doi.org/10.5067/BTSNKAJND3ME) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXLFO_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ lnd\_ Nx:
2d,Diurnal,Time-Averaged,Single-Level,Assimilation,Land Surface
Diagnostics V5.12.4.”
[doi:10.5067/W0J15047CF6N](https://doi.org/10.5067/W0J15047CF6N) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXLND_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ ocn\_ Nx:
2d,Diurnal,Time-Averaged,Single-Level,Assimilation,Ocean Surface
Diagnostics V5.12.4.”
[doi:10.5067/KLNAVGAX7J66](https://doi.org/10.5067/KLNAVGAX7J66) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXOCN_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ rad\_ Nx:
2d,Diurnal,Time-Averaged,Single-Level,Assimilation,Radiation Diagnostics
V5.12.4.”
[doi:10.5067/4SDCJYK8P9QU](https://doi.org/10.5067/4SDCJYK8P9QU) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXRAD_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ slv\_ Nx:
2d,Diurnal,Time-Averaged,Single-Level,Assimilation,Single-Level
Diagnostics V5.12.4.”
[doi:10.5067/AFOK0TPEVQEK](https://doi.org/10.5067/AFOK0TPEVQEK) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXSLV_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_2d\_ glc\_ Nx:
2d,Diurnal,Time-Averaged,Single-Level,Assimilation,Land Ice Surface
Diagnostics V5.12.4.”
[doi:10.5067/7VUPQC736SWX](https://doi.org/10.5067/7VUPQC736SWX) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNXGLC_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_3d\_ cld\_ Np:
3d,Diurnal,Time-Averaged,Pressure-Level,Assimilation,Cloud Diagnostics
V5.12.4.”
[doi:10.5067/EPW7T5UO0C0N](https://doi.org/10.5067/EPW7T5UO0C0N) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNPCLD_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_3d\_ mst\_ Np:
3d,Diurnal,Time-Averaged,Pressure-Level,Assimilation,Moist Processes
Diagnostics V5.12.4.”
[doi:10.5067/ZRSN0JU27DK2](https://doi.org/10.5067/ZRSN0JU27DK2) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNPMST_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_3d\_ rad\_ Np:
3d,Diurnal,Time-Averaged,Pressure-Level,Assimilation,Radiation
Diagnostics V5.12.4.”
[doi:10.5067/H140JMDOWB0Y](https://doi.org/10.5067/H140JMDOWB0Y) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNPRAD_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_3d\_ tdt\_ Np:
3d,Diurnal,Time-Averaged,Pressure-Level,Assimilation,Temperature
Tendencies V5.12.4.”
[doi:10.5067/QPO9E5TPZ8OF](https://doi.org/10.5067/QPO9E5TPZ8OF) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNPTDT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_3d\_ trb\_ Np:
3d,Diurnal,Time-Averaged,Pressure-Level,Assimilation,Turbulence
Diagnostics V5.12.4.”
[doi:10.5067/2A99C60CG7WC](https://doi.org/10.5067/2A99C60CG7WC) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNPTRB_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_3d\_ udt\_ Np:
3d,Diurnal,Time-Averaged,Pressure-Level,Assimilation,Wind Tendencies
V5.12.4.”
[doi:10.5067/DO715T7T5PG8](https://doi.org/10.5067/DO715T7T5PG8) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNPUDT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_3d\_ odt\_ Np:
3d,Diurnal,Time-Averaged,Pressure-Level,Assimilation,Ozone Tendencies
V5.12.4.”
[doi:10.5067/M8OJ09GZP23E](https://doi.org/10.5067/M8OJ09GZP23E) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNPODT_5.12.4/summary>.

Global Modeling And Assimilation Office, Pawson S (2015). “MERRA-2
tavgU_3d\_ qdt\_ Np:
3d,Diurnal,Time-Averaged,Pressure-Level,Assimilation,Moist Tendencies
V5.12.4.”
[doi:10.5067/S8HJXIR0BFTS](https://doi.org/10.5067/S8HJXIR0BFTS) ,
<https://disc.gsfc.nasa.gov/datasets/M2TUNPQDT_5.12.4/summary>.

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_merra2(
  collection = "inst1_2d_int_Nx",
  date = "2024-01-01",
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
)
} # }
```
