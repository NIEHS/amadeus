[![test-coverage](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/Spatiotemporal-Exposures-and-Toxicology/amadeus/graph/badge.svg)](https://codecov.io/gh/Spatiotemporal-Exposures-and-Toxicology/amadeus)
[![R-CMD-check](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/check-standard.yaml)
[![lint](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/lint.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/lint.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

# amadeus

**A** **M**echanism/**M**achine for **D**ata, **E**nvironments, and **U**ser **S**etup

Description of package

### Download

`download_data()` accesses and downloads raw geospatial data from a variety of open source data repositories. The function is a wrapper that calls source-specific download functions, each of which account for the source's unique combination of URL, file naming conventions, and data types.

| Function                     | Source       | Data Type   | Genre       |
| :--------------------------- | :----------- | :---------- | :---------- |
| `download_aqs_data()`    | [US EPA Air Data Pre-Generated Data Files](https://aqs.epa.gov/aqsweb/airdata/download_files.html) | CSV &nbsp; &nbsp;| Air Pollution |
| `download_ecoregion_data()` | [US EPA Ecoregions](https://www.epa.gov/eco-research/ecoregion) | **data type** | Climate Regions |
| `download_geos_cf_data()` | [NASA Goddard Earth Observing System Composition Forcasting (GEOS-CF)](https://gmao.gsfc.nasa.gov/GEOS_systems/) | netCDF | Atmosphere, Meteorology |
| `download_gmted_data()`   | [USGS Global Multi-resolution Terrain Elevation Data (GMTED2010)](https://www.usgs.gov/coastal-changes-and-impacts/gmted2010) | ESRI ASCII Grid | Elevation |
| `download_koppen_geiger_data()` | [Köppen-Geiger Climate Classification (Beck et al., 2018)](https://www.nature.com/articles/sdata2018214) | GeoTIFF | Climate Classification |
| `download_merra2_data()` | [NASA Modern-Era Retrospective analysis for Research and Applications, Version 2 (MERRA-2)](https://www.nature.com/articles/sdata2018214) | netCDF | Atmosphere, Meteorology |
| `download_modis_data()` | [NASA Moderate Resolution Imaging Spectroradiometer (MODIS)](https://modis.gsfc.nasa.gov/data/) | HDF | Atmosphere, Meteorology, Land Use, Satellite |
| `download_narr_monolevel_data()` | [NOAA NCEP North American Regional Reanalysis (NARR)](https://psl.noaa.gov/data/gridded/data.narr.html) (monolevel variables) | netCDF | Atmosphere, Meteorology |
| `download_narr_p_levels_data()` | [NOAA NCEP North American Regional Reanalysis (NARR)](https://psl.noaa.gov/data/gridded/data.narr.html) (pressure levels variables) | netCDF | Atmosphere, Meteorology |
| `download_nlcd_data()` | [MRLC Consortium National Land Cover Database (NLCD)](https://www.mrlc.gov/data) | GeoTIFF | Land Use |
| `download_hms_data()` | [NOAA Hazard Mapping System Fire and Smoke Product](https://www.ospo.noaa.gov/Products/land/hms.html#0) | Shapefile | Wildfire Smoke |
| `download_sedac_groads_data()` | [NASA SEDAC Global Roads Open Access Data Set](https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1/data-download) | **data type** | Roadways |
| `download_sedac_population_data()` | [NASA SEDAC UN WPP-Adjusted Population Density](https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11) | GeoTIFF | Population |

```
> download_data(
+   dataset_name = "narr_monolevel",
+   year_start = 2022,
+   year_end = 2022,
+   variable = "weasd",
+   directory_to_save = directory_to_save,
+   data_download_acknowledgement = TRUE,
+   download = TRUE
+ )
Downloading requested files...
Requested files have been downloaded.
> list.files(paste0(directory_to_save, "weasd/"))
[1] "weasd.2022.nc"
```

### Process

`process()` imports and cleans raw geospatial data (downloaded with `download()`), and returns a single `SpatRaster` or `SpatVector` into the user's R environment. `process()` "cleans" the data by defining interpretable layer names, ensuring a coordinate reference system is present, and managing `time` data (if applicable).

To avoid errors when using `process()`, **do not edit the raw downloaded data files**. Passing user-generated or edited data into `procces()` may result in errors as the underlying functions are adapted to each sources' raw data file type.

Example workflow using `download_data()` and `process_narr()`:

```
> weasd <- process_narr(
+   date_start = "2022-01-01",
+   date_end = "2022-01-05",
+   variable = "weasd",
+   directory_with_data = directory_with_data
+ )
Cleaning weasd data for year 2022...
Returning daily weasd data from 2022-01-01 to 2022-01-05.
> weasd
class       : SpatRaster 
dimensions  : 277, 349, 5  (nrow, ncol, nlyr)
resolution  : 32462.99, 32463  (x, y)
extent      : -16231.49, 11313351, -16231.5, 8976020  (xmin, xmax, ymin, ymax)
coord. ref. : +proj=lcc +lat_0=50 +lon_0=-107 +lat_1=50 +lat_2=50 +x_0=5632642.22547 +y_0=4612545.65137 +datum=WGS84 +units=m +no_defs 
source      : weasd.2022.nc:weasd 
varname     : weasd (Daily Accumulated Snow at Surface) 
names       : weasd_20220101, weasd_20220102, weasd_20220103, weasd_20220104, weasd_20220105 
unit        :         kg/m^2,         kg/m^2,         kg/m^2,         kg/m^2,         kg/m^2 
time        : 2022-01-01 to 2022-01-05 UTC 
```

### Calculate Covariates

`calculate_covariate()` stems from the `beethoven` package, and the *air pollution model's (citation)* need for various types of data extracted at precise locations. `calculate_covariate()`, therefore, extracts data from the "cleaned" `SpatRaster` or `SpatVector` object at user defined locations. Users can choose to buffer the locations. The function returns a `data.frame` with data extracted at all locations for each layer or row in the `SpatRaster` or `SpatVector` object, respectively.

```
> weasd_covar <- covar_narr(
+   from = weasd,
+   locs = sites,
+   id = "site_id",
+   buffer = 0
+ )
Converting data.table to data.frame...
Projecting data to desired coordinate reference system...
Utilizing 0 meter buffer for covariate calculations.
Calculating daily weasd covariates at monolevel for date 2022-01-01...
Calculating daily weasd covariates at monolevel for date 2022-01-02...
Calculating daily weasd covariates at monolevel for date 2022-01-03...
Calculating daily weasd covariates at monolevel for date 2022-01-04...
Calculating daily weasd covariates at monolevel for date 2022-01-05...
Returning weasd covariates.
> weasd_covar
          site_id       date     level     weasd_0
1  37183001488101 2022-01-01 monolevel 0.000000000
2  37183002188101 2022-01-01 monolevel 0.000000000
3  37063001588101 2022-01-01 monolevel 0.000000000
4  37183001488101 2022-01-02 monolevel 0.000000000
5  37183002188101 2022-01-02 monolevel 0.000000000
6  37063001588101 2022-01-02 monolevel 0.000000000
7  37183001488101 2022-01-03 monolevel 0.000000000
8  37183002188101 2022-01-03 monolevel 0.000000000
9  37063001588101 2022-01-03 monolevel 0.000000000
10 37183001488101 2022-01-04 monolevel 0.000000000
11 37183002188101 2022-01-04 monolevel 0.000000000
12 37063001588101 2022-01-04 monolevel 0.000000000
13 37183001488101 2022-01-05 monolevel 0.003906250
14 37183002188101 2022-01-05 monolevel 0.001953125
15 37063001588101 2022-01-05 monolevel 0.001953125
```
### References
