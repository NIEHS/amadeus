# amadeus

[![test-coverage](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/Spatiotemporal-Exposures-and-Toxicology/amadeus/graph/badge.svg)](https://codecov.io/gh/Spatiotemporal-Exposures-and-Toxicology/amadeus)
[![R-CMD-check](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/check-standard.yaml)
[![lint](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/lint.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/lint.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

**A** **M**echanism/**M**achine for **D**ata, **E**nvironments, and **U**ser **S**etup

`amadeus` is an R package devloped to improve and expideite users' access to large, publicly available geospatial data sets. The functions in `amadeus` allow users to download and import cleaned geospatial data directly in R, useful for automated run scripts, analysis pipelines, and reproducible science in general.

## Download

`download_data()` accesses and downloads raw geospatial data from a variety of open source data repositories. The function is a wrapper that calls source-specific download functions, each of which account for the source's unique combination of URL, file naming conventions, and data types. Download functions cover the following sources:

| Source | Data Type | Genre |
| :--- | :--- | :--- |
| [US EPA Air Data Pre-Generated Data Files](https://aqs.epa.gov/aqsweb/airdata/download_files.html) | CSV | Air Pollution |
| [US EPA Ecoregions](https://www.epa.gov/eco-research/ecoregion) | Shapefile | Climate Regions |
| [NASA Goddard Earth Observing System Composition Forcasting (GEOS-CF)](https://gmao.gsfc.nasa.gov/GEOS_systems/) | netCDF | Atmosphere, Meteorology |
| [USGS Global Multi-resolution Terrain Elevation Data (GMTED2010)](https://www.usgs.gov/coastal-changes-and-impacts/gmted2010) | ESRI ASCII Grid | Elevation |
| [Köppen-Geiger Climate Classification (Beck et al., 2018)](https://www.nature.com/articles/sdata2018214) | GeoTIFF | Climate Classification |
| [NASA Modern-Era Retrospective analysis for Research and Applications, Version 2 (MERRA-2)](https://www.nature.com/articles/sdata2018214) | netCDF | Atmosphere, Meteorology |
| [NASA Moderate Resolution Imaging Spectroradiometer (MODIS)](https://modis.gsfc.nasa.gov/data/) | HDF | Atmosphere, Meteorology, Land Use, Satellite |
| [NOAA NCEP North American Regional Reanalysis (NARR)](https://psl.noaa.gov/data/gridded/data.narr.html) | netCDF | Atmosphere, Meteorology |
| [MRLC Consortium National Land Cover Database (NLCD)](https://www.mrlc.gov/data) | GeoTIFF | Land Use |
| [NOAA Hazard Mapping System Fire and Smoke Product](https://www.ospo.noaa.gov/Products/land/hms.html#0) | Shapefile, KML | Wildfire Smoke |
| [NASA SEDAC Global Roads Open Access Data Set](https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1/data-download) | Shapefile, Geodatabase | Roadways |
| [NASA SEDAC UN WPP-Adjusted Population Density](https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11) | GeoTIFF, netCDF | Population |

See the `download_functions` vignette for a detailed description of source-specific download functions.

Example use of `download_data()` using NOAA NCEP North American Regional Reanalysis's (NARR) "weasd" (Daily Accumulated Snow at Surface) variable.

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

## Process

`process()` imports and cleans raw geospatial data (downloaded with `download()`), and returns a single `SpatRaster` or `SpatVector` into the user's R environment. `process()` "cleans" the data by defining interpretable layer names, ensuring a coordinate reference system is present, and managing `time` data (if applicable).

To avoid errors when using `process()`, **do not edit the raw downloaded data files**. Passing user-generated or edited data into `procces()` may result in errors as the underlying functions are adapted to each sources' raw data file type.

Example use of `process()` using downloaded "weasd" data.

```
> weasd <- process_narr(
+   date_start = "2022-01-01",
+   date_end = "2022-01-05",
+   variable = "weasd",
+   path = path
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

## Calculate Covariates

`calc_covariates()` stems from the `beethoven` package, and the *air pollution model's (citation)* need for various types of data extracted at precise locations. `calc_covariates()`, therefore, extracts data from the "cleaned" `SpatRaster` or `SpatVector` object at user defined locations. Users can choose to buffer the locations. The function returns a `data.frame` with data extracted at all locations for each layer or row in the `SpatRaster` or `SpatVector` object, respectively.

Example of `calc_covariates()` using processed "weasd" data.

```
> weasd_covar <- calc_narr(
+   from = weasd,
+   locs = locs,
+   locs_id = "site_id",
+   radius = 0
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
## References
