# **A** **M**achine for **D**ata, **E**nvironments, and **U**ser **S**etup for common environmental and climate health datasets


[![R-CMD-check](https://github.com/NIEHS/amadeus/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/NIEHS/amadeus/actions/workflows/check-standard.yaml)
[![cov](https://NIEHS.github.io/amadeus/badges/coverage.svg)](https://github.com/NIEHS/amadeus/actions)
[![lint](https://github.com/NIEHS/amadeus/actions/workflows/lint.yaml/badge.svg)](https://github.com/NIEHS/amadeus/actions/workflows/lint.yaml)
[![pkgdown](https://github.com/NIEHS/amadeus/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/NIEHS/amadeus/actions/workflows/pkgdown.yaml)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

`amadeus` is an R package developed to improve and expedite users' access to large, publicly available geospatial datasets. The functions in `amadeus` allow users to download and import cleaned geospatial data directly in R, useful for automated run scripts, analysis pipelines, and reproducible science in general.

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
| [Climatology Lab TerraClimate](https://www.climatologylab.org/terraclimate.html) | netCDF | Climate, Water |
| [Climatology Lab GridMet](https://www.climatologylab.org/gridmet.html) | netCDF | Meteorology |

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

`process_covariates()` imports and cleans raw geospatial data (downloaded with `download_data()`), and returns a single `SpatRaster` or `SpatVector` into the user's R environment. `process_covariates()` "cleans" the data by defining interpretable layer names, ensuring a coordinate reference system is present, and managing `time` data (if applicable).

To avoid errors when using `process_covariates()`, **do not edit the raw downloaded data files**. Passing user-generated or edited data into `process_covariates()` may result in errors as the underlying functions are adapted to each sources' raw data file type.

Example use of `process_covariates()` using the downloaded "weasd" data.

```
> weasd <- process_covariates(
+   covariate = "narr",
+   date = c("2022-01-01", "2022-01-05"),
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
> weasd_covar <- calc_covariates(
+   covariate = "narr",
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

## Other sources
- Below is a list of other data sources that can be accessed via R packages for climate and weather datasets.

| Source | Link | R package |
| :----- | :--- | :-------- |
| Monitoring Trends in Burn Severity (MTBS) | https://www.mtbs.gov/ | |
| Daymet | https://daac.ornl.gov/cgi-bin/dataset_lister.pl?p=32 | [`daymetr`](https://cran.r-project.org/web/packages/daymetr/index.html) |
| Gridmet | https://www.climatologylab.org/gridmet.html | [`climateR`](https://github.com/mikejohnson51/climateR?tab=readme-ov-file) |
| NEX-GDDP-CMIP6 | | [`RClimChange`<sup>*</sup>](https://github.com/hllauca/RClimChange/) |
| ECMWF (e.g., ERA5) | https://www.ecmwf.int/en/forecasts/dataset/ecmwf-reanalysis-v5 | [`ecmwfr`](https://cran.r-project.org/web/packages/ecmwfr/index.html) |
| Copernicus/Sentinel | https://sentinels.copernicus.eu/web/sentinel/home | [`sen2r`<sup>**</sup>](https://github.com/ranghetti/sen2r) |
| USGS and EPA Hydrology and Water Quality Data | | [`dataRetrieval`](https://cran.r-project.org/web/packages/dataRetrieval/index.html) |
| NASA and USGS Satellite Products | | [`luna`](https://github.com/rspatial/luna) |
| NOAA Operational Model Archive | [https://nomads.ncep.noaa.gov] | [`rNOMADS`](https://cran.r-project.org/web/packages/rNOMADS/) |

<sup>*</sup> Updated longer than two years before.
<sup>**</sup> Archived; no longer maintained.

## References
