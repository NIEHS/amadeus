# amadeus <a href="https://niehs.github.io/amadeus/"><img src="man/figures/logo.svg" align="right" height="139" alt="amadeus website" /></a>

[![R-CMD-check](https://github.com/NIEHS/amadeus/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/NIEHS/amadeus/actions/workflows/check-standard.yaml)
[![cov](https://NIEHS.github.io/amadeus/badges/coverage.svg)](https://github.com/NIEHS/amadeus/actions)
[![lint](https://github.com/NIEHS/amadeus/actions/workflows/lint.yaml/badge.svg)](https://github.com/NIEHS/amadeus/actions/workflows/lint.yaml)
[![pkgdown](https://github.com/NIEHS/amadeus/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/NIEHS/amadeus/actions/workflows/pkgdown.yaml)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

`amadeus` is **a** **m**ech**a**nism for **d**ata, **e**nvironments, and **u**ser **s**etup for common environmental and climate health datasets in R. `amadeus` has been developed to improve access to and utility with large scale, publicly available environmental data in R.

## Installation

`amadeus` is not yet available from CRAN, but it can be installed with the `devtools`, `remotes`, or `pak` packages.

```r
devtools::install_github("NIEHS/amadeus")
```

```r
remotes::install_github("NIEHS/amadeus")
```

```r
pak::pak("NIEHS/amadeus")
```

## Contribution

To add or edit functionality for new data sources or datasets, open a [Pull request](https://github.com/NIEHS/amadeus/pulls) into the main branch with a detailed description of the proposed changes. Pull requests must pass all status checks, and then will be approved or rejected by `amadeus`'s authors.

Utilize [Issues](https://github.com/NIEHS/amadeus/issues) to notify the authors of bugs, questions, or recommendations. Identify each issue with the appropriate label to help ensure a timely response.

<div align="center">
  <img src="man/figures/readme_issues.png" style="width: 100%;">
</div>

## Download

`download_data` accesses and downloads raw geospatial data from a variety of open source data repositories. The function is a wrapper that calls source-specific download functions, each of which account for the source's unique combination of URL, file naming conventions, and data types. Download functions cover the following sources:

| Data Source | File Type | Data Genre |
| :---- | :-- | :--- |
| [Climatology Lab TerraClimate](https://www.climatologylab.org/terraclimate.html) | netCDF | Meteorology |
| [Climatology Lab GridMet](https://www.climatologylab.org/gridmet.html) | netCDF | Climate<br>Water |
| [Köppen-Geiger Climate Classification](https://www.nature.com/articles/sdata2018214) | GeoTIFF | Climate Classification |
| [MRLC[^1] Consortium National Land Cover Database (NLCD)](https://www.mrlc.gov/data) | GeoTIFF | Land Use |
| [NASA[^2] Moderate Resolution Imaging Spectroradiometer (MODIS)](https://modis.gsfc.nasa.gov/data/) | HDF | Atmosphere<br>Meteorology<br>Land Use<br>Satellite |
| [NASA Modern-Era Retrospective analysis for Research and Applications, Version 2 (MERRA-2)](https://www.nature.com/articles/sdata2018214) | netCDF | Atmosphere<br>Meteorology |
| [NASA SEDAC[^3] UN WPP-Adjusted Population Density](https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11) | GeoTIFF<br>netCDF | Population |
| [NASA SEDAC Global Roads Open Access Data Set](https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1/data-download) | Shapefile<br>Geodatabase | Roadways |
| [NASA Goddard Earth Observing System Composition Forcasting (GEOS-CF)](https://gmao.gsfc.nasa.gov/GEOS_systems/) | netCDF | Atmosphere<br>Meteorology |
| [NOAA Hazard Mapping System Fire and Smoke Product](https://www.ospo.noaa.gov/products/land/hms.html#about) | Shapefile<br>KML | Wildfire Smoke |
| [NOAA NCEP[^4] North American Regional Reanalysis (NARR)](https://psl.noaa.gov/data/gridded/data.narr.html) | netCDF | Atmosphere<br>Meteorology |
| [OpenGeoHub Foundation OpenLandMap](https://opengeohub.org/about-openlandmap/) | GeoTIFF | Climate<br>Elevation<br>Soil<br>Land Use<br>Satellite |
| [Parameter Elevation Regression on Independent Slopes Model (PRISM)](https://elibrary.asabe.org/abstract.asp??JID=3&AID=3101&CID=t2000&v=43&i=6&T=1) | BIL<br>ASCII | Climate |
| [US EPA[^5] Air Data Pre-Generated Data Files](https://aqs.epa.gov/aqsweb/airdata/download_files.html) | CSV | Air Pollution |
| [US EPA Ecoregions](https://www.epa.gov/eco-research/ecoregions) | Shapefile | Climate Regions |
| [US EPA National Emissions Inventory (NEI)](https://www.epa.gov/air-emissions-inventories) | CSV | Emissions |
| [US EPA Toxic Release Inventory (TRI) Program](https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-present) | CSV | Chemicals<br>Pollution |
| [USGS[^6] Global Multi-resolution Terrain Elevation Data (GMTED2010)](https://www.usgs.gov/coastal-changes-and-impacts/gmted2010) | ESRI ASCII Grid | Elevation |
| [USGS National Hydrography Dataset (NHD)](https://www.sciencebase.gov/catalog/item/4f5545cce4b018de15819ca9) | Geopackage<br>Geodatabase | Hydrography |

See the "[download_data](https://niehs.github.io/amadeus/articles/download_functions.html)" vignette for a detailed description of source-specific download functions.

Example use of `download_data` using NOAA NCEP North American Regional Reanalysis's (NARR) "weasd" (Daily Accumulated Snow at Surface) variable.

```r
directory <- "/  EXAMPLE  /  FILE  /  PATH  /"
download_data(
  dataset_name = "narr",
  year = c(2022, 2022),
  variable = "weasd",
  directory_to_save = directory,
  acknowledgement = TRUE,
  download = TRUE
)
```
```
Downloading requested files...
Requested files have been downloaded.
```
```r
list.files(paste0(directory, "weasd"))
```
```
[1] "weasd.2022.nc"
```

## Process

`process_covariates` imports and cleans raw geospatial data (downloaded with `download_data`), and returns a single `SpatRaster` or `SpatVector` into the user's R environment. `process_covariates` "cleans" the data by defining interpretable layer names, ensuring a coordinate reference system is present, and managing `timedata (if applicable).

To avoid errors when using `process_covariates`, **do not edit the raw downloaded data files**. Passing user-generated or edited data into `process_covariates` may result in errors as the underlying functions are adapted to each sources' raw data file type.

Example use of `process_covariates` using the downloaded "weasd" data.

```r
weasd <- process_covariates(
  covariate = "narr",
  date = c("2022-01-01", "2022-01-05"),
  variable = "weasd",
  path = paste0(directory, "weasd"),
  extent = NULL
)
```
```
Cleaning weasd data for January, 2022...
Detected monolevel data...
Returning daily weasd data from 2022-01-01 to 2022-01-05.
```
```r
weasd
```
```
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

`calc_covariates` stems from the [`beethoven`](https://github.com/NIEHS/beethoven) project's need for various types of data extracted at precise locations. `calc_covariates`, therefore, extracts data from the "cleaned" `SpatRaster` or `SpatVector` object at user defined locations. Users can choose to buffer the locations. The function returns a `data.frame` or `SpatVector` with data extracted at all locations for each layer or row in the `SpatRaster` or `SpatVector` object, respectively.

Example of `calc_covariates` using processed "weasd" data.

```r
locs <- data.frame(id = "001", lon = -78.8277, lat = 35.95013)
weasd_covar <- calc_covariates(
  covariate = "narr",
  from = weasd_process,
  locs = locs,
  locs_id = "id",
  radius = 0,
  geom = FALSE
)
```
```
Detected `data.frame` extraction locations...
Calculating weasd covariates for 2022-01-01...
Calculating weasd covariates for 2022-01-02...
Calculating weasd covariates for 2022-01-03...
Calculating weasd covariates for 2022-01-04...
Calculating weasd covariates for 2022-01-05...
Returning extracted covariates.
```
```r
weasd_covar
```
```
    id       time     weasd_0
1 0001 2022-01-01 0.000000000
2 0001 2022-01-02 0.000000000
3 0001 2022-01-03 0.000000000
4 0001 2022-01-04 0.000000000
5 0001 2022-01-05 0.001953125
```

## Additional Resources

The following R packages can also be used to access climate and weather data in R, but each differs from `amadeus` in the data sources covered or type of functionality provided.

| Package | Source |
| :--- | :----- |
| [`dataRetrieval`](https://cran.r-project.org/package=dataRetrieval) | [USGS Hydrological Data](https://www.usgs.gov/mission-areas/water-resources/data) and [EPA Water Quality Data](https://www.epa.gov/waterdata/water-quality-data) |
| [`daymetr`](https://cran.r-project.org/package=daymetr) | [Daymet](https://daac.ornl.gov/cgi-bin/dataset_lister.pl?p=32) |
| [`ecmwfr`](https://cran.r-project.org/package=ecmwfr) | [ECMWF Reanalysis v5 (ERA5)](https://www.ecmwf.int/en/forecasts/dataset/ecmwf-reanalysis-v5) |
| [`RClimChange`[^7]](https://github.com/hllauca/RClimChange/) | [NASA Earth Exchange Global Daily Downscaled Projections (NEX-GDDP-CMIP6)](https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6) |
| [`rNOMADS`](https://cran.r-project.org/package=rNOMADS) | [NOAA Operational Model Archive and Distribution System](https://nomads.ncep.noaa.gov/) |
| [`sen2r`[^8]](https://github.com/ranghetti/sen2r) | [Sentinel-2](https://sentiwiki.copernicus.eu/web/s2-mission) |

[^1]: Multi-Resolution Land Characteristics
[^2]: National Aeronautics and Space Administration
[^3]: Socioeconomic Data and Applications Center
[^4]: National Centers for Environmental Prediction
[^5]: United States Environmental Protection Agency
[^6]: United States Geological Survey
[^7]: Last updated more than two years ago.
[^8]: Archived; no longer maintained.
