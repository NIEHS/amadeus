# amadeus <a href="https://niehs.github.io/amadeus/"><img src="man/figures/logo.svg" align="right" width="185" alt="amadeus website" /></a>

[![R-CMD-check](https://github.com/NIEHS/amadeus/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/NIEHS/amadeus/actions/workflows/check-standard.yaml)
[![cov](https://NIEHS.github.io/amadeus/badges/coverage.svg)](https://github.com/NIEHS/amadeus/actions)
[![lint](https://github.com/NIEHS/amadeus/actions/workflows/lint.yaml/badge.svg)](https://github.com/NIEHS/amadeus/actions/workflows/lint.yaml)
[![pkgdown](https://github.com/NIEHS/amadeus/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/NIEHS/amadeus/actions/workflows/pkgdown.yaml)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN](https://www.r-pkg.org/badges/version/amadeus?color=blue)](https://cran.r-project.org/package=amadeus)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/amadeus)](https://cran.r-project.org/package=amadeus)

`amadeus` is **a** **m**ech**a**nism for **d**ata, **e**nvironments, and **u**ser **s**etup for common environmental and weather datasets in R. `amadeus` has been developed to improve access to and utility with large scale, publicly available environmental data in R.

See the peer-reviewed publication, [Amadeus: Accessing and analyzing large scale environmental data in R](https://www.sciencedirect.com/science/article/pii/S1364815225000362), for full description and details.

Cite `amadeus` as:
> Manware, M., Song, I., Marques, E. S., Kassien, M. A., Clark, L. P., & Messier, K. P. (2025). Amadeus: Accessing and analyzing large scale environmental data in R. Environmental Modelling & Software, 186, 106352.

## Installation

`amadeus` can be installed from CRAN with `install.packages` or from GitHub with `pak`.

```r
install.packages("amadeus")
```
```r
pak::pak("NIEHS/amadeus")
```

## Download

`download_data` accesses and downloads raw geospatial data from a variety of open source data repositories. The function is a wrapper that calls source-specific download functions, each of which account for the source's unique combination of URL, file naming conventions, and data types. Download functions cover the following sources:

| Data Source | File Type | Data Genre | Spatial Extent | Function Suffix |
| :---- | :-- | :--- | :--- | :--- |
| [Climatology Lab TerraClimate](https://www.climatologylab.org/terraclimate.html) | netCDF | Meteorology | Global | `_terraclimate` |
| [Climatology Lab GridMet](https://www.climatologylab.org/gridmet.html) | netCDF | Climate<br>Water | Contiguous United States | `_gridmet` |
| [Köppen-Geiger Climate Classification](https://www.nature.com/articles/sdata2018214) | GeoTIFF | Climate Classification | Global | `_koppen_geiger` |
| [MRLC[^1] Consortium National Land Cover Database (NLCD)](https://www.mrlc.gov/data) | GeoTIFF | Land Use | United States | `_nlcd` |
| [USDA CropScape Cropland Data Layer (CDL)](https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php) | GeoTIFF | Land Use<br>Agriculture | United States | `_cropscape` |
| [NASA[^2] Moderate Resolution Imaging Spectroradiometer (MODIS)](https://modis.gsfc.nasa.gov/data/) | HDF | Atmosphere<br>Meteorology<br>Land Use<br>Satellite | Global | `_modis` |
| [NASA Modern-Era Retrospective analysis for Research and Applications, Version 2 (MERRA-2)](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/) | netCDF | Atmosphere<br>Meteorology | Global | `_merra2` |
| [NASA SEDAC[^3] UN WPP-Adjusted Population Density](https://earthdata.nasa.gov/data/catalog/sedac-ciesin-sedac-gpwv4-apdens-wpp-2015-r11-4.11) | GeoTIFF<br>netCDF | Population | Global | `_population` |
| [NASA SEDAC Global Roads Open Access Data Set](https://data.nasa.gov/dataset/global-roads-open-access-data-set-version-1-groadsv1) | Shapefile<br>Geodatabase | Roadways | Global | `_groads` |
| [USGS[^6] Hydrologic Unit Codes (HUC)](https://www.usgs.gov/national-hydrography/access-national-hydrography-products) | Geodatabase<br>Shapefile | Hydrology | United States | `_huc` |
| [NASA Goddard Earth Observing System Composition Forcasting (GEOS-CF)](https://gmao.gsfc.nasa.gov/GEOS_systems/) | netCDF | Atmosphere<br>Meteorology | Global | `_geos` |
| [EDGAR Emissions Database for Global Atmospheric Research](https://edgar.jrc.ec.europa.eu/) | netCDF<br>TXT | Emissions | Global | `_edgar` |
| [NOAA Hazard Mapping System Fire and Smoke Product](https://www.ospo.noaa.gov/products/land/hms.html#about) | Shapefile<br>KML | Wildfire Smoke | North America | `_hms` |
| [NOAA GOES Aerosol Detection Product (ADP)](https://www.star.nesdis.noaa.gov/goes/) | netCDF | Atmosphere<br>Satellite | Americas<br>Pacific | `_goes` |
| [NOAA NCEP[^4] North American Regional Reanalysis (NARR)](https://psl.noaa.gov/data/gridded/data.narr.html) | netCDF | Atmosphere<br>Meteorology | North America | `_narr` |
| [PRISM Climate Group](https://prism.oregonstate.edu/) | netCDF<br>ASCII Grid<br>GRIB2 | Meteorology<br>Climate | Contiguous United States | `_prism` |
| [Drought indices ([SPEI](https://spei.csic.es), [EDDI](https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/), [USDM](https://droughtmonitor.unl.edu))](https://droughtmonitor.unl.edu) | netCDF<br>ASCII Grid<br>Shapefile | Drought | Global<br>Contiguous United States | `_drought` |
| [US EPA[^5] Air Data Pre-Generated Data Files](https://aqs.epa.gov/aqsweb/airdata/download_files.html) | CSV | Air Pollution | United States | `_aqs` |
| [IMPROVE aerosol monitoring program](https://vibe.cira.colostate.edu/data/export/) | TXT (pipe-delimited) | Air Pollution<br>Aerosols | United States | `_improve` |
| [US EPA Ecoregions](https://www.epa.gov/eco-research/ecoregions) | Shapefile | Climate Regions | North America | `_ecoregions` |
| [US EPA National Emissions Inventory (NEI)](https://www.epa.gov/air-emissions-inventories) | CSV | Emissions | United States | `_nei` |
| [US EPA Toxic Release Inventory (TRI) Program](https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-present) | CSV | Chemicals<br>Pollution | United States | `_tri` |
| [USGS[^6] Global Multi-resolution Terrain Elevation Data (GMTED2010)](https://www.usgs.gov/coastal-changes-and-impacts/gmted2010) | ESRI ASCII Grid | Elevation | Global | `_gmted` |

See the "[download_data](https://niehs.github.io/amadeus/articles/download_functions.html)" vignette for a detailed description of source-specific download functions.

For TRI, `download_tri()` can retrieve EPA annual basic data files for the nationwide dataset (`jurisdiction = "US"`), individual states or territories (`jurisdiction = "AZ"`, `"NC"`, etc.), and the tribal file (`jurisdiction = "tbl"`).

## NASA Earthdata authentication with `setup_nasa_token()`

Many NASA-hosted datasets require an Earthdata Login bearer token. In `amadeus`, this includes `modis`, `merra2`, `geos`, and `population` (NASA SEDAC). Use `setup_nasa_token()` to store the token before calling the corresponding `download_*()` functions. See `vignette("protected_datasets", package = "amadeus")` for more detail.

`setup_nasa_token()` supports three storage methods: `method = "renviron"` writes `NASA_EARTHDATA_TOKEN` to `~/.Renviron` for persistent personal use; `method = "file"` writes a local token file such as `~/.nasa_earthdata_token`; and `method = "session"` uses `Sys.setenv()` for the current R session only.

```r
setup_nasa_token()                              # prompts interactively
setup_nasa_token(method = "renviron", token = "<your_token>")
```

Never commit Earthdata tokens to git or include them in shared scripts. Prefer `method = "renviron"` on personal machines, and `method = "session"` for shared systems or CI jobs where the token is supplied from a CI secret.

Example use of `download_data` using NOAA NCEP North American Regional Reanalysis's (NARR) "weasd" (Daily Accumulated Snow at Surface) variable.

```r
directory <- "/  EXAMPLE  /  FILE  /  PATH  /"
download_data(
  dataset_name = "narr",
  year = 2022,
  variable = "weasd",
  directory_to_save = directory,
  acknowledgement = TRUE,
  download = TRUE,
  hash = TRUE
)
```
```
Downloading requested files...
Requested files have been downloaded.
[1] "5655d4281b76f4d4d5bee234c2938f720cfec879"
```
```r
list.files(file.path(directory, "weasd"))
```
```
[1] "weasd.2022.nc"
```

## Process

`process_covariates` imports and cleans raw geospatial data (downloaded with `download_data`), and returns a single `SpatRaster` or `SpatVector` into the user's R environment. `process_covariates` "cleans" the data by defining interpretable layer names, ensuring a coordinate reference system is present, and managing `timedata (if applicable).

To avoid errors when using `process_covariates`, **do not edit the raw downloaded data files**. Passing user-generated or edited data into `process_covariates` may result in errors as the underlying functions are adapted to each sources' raw data file type.

Example use of `process_covariates` using the downloaded "weasd" data.

```r
weasd_process <- process_covariates(
  covariate = "narr",
  date = c("2022-01-01", "2022-01-05"),
  variable = "weasd",
  path = file.path(directory, "weasd"),
  extent = NULL
)
```
```
Detected monolevel data...
Cleaning weasd data for 2022...
Returning daily weasd data from 2022-01-01 to 2022-01-05.
```
```r
weasd_process
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

`calculate_covariates` stems from the [`beethoven`](https://github.com/NIEHS/beethoven) project's need for various types of data extracted at precise locations. `calculate_covariates`, therefore, extracts data from the "cleaned" `SpatRaster` or `SpatVector` object at user defined locations. Users can choose to buffer the locations. The function returns a `data.frame`, `sf`, or `SpatVector` with data extracted at all locations for each layer or row in the `SpatRaster` or `SpatVector` object, respectively.

Example of `calculate_covariates` using processed "weasd" data.

```r
locs <- data.frame(id = "001", lon = -78.8277, lat = 35.95013)
weasd_covar <- calculate_covariates(
  covariate = "narr",
  from = weasd_process,
  locs = locs,
  locs_id = "id",
  radius = 0,
  geom = "sf"
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
Simple feature collection with 5 features and 3 fields
Geometry type: POINT
Dimension:     XY
Bounding box:  xmin: 8184606 ymin: 3523283 xmax: 8184606 ymax: 3523283
Projected CRS: unnamed
   id       time     weasd_0                geometry
1 001 2022-01-01 0.000000000 POINT (8184606 3523283)
2 001 2022-01-02 0.000000000 POINT (8184606 3523283)
3 001 2022-01-03 0.000000000 POINT (8184606 3523283)
4 001 2022-01-04 0.000000000 POINT (8184606 3523283)
5 001 2022-01-05 0.001953125 POINT (8184606 3523283)
```

## Computational considerations

`amadeus` builds on `terra` and `exactextractr`, which are C++-backed and efficient for individual raster, vector, and extraction operations. For large spatial or temporal domains, however, the cumulative wall-clock cost of many `process_*()` or `calculate_*()` calls can still be significant.

These workloads are often embarrassingly parallel across dates, variables, or location chunks. See `vignette("computational_considerations", package = "amadeus")` for examples using sequential baselines, process-level parallelism, and reproducible pipeline tools.

### Calculate_* buffer radius information

 1. locs are first projected to crs(from), then buffering uses that projected geometry.
 2. radius is interpreted in the geometry CRS distance units
 3. Most calc_* docs explicitly describe radius in meters, and output column names often encode that radius (sometimes zero-padded).
 4. For radius == 0, many paths do point extraction (no real buffer), but a couple helper paths create a tiny fallback buffer (1 or 1e-6)
for weighted/exact extraction logic.


## Connecting Health Outcomes Research Data Systems 

The `amadeus` package has been developed as part of the National Institute of Environmental Health Science's (NIEHS) Connecting Health Outcomes Research Data Systems (CHORDS) program. CHORDS aims to "build and strengthen data infrastructure for patient-centered outcomes research on environment and health" by providing curated data, analysis tools, and educational resources. As the CHORDS project comes to an end in FY26, it is being absorbed into the larger NIH Health and Extreme Weather program and the NIH Accelerator program (https://www.niehs.nih.gov/research/programs/chords/hew-data). 

## Future Development, Maintenance, and Opportunities for Contribution

`amadeus` is being actively developed and maintained by the SET group at NIEHS. Future development will focus on expanding the number of data sources and datasets covered, improving the efficiency of download and processing functions, and adding new functionality for calculating covariates and analyzing data.

1. PI driven datasets: There are many datasets created by individual researchers. To expand the number of datasets covered by `amadeus`, we will be adding functions to access and process datasets created by individual researchers. If you are an environmental health researcher with a dataset that you would like to see added to `amadeus`, please reach out via the `issues` tab on GitHub and add a tag `new dataset` to your issue.
2. More options for covariate calculations: Developing the best exposure metric for a given research question is an active area of research in environmental health. To support this research, we will be adding new options for calculating covariates from the processed data. If you have a method for calculating covariates that you would like to see added to `amadeus`, please reach out via the `issues` tab on GitHub and add a tag `new covariate calculation` to your issue.
3. Bug Fixes: As with any software, there may be bugs that arise as users interact with the package. We will be actively monitoring the `issues` tab on GitHub for bug reports and will work to fix any bugs that are reported in a timely manner. If you encounter a bug while using `amadeus`, please report it via the `issues` tab on GitHub and add a tag `bug` to your issue.


## Additional Resources

The following R packages can also be used to access environmental and weather data in R, but each differs from `amadeus` in the data sources covered or type of functionality provided.

| Package | Source |
| :--- | :----- |
| [`dataRetrieval`](https://cran.r-project.org/package=dataRetrieval) | [USGS Hydrological Data](https://www.usgs.gov/mission-areas/water-resources/data) and [EPA Water Quality Data](https://www.epa.gov/waterdata/water-quality-data) |
| [`daymetr`](https://cran.r-project.org/package=daymetr) | [Daymet](https://daac.ornl.gov/cgi-bin/dataset_lister.pl?p=32) |
| [`ecmwfr`](https://cran.r-project.org/package=ecmwfr) | [ECMWF Reanalysis v5 (ERA5)](https://www.ecmwf.int/en/forecasts/dataset/ecmwf-reanalysis-v5) |
| [`rNOMADS`](https://cran.r-project.org/package=rNOMADS) | [NOAA Operational Model Archive and Distribution System](https://nomads.ncep.noaa.gov/) |
| [`sen2r`[^8]](https://github.com/ranghetti/sen2r) | [Sentinel-2](https://sentiwiki.copernicus.eu/web/s2-mission) |
| [`eddi`](https://github.com/earthlab/eddi) | [EDDI](https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/) |
| [`heat`](https://github.com/echolab-stanford/heat) | [Harmonized Environmental Exposure Aggregation Tools] (https://github.com/echolab-stanford) |

## Contribution and AI use 

The long-term sustainability and continuous improvements and development of `amadeus` is relying on contributions from agentic AI products. GitHub Copilot is currently being used to assist with code development, documentation, and testing. To ensure the quality and reliability of the package, all contributions are reviewed and extensively tested by the maintainers before being merged into the main branch.

To add or edit functionality for new data sources or datasets, open a [Pull request](https://github.com/NIEHS/amadeus/pulls) into the main branch with a detailed description of the proposed changes. Pull requests must pass all status checks, and then will be approved or rejected by `amadeus`'s authors.

Utilize [Issues](https://github.com/NIEHS/amadeus/issues) to notify the authors of bugs, questions, or recommendations. Identify each issue with the appropriate label to help ensure a timely response.

[^1]: Multi-Resolution Land Characteristics
[^2]: National Aeronautics and Space Administration
[^3]: Socioeconomic Data and Applications Center
[^4]: National Centers for Environmental Prediction
[^5]: United States Environmental Protection Agency
[^6]: United States Geological Survey
[^7]: Last updated more than two years ago.
[^8]: Archived; no longer maintained.
