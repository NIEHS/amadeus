# Download raw data wrapper function

The `download_data()` function accesses and downloads atmospheric,
meteorological, and environmental data from various open-access data
sources.

## Usage

``` r
download_data(
  dataset_name = c("aqs", "ecoregion", "ecoregions", "geos", "goes", "goes_adp", "GOES",
    "gmted", "koppen", "koppengeiger", "merra2", "merra", "modis", "narr", "nlcd",
    "noaa", "sedac_groads", "sedac_population", "groads", "population", "hms", "smoke",
    "tri", "nei", "gridmet", "terraclimate", "huc", "cropscape", "cdl", "prism", "edgar",
    "improve", "IMPROVE", "drought", "spei", "eddi", "usdm"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  hash = FALSE,
  nasa_earth_data_token = NULL,
  rate_limit = 2,
  ...
)
```

## Arguments

- dataset_name:

  character(1). Dataset to download.

- directory_to_save:

  character(1). Directory to save / unzip (if zip files are downloaded)
  data.

- acknowledgement:

  logical(1). By setting `TRUE` the user acknowledges that the data
  downloaded using this function may be very large and use lots of
  machine storage and memory.

- hash:

  logical(1). By setting `TRUE` the function will return an
  [`rlang::hash_file()`](https://rlang.r-lib.org/reference/hash.html)
  hash character corresponding to the downloaded files. Default is
  `FALSE`.

- nasa_earth_data_token:

  character(1) or NULL. NASA EarthData authentication token. Required
  for NASA EarthData datasets: `"geos"`, `"merra2"`, `"modis"`,
  `"sedac_groads"` / `"groads"`, and `"sedac_population"` /
  `"population"`. Can be a token string, a path to a file containing the
  token, or `NULL` to read from the `NASA_EARTHDATA_TOKEN` environment
  variable. Ignored for datasets that do not use NASA EarthData
  authentication.

- rate_limit:

  numeric(1). Minimum seconds between HTTP requests (default 2). Passed
  to the underlying download function.

- ...:

  Additional arguments passed to each download function.

## Value

- For `hash = FALSE`, NULL

- For `hash = TRUE`, an
  [`rlang::hash_file`](https://rlang.r-lib.org/reference/hash.html)
  character.

- Data files will be downloaded and stored in respective sub-directories
  within `directory_to_save`. File format and sub-directory names depend
  on data source and dataset of interest.

## Note

- All download function names are in `download_*` formats

## See also

For details of each download function per dataset, Please refer to:

- [`download_aqs`](https://niehs.github.io/amadeus/dev/reference/download_aqs.md):
  `"aqs"`, `"AQS"`

- [`download_ecoregion`](https://niehs.github.io/amadeus/dev/reference/download_ecoregion.md):
  `"ecoregions"`, `"ecoregion"`

- [`download_geos`](https://niehs.github.io/amadeus/dev/reference/download_geos.md):
  `"geos"`

- [`download_goes`](https://niehs.github.io/amadeus/dev/reference/download_goes.md):
  `"goes"`, `"goes_adp"`, `"GOES"`

- [`download_gmted`](https://niehs.github.io/amadeus/dev/reference/download_gmted.md):
  `"gmted"`, `"GMTED"`

- [`download_koppen_geiger`](https://niehs.github.io/amadeus/dev/reference/download_koppen_geiger.md):
  `"koppen"`, `"koppengeiger"`

- [`download_merra2`](https://niehs.github.io/amadeus/dev/reference/download_merra2.md):
  "merra2", `"merra"`, `"MERRA"`, `"MERRA2"`

- [`download_narr`](https://niehs.github.io/amadeus/dev/reference/download_narr.md):
  `"narr"`

- [`download_nlcd`](https://niehs.github.io/amadeus/dev/reference/download_nlcd.md):
  `"nlcd"`, `"NLCD"`

- [`download_hms`](https://niehs.github.io/amadeus/dev/reference/download_hms.md):
  `"noaa"`, `"smoke"`, `"hms"`

- [`download_groads`](https://niehs.github.io/amadeus/dev/reference/download_groads.md):
  `"sedac_groads"`, `"groads"`

- [`download_population`](https://niehs.github.io/amadeus/dev/reference/download_population.md):
  `"sedac_population"`, `"population"`

- [`download_modis`](https://niehs.github.io/amadeus/dev/reference/download_modis.md):
  `"modis"`, `"MODIS"`

- [`download_tri`](https://niehs.github.io/amadeus/dev/reference/download_tri.md):
  `"tri"`, `"TRI"`

- [`download_nei`](https://niehs.github.io/amadeus/dev/reference/download_nei.md):
  `"nei"`, `"NEI"`

- [`download_gridmet`](https://niehs.github.io/amadeus/dev/reference/download_gridmet.md):
  `"gridMET"`, `"gridmet"`

- [`download_terraclimate`](https://niehs.github.io/amadeus/dev/reference/download_terraclimate.md):
  `"TerraClimate"`, `"terraclimate"`

- [`download_huc`](https://niehs.github.io/amadeus/dev/reference/download_huc.md):
  `"huc"`

- [`download_cropscape`](https://niehs.github.io/amadeus/dev/reference/download_cropscape.md):
  `"cropscape"`, `"cdl"`

- [`download_prism`](https://niehs.github.io/amadeus/dev/reference/download_prism.md):
  `"prism"`

- [`download_edgar`](https://niehs.github.io/amadeus/dev/reference/download_edgar.md):
  `"edgar"`

- [`download_improve`](https://niehs.github.io/amadeus/dev/reference/download_improve.md):
  `"improve"`, `"IMPROVE"`

- [`download_drought`](https://niehs.github.io/amadeus/dev/reference/download_drought.md):
  `"drought"`, `"spei"`, `"eddi"`, `"usdm"`

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_data(
  dataset_name = "narr",
  variables = "weasd",
  year = 2023,
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE
)
} # }
```
