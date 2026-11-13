# Download raw data wrapper function

The `download_data()` function accesses and downloads atmospheric,
meteorological, and environmental data from various open-access data
sources.

## Usage

``` r
download_data(
  dataset_name = c("aqs", "ecoregion", "ecoregions", "geos", "goes", "goes_adp", "gmted",
    "koppen", "koppengeiger", "merra2", "merra", "modis", "narr", "nlcd", "noaa",
    "sedac_groads", "sedac_population", "groads", "population", "hms", "smoke", "tri",
    "nei", "gridmet", "terraclimate", "huc", "cropscape", "cdl", "prism", "edgar",
    "improve", "drought", "spei", "eddi", "usdm"),
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

  character(1). Dataset name or alias from the list below. Matching
  ignores case and accepts unambiguous partial matches. The default
  selects `"aqs"`.

- directory_to_save:

  character(1). Directory to save / unzip (if zip files are downloaded)
  data.

- acknowledgement:

  logical(1). By setting `TRUE` the user acknowledges that the data
  downloaded using this function may be very large and use lots of
  machine storage and memory.

- hash:

  logical(1). Return the source function's combined MD5 directory hash
  instead of its download summary. Default is `FALSE`. The directory
  included in the hash depends on the source and can include existing
  files. Ignored when `download = FALSE` is passed through `...`.

- nasa_earth_data_token:

  character(1) or NULL. NASA EarthData authentication token. Required
  for NASA EarthData datasets: `"geos"`, `"merra2"`, `"modis"`,
  `"sedac_groads"` / `"groads"`, and `"sedac_population"` /
  `"population"`. Can be a token string, a path to a file containing the
  token, or `NULL` to read from the `NASA_EARTHDATA_TOKEN` environment
  variable. Ignored for datasets that do not use NASA EarthData
  authentication.

- rate_limit:

  numeric(1). Non-negative request-throttling interval in seconds,
  passed to the source function. Default is 2. This controls the average
  request rate rather than a strict minimum gap between requests.

- ...:

  Source-specific selections and download controls, such as `date`,
  `year`, `timescale`, `version`, `download`, `show_progress`,
  `max_tries`, `unzip`, and `remove_zip`. See the selected source
  function for its arguments.

## Value

The selected downloader's return value, with its visibility preserved:

- With `download = FALSE`, an invisible preview list containing `urls`,
  `destfiles`, and `n_files`.

- With downloading enabled and `hash = FALSE`, an invisible summary
  containing `success`, `failed`, `skipped`, `failed_urls`, and
  `failed_files`. The two failure vectors are `character(0)` when no
  downloads failed, including when every file was skipped.

- With downloading enabled and `hash = TRUE`, a visible character scalar
  containing the combined MD5 hash of the source's output directory.

## Details

This wrapper selects a downloader and forwards arguments. The source
function performs validation, directory setup, discovery, downloading,
and extraction. Previews and directory layouts follow that function's
behavior.

The `"drought"` alias calls
[`download_drought()`](https://niehs.github.io/amadeus/reference/download_drought.md)
with its default SPEI source unless `source` is supplied. The `"spei"`,
`"eddi"`, and `"usdm"` aliases call
[`download_spei()`](https://niehs.github.io/amadeus/reference/download_spei.md),
[`download_eddi()`](https://niehs.github.io/amadeus/reference/download_eddi.md),
and
[`download_usdm()`](https://niehs.github.io/amadeus/reference/download_usdm.md)
directly. Supply the selected function's required arguments through
`...`. The `source` argument selects a drought source only when
`dataset_name = "drought"`.

## Note

- All download function names are in `download_*` formats

## See also

For details of each download function per dataset, Please refer to:

- [`download_aqs`](https://niehs.github.io/amadeus/reference/download_aqs.md):
  `"aqs"`, `"AQS"`

- [`download_ecoregion`](https://niehs.github.io/amadeus/reference/download_ecoregion.md):
  `"ecoregions"`, `"ecoregion"`

- [`download_geos`](https://niehs.github.io/amadeus/reference/download_geos.md):
  `"geos"`

- [`download_goes`](https://niehs.github.io/amadeus/reference/download_goes.md):
  `"goes"`, `"goes_adp"`, `"GOES"`

- [`download_gmted`](https://niehs.github.io/amadeus/reference/download_gmted.md):
  `"gmted"`, `"GMTED"`

- [`download_koppen_geiger`](https://niehs.github.io/amadeus/reference/download_koppen_geiger.md):
  `"koppen"`, `"koppengeiger"`

- [`download_merra2`](https://niehs.github.io/amadeus/reference/download_merra2.md):
  "merra2", `"merra"`, `"MERRA"`, `"MERRA2"`

- [`download_narr`](https://niehs.github.io/amadeus/reference/download_narr.md):
  `"narr"`

- [`download_nlcd`](https://niehs.github.io/amadeus/reference/download_nlcd.md):
  `"nlcd"`, `"NLCD"`

- [`download_hms`](https://niehs.github.io/amadeus/reference/download_hms.md):
  `"noaa"`, `"smoke"`, `"hms"`

- [`download_groads`](https://niehs.github.io/amadeus/reference/download_groads.md):
  `"sedac_groads"`, `"groads"`

- [`download_population`](https://niehs.github.io/amadeus/reference/download_population.md):
  `"sedac_population"`, `"population"`

- [`download_modis`](https://niehs.github.io/amadeus/reference/download_modis.md):
  `"modis"`, `"MODIS"`

- [`download_tri`](https://niehs.github.io/amadeus/reference/download_tri.md):
  `"tri"`, `"TRI"`

- [`download_nei`](https://niehs.github.io/amadeus/reference/download_nei.md):
  `"nei"`, `"NEI"`

- [`download_gridmet`](https://niehs.github.io/amadeus/reference/download_gridmet.md):
  `"gridMET"`, `"gridmet"`

- [`download_terraclimate`](https://niehs.github.io/amadeus/reference/download_terraclimate.md):
  `"TerraClimate"`, `"terraclimate"`

- [`download_huc`](https://niehs.github.io/amadeus/reference/download_huc.md):
  `"huc"`

- [`download_cropscape`](https://niehs.github.io/amadeus/reference/download_cropscape.md):
  `"cropscape"`, `"cdl"`

- [`download_prism`](https://niehs.github.io/amadeus/reference/download_prism.md):
  `"prism"`

- [`download_edgar`](https://niehs.github.io/amadeus/reference/download_edgar.md):
  `"edgar"`

- [`download_improve`](https://niehs.github.io/amadeus/reference/download_improve.md):
  `"improve"`, `"IMPROVE"`

- [`download_drought`](https://niehs.github.io/amadeus/reference/download_drought.md):
  `"drought"`

- [`download_spei`](https://niehs.github.io/amadeus/reference/download_spei.md):
  `"spei"`

- [`download_eddi`](https://niehs.github.io/amadeus/reference/download_eddi.md):
  `"eddi"`

- [`download_usdm`](https://niehs.github.io/amadeus/reference/download_usdm.md):
  `"usdm"`

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
  download = FALSE # Deprecated; may still contact the source.
)
} # }
```
