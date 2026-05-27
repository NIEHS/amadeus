# download_data Function

``` r

library(amadeus)
```

## Motivation

The `download_data` function was developed to improve researchers’
access to publicly available environmental data. Although the data are
already available online, using a web browser to manually download
hundreds or thousands of data files is slow, arduous, and not
efficiently repeatable. Additionally, as users may not be familiar with
creating download recipes in Bash (Unix shell), `download_data` allows
researchers to download data directly with `R`, a common coding language
in the field of environmental health research. Finally, function-izing
data downloads is useful for repeated code or automated analysis
pipelines.

## download_data

`download_data` is acccesses and downloads environmental datasets,
collections, and variables from a variety of sources. This wrapper
function calls source-specific data download functions, each utilizing a
unique combination of input parameters, host URL, naming convention, and
data formats. For example, EPA TRI basic data files are available as
nationwide (`jurisdiction = "US"`), state or territory-specific
(`jurisdiction = "AZ"`, `"NC"`, etc.), and tribal
(`jurisdiction = "tbl"`) annual CSV downloads via
[`download_tri()`](https://niehs.github.io/amadeus/reference/download_tri.md).

|  | Download Function | Data Source |
|:---|:---|:---|
| 7 | download_gridmet | [Climatology Lab GridMET](https://www.climatologylab.org/gridmet.html) |
| 19 | download_terraclimate | [Climatology Lab TerraClimate](https://www.climatologylab.org/terraclimate.html) |
| 4 | download_edgar | [EU JRC Emissions Database for Global Atmospheric Research (EDGAR)](https://edgar.jrc.ec.europa.eu/) |
| 11 | download_koppen_geiger | [Köppen-Geiger Climate Classification (Beck et al., 2018)](https://www.nature.com/articles/sdata2018214) |
| 16 | download_nlcd | [MRLC Consortium National Land Cover Database (NLCD)](https://www.mrlc.gov/data) |
| 5 | download_geos | [NASA Goddard Earth Observing System Composition Forecasting (GEOS-CF)](https://gmao.gsfc.nasa.gov/GEOS_systems/) |
| 13 | download_modis | [NASA Moderate Resolution Imaging Spectroradiometer (MODIS)](https://modis.gsfc.nasa.gov/data/) |
| 12 | download_merra2 | [NASA Modern-Era Retrospective analysis for Research and Applications, Version 2 (MERRA-2)](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/) |
| 8 | download_groads | [NASA SEDAC Global Roads Open Access Data Set](https://data.nasa.gov/dataset/global-roads-open-access-data-set-version-1-groadsv1) |
| 17 | download_population | [NASA SEDAC UN WPP-Adjusted Population Density](https://earthdata.nasa.gov/data/catalog/sedac-ciesin-sedac-gpwv4-apdens-wpp-2015-r11-4.11) |
| 9 | download_hms | [NOAA Hazard Mapping System Fire and Smoke Product](https://www.ospo.noaa.gov/products/land/hms.html#0) |
| 14 | download_narr | [NOAA NCEP North American Regional Reanalysis (NARR)](https://psl.noaa.gov/data/gridded/data.narr.html) |
| 18 | download_prism | [Parameter Elevation Regression on Independent Slopes Model (PRISM)](https://elibrary.asabe.org/abstract.asp??JID=3&AID=3101&CID=t2000&v=43&i=6&T=1) |
| 1 | download_aqs | [US EPA Air Data Pre-Generated Data Files](https://aqs.epa.gov/aqsweb/airdata/download_files.html) |
| 3 | download_ecoregion | [US EPA Ecoregions](https://www.epa.gov/eco-research/ecoregions) |
| 15 | download_nei | [US EPA National Emissions Inventory (NEI)](https://www.epa.gov/air-emissions-inventories) |
| 20 | download_tri | [US EPA Toxic Release Inventory (TRI) Program](https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-present) |
| 2 | download_cropscape | [USDA National Agricultural Statistics Service CropScape (Cropland Data Layer)](https://nassgeodata.gmu.edu/CropScape/) |
| 6 | download_gmted | [USGS Global Multi-resolution Terrain Elevation Data (GMTED2010)](https://www.usgs.gov/coastal-changes-and-impacts/gmted2010) |
| 10 | download_huc | [USGS National Hydrography Dataset (NHD)](https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data) |

Source-specific download functions and data sources {.table}

It is important to note that `download_data` calls a source-specific
function based on the `dataset_name` parameter. Using the
source-specific function directly will return the same data **if the
parameters are the same**, but the error messages produced by each
differ slightly.

### Parameters

User-defined parameters differ based on the data source. Required
parameters for each source can be checked with `names(formals())`.

``` r

names(formals(download_hms))
#>  [1] "data_format"       "date"              "directory_to_save"
#>  [4] "acknowledgement"   "download"          "remove_command"   
#>  [7] "unzip"             "remove_zip"        "show_progress"    
#> [10] "hash"              "max_tries"         "rate_limit"
names(formals(download_narr))
#>  [1] "variables"         "year"              "directory_to_save"
#>  [4] "acknowledgement"   "download"          "remove_command"   
#>  [7] "show_progress"     "hash"              "max_tries"        
#> [10] "rate_limit"
```

The two functions have different required parameters because
`download_hms` uses a daily temporal resolution while `download_narr`
uses yearly, but they share some common, standard parameters.

#### Standard parameters

Three parameters are included in all of the data download functions.

| Parameter | Type | Description |
|:---|:---|:---|
| directory_to_save | Character | There must be a directory to save downloaded data. Default = ‘./input/DATASET_NAME/’. |
| acknowledgement | Logical | User must acknowledge that downloading geospatial data can be very large and may use lots of machine storage and memory. |
| download | Logical | DEPRECATED. Downloads now use httr2 by default. When FALSE, the function returns early with a list of URLs and destination file paths (useful for unit tests — see [Unit Tests](#unit-tests)). |

Additionally, the `dataset_name` parameter must be specified when using
`download_data`, but is assumed when using a source-specific download
function.

### Function Structure

Although each source-specific download function is unique, they all
follow the same general structure. The following chunks of code have
been adopted from `download_hms` to demonstrate the functions’
structure.

[1. Clean Parameters](#clean-parameters)

[2. Generate Download URLs and destination file
paths](#generate-download-urls-and-destination-file-paths)

[3. Validate URLs](#validate-urls)

[4. Download files with httr2](#download-files-with-httr2)

[5. Zip files (if applicable)](#zip-files-if-applicable)

#### 1. Clean parameters

Cleaning the user-defined parameters is highly dependent on the
parameters themselves and the desired URL to be created. A common
parameter cleaning step is creating a date-time sequence based on a
given temporal range and required format, in this case `YYYYMMDD`.

``` r

# user defined parameters
dates <- c("2023-12-28", "2024-01-02")
```

``` r

date_sequence <- seq(
  as.Date(dates[1], format = "%Y-%m-%d"),
  as.Date(dates[2], format = "%Y-%m-%d"),
  "day"
)
date_sequence <- gsub("-", "", as.character(date_sequence))
date_sequence
#> [1] "20231228" "20231229" "20231230" "20231231" "20240101" "20240102"
```

#### 2. Generate download URLs and destination file paths

The URL base and pattern are identified by manually inspecting the
download link on the source-specific web page. `download_hms` utilizes
the year, month, date, and data format to generate the download URL and
a corresponding destination file path.

``` r

# user defined parameters
data_format <- "Shapefile"
suffix <- ".zip"
directory_to_save <- "./data/"
```

``` r

all_urls <- character()
all_destfiles <- character()

for (d in seq_along(date_sequence)) {
  year <- substr(date_sequence[d], 1, 4)
  month <- substr(date_sequence[d], 5, 6)
  base <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/"
  url <- paste0(
    base,
    data_format,
    "/",
    year,
    "/",
    month,
    "/hms_smoke",
    date_sequence[d],
    suffix
  )
  destfile <- paste0(
    directory_to_save,
    "hms_smoke_",
    data_format,
    "_",
    date_sequence[d],
    suffix
  )
  all_urls <- c(all_urls, url)
  all_destfiles <- c(all_destfiles, destfile)
}
all_urls
#> [1] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2023/12/hms_smoke20231228.zip"
#> [2] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2023/12/hms_smoke20231229.zip"
#> [3] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2023/12/hms_smoke20231230.zip"
#> [4] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2023/12/hms_smoke20231231.zip"
#> [5] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2024/01/hms_smoke20240101.zip"
#> [6] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2024/01/hms_smoke20240102.zip"
```

A URL and destination file path are created for each date in
`date_sequence` based on fixed patterns.

#### 3. Validate URLs

Before initiating downloads, the first URL in the list is validated
internally. This guards against common user errors such as invalid dates
or unsupported data formats — an error is raised early rather than
partway through a large download batch.

#### 4. Download files with httr2

All source-specific download functions use `httr2` internally to provide
robust retry logic, rate-limiting, token-based authentication (for NASA
datasets), and streaming downloads directly to disk.

When `download = FALSE` is passed to a source-specific function (or
[`download_data()`](https://niehs.github.io/amadeus/reference/download_data.md)),
the function returns early with a named list instead of downloading:

``` r

result <- download_data(
  dataset_name = "hms",
  date = c("2023-12-28", "2024-01-02"),
  data_format = "Shapefile",
  directory_to_save = "./data/",
  acknowledgement = TRUE,
  download = FALSE
)
# result$urls       — character vector of download URLs
# result$destfiles  — character vector of destination paths
# result$n_files    — integer count
```

#### 5. Zip files (if applicable)

All of the source-specific data download functions follow this general
pattern, but those functions which download zip files require additional
steps to inflate and remove the downloaded zip files, if desired. Each
of these two steps are run by helper functions, and they are run by the
user-defined `unzip` and `remove_zip` parameters in `download_data`.

`download_unzip` inflates zip files if `unzip = TRUE`, and skips
inflation if `unzip = FALSE`.

``` r

download_unzip <-
  function(file_name,
           directory_to_unzip,
           unzip = TRUE) {
    if (!unzip) {
      cat(paste0("Downloaded files will not be unzipped.\n"))
      return(NULL)
    }
    cat(paste0("Unzipping files...\n"))
    unzip(file_name,
      exdir = directory_to_unzip
    )
    cat(paste0(
      "Files unzipped and saved in ",
      directory_to_unzip,
      ".\n"
    ))
  }
```

`download_remove_zips` removes the downloaded zip files if
`remove = TRUE`, and skips removal if `remove = FALSE`.

``` r

download_remove_zips <-
  function(remove = FALSE,
           download_name) {
    if (remove) {
      cat(paste0("Removing download files...\n"))
      file.remove(download_name)
      cat(paste0("Download files removed.\n"))
    }
  }
```

For this demonstration we will unzip (inflate) the downloaded zip files
but we will not delete them.

``` r

for (f in seq_along(all_destfiles)) {
  download_unzip(
    file_name = all_destfiles[f],
    directory_to_unzip = directory_to_save,
    unzip = TRUE
  )
}
download_remove_zips(
  download_name = all_destfiles,
  remove = FALSE
)
```

    #> Unzipping files...
    #> Files unzipped and saved in ./data/.
    #> Unzipping files...
    #> Files unzipped and saved in ./data/.
    #> Unzipping files...
    #> Files unzipped and saved in ./data/.
    #> Unzipping files...
    #> Files unzipped and saved in ./data/.
    #> Unzipping files...
    #> Files unzipped and saved in ./data/.
    #> Unzipping files...
    #> Files unzipped and saved in ./data/.

Listing the files again shows that the contents of the zip files have
been inflated and the zip files have been retained.

``` r

list.files(path = directory_to_save)
```

    #>  [1] "hms_smoke_Shapefile_20231228.zip" "hms_smoke_Shapefile_20231229.zip"
    #>  [3] "hms_smoke_Shapefile_20231230.zip" "hms_smoke_Shapefile_20231231.zip"
    #>  [5] "hms_smoke_Shapefile_20240101.zip" "hms_smoke_Shapefile_20240102.zip"
    #>  [7] "hms_smoke20231228.dbf"            "hms_smoke20231228.prj"           
    #>  [9] "hms_smoke20231228.shp"            "hms_smoke20231228.shx"           
    #> [11] "hms_smoke20231229.dbf"            "hms_smoke20231229.prj"           
    #> [13] "hms_smoke20231229.shp"            "hms_smoke20231229.shx"           
    #> [15] "hms_smoke20231230.dbf"            "hms_smoke20231230.prj"           
    #> [17] "hms_smoke20231230.shp"            "hms_smoke20231230.shx"           
    #> [19] "hms_smoke20231231.dbf"            "hms_smoke20231231.prj"           
    #> [21] "hms_smoke20231231.shp"            "hms_smoke20231231.shx"           
    #> [23] "hms_smoke20240101.dbf"            "hms_smoke20240101.prj"           
    #> [25] "hms_smoke20240101.shp"            "hms_smoke20240101.shx"           
    #> [27] "hms_smoke20240102.dbf"            "hms_smoke20240102.prj"           
    #> [29] "hms_smoke20240102.shp"            "hms_smoke20240102.shx"

The download function was structured successfully.

## Unit Tests

The previous outline successfully cleaned parameters, generated URLs,
and downloaded data, but how can we be sure that it will continue to
work with different temporal ranges and data types? To this end, unit
tests have been implemented to ensure that each data download function
runs properly and that the URLs it generates are valid and accessible.
Like the download functions, the unit tests rely on helper functions to
reduce repeated code across the tests.

### Helper functions

URL validation in the unit tests uses `httr2` to check the HTTP response
status of a given URL. The desired HTTP response status is 200 (or 206),
which means the URL is valid and accessible. A helper `check_urls`
applies this check to a random sample of URLs, returning a logical
vector.

``` r

check_urls <- function(
  urls = urls,
  size = NULL
) {
  if (is.null(size)) {
    cat(paste0("URL sample size is not defined.\n"))
    return(NULL)
  }
  if (length(urls) < size) {
    size <- length(urls)
  }
  url_sample <- sample(urls, size, replace = FALSE)
  url_status <- sapply(url_sample, function(url) {
    tryCatch({
      status <- httr2::request(url) |>
        httr2::req_method("HEAD") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform() |>
        httr2::resp_status()
      Sys.sleep(1)
      status %in% c(200L, 206L)
    }, error = function(e) FALSE)
  })
  return(url_status)
}
```

### testthat

To demonstrate a test in action, test the URLs generated by
`download_data` for the NOAA HMS Smoke dataset. When called with
`download = FALSE`, functions return a list with `$urls`, `$destfiles`,
and `$n_files` — no files are written and no system commands are
executed.

For more information see [testthat](https://testthat.r-lib.org/).

``` r

library(testthat)
testthat::test_that(
  "Valid dates return HTTP response status = 200.",
  {
    # parameters
    test_start <- "2023-12-28"
    test_end <- "2024-01-02"
    test_directory <- "./data/"
    # download = FALSE returns a list with $urls (no files downloaded)
    result <- download_data(
      dataset_name = "hms",
      date = c(test_start, test_end),
      data_format = "Shapefile",
      directory_to_save = test_directory,
      acknowledgement = TRUE,
      download = FALSE
    )
    urls <- result$urls
    url_status <- check_urls(urls = urls, size = 6)
    # test for true
    expect_true(all(url_status))
  }
)
```

    #> Test passed with 1 success 🎊.

Although the `testthat::test_that(...)` chunk contains code to generate
and check URLs, the unit test is performed by
`expect_true(all(url_status))`. In words, this line is expecting
(`expect_true`) that all (`all`) of the sampled URLs return HTTP
response status 200 (`url_status`). Since this expectation was met, the
test passed!

For an alternate example, we can use a start and end date that are known
to not have data. As the URLs associated with these dates do not exist,
we expect the function will fail. This test utilizes
[`expect_error()`](https://testthat.r-lib.org/reference/expect_error.html)
because the `download_data` wrapper function returns an error message if
the underlying source-specific download function returns an error.

``` r

testthat::test_that(
  "Invalid dates cause function to fail.",
  {
    # parameters
    test_start <- "1800-01-01"
    test_end <- "1800-01-02"
    test_directory <- "../inst/extdata/"
    # test for error
    testthat::expect_error(
      download_data(
        dataset_name = "hms",
        date = c(test_start, test_end),
        data_format = "Shapefile",
        directory_to_save = test_directory,
        acknowledgement = TRUE,
        download = FALSE,
        unzip = FALSE,
        remove_zip = FALSE
      )
    )
  }
)
#> Test passed with 1 success 🎊.
```

This test utilizes
[`testthat::expect_error`](https://testthat.r-lib.org/reference/expect_error.html)
because the `download_data` wrapper function returns an error message if
the underlying source-specific download function returns an error. If we
directly used the `download_hms` function, we would expect and receive
an error.

``` r

testthat::test_that(
  "Invalid dates cause function to fail.",
  {
    # parameters
    test_start <- "1800-01-01"
    test_end <- "1800-01-02"
    test_directory <- "../inst/extdata/"
    # test for error
    testthat::expect_error(
      download_hms(
        date = c(test_start, test_end),
        data_format = "Shapefile",
        directory_to_save = test_directory,
        acknowledgement = TRUE,
        download = FALSE,
        unzip = FALSE,
        remove_zip = FALSE
      )
    )
  }
)
#> Test passed with 1 success 🌈.
```

As expected, the test passes because the NOAA HMS Smoke dataset does not
contain data for January 1-2, 1800.

These unit tests are just two of many implemented on `download_data` and
the accompanying source-specific download functions, but they
demonstrate how unit testing helps build stable code.

## Download Example

With the function structure outlined and the unit tests in place, we can
now perform a data download. To begin, check the parameters required by
the source-specific data download function.

``` r

names(formals(download_hms))
#>  [1] "data_format"       "date"              "directory_to_save"
#>  [4] "acknowledgement"   "download"          "remove_command"   
#>  [7] "unzip"             "remove_zip"        "show_progress"    
#> [10] "hash"              "max_tries"         "rate_limit"
```

Define the parameters.

``` r

dates <- c("2023-12-28", "2024-01-02")
data_format <- "Shapefile"
data_directory <- "./download_example/"
acknowledgement <- TRUE
unzip <- TRUE # inflate (unzip) downloaded zip files
remove_zip <- FALSE # retain downloaded zip files
```

Download the data.

``` r

download_data(
  dataset_name = "hms",
  date = dates,
  data_format = data_format,
  directory_to_save = data_directory,
  acknowledgement = acknowledgement,
  unzip = unzip,
  remove_zip = remove_zip
)
```

    #> Downloading requested files...
    #> Requested files have been downloaded.
    #> Unzipping files...
    #> Files unzipped and saved in ./download_example/.
    #> Unzipping files...
    #> Files unzipped and saved in ./download_example/.
    #> Unzipping files...
    #> Files unzipped and saved in ./download_example/.
    #> Unzipping files...
    #> Files unzipped and saved in ./download_example/.
    #> Unzipping files...
    #> Files unzipped and saved in ./download_example/.
    #> Unzipping files...
    #> Files unzipped and saved in ./download_example/.

Checking the directory shows that all of our desired data has been
downloaded and inflated, and the original zip files have been retained.

``` r

list.files(data_directory)
```

    #>  [1] "hms_smoke_Shapefile_20231228.zip" "hms_smoke_Shapefile_20231229.zip"
    #>  [3] "hms_smoke_Shapefile_20231230.zip" "hms_smoke_Shapefile_20231231.zip"
    #>  [5] "hms_smoke_Shapefile_20240101.zip" "hms_smoke_Shapefile_20240102.zip"
    #>  [7] "hms_smoke20231228.dbf"            "hms_smoke20231228.prj"           
    #>  [9] "hms_smoke20231228.shp"            "hms_smoke20231228.shx"           
    #> [11] "hms_smoke20231229.dbf"            "hms_smoke20231229.prj"           
    #> [13] "hms_smoke20231229.shp"            "hms_smoke20231229.shx"           
    #> [15] "hms_smoke20231230.dbf"            "hms_smoke20231230.prj"           
    #> [17] "hms_smoke20231230.shp"            "hms_smoke20231230.shx"           
    #> [19] "hms_smoke20231231.dbf"            "hms_smoke20231231.prj"           
    #> [21] "hms_smoke20231231.shp"            "hms_smoke20231231.shx"           
    #> [23] "hms_smoke20240101.dbf"            "hms_smoke20240101.prj"           
    #> [25] "hms_smoke20240101.shp"            "hms_smoke20240101.shx"           
    #> [27] "hms_smoke20240102.dbf"            "hms_smoke20240102.prj"           
    #> [29] "hms_smoke20240102.shp"            "hms_smoke20240102.shx"

## `download_hms` function code

The following is the entire R code used to create `download_hms`.

``` r

download_hms
#> function (data_format = "Shapefile", date = c("2018-01-01", "2018-01-01"), 
#>     directory_to_save = NULL, acknowledgement = FALSE, download = TRUE, 
#>     remove_command = FALSE, unzip = TRUE, remove_zip = FALSE, 
#>     show_progress = TRUE, hash = FALSE, max_tries = 20, rate_limit = 2) 
#> {
#>     amadeus::download_permit(acknowledgement = acknowledgement)
#>     amadeus::check_for_null_parameters(mget(ls()))
#>     if (length(date) == 1) {
#>         date <- c(date, date)
#>     }
#>     stopifnot(length(date) == 2)
#>     date <- date[order(as.Date(date))]
#>     if (as.Date(date[1]) < as.Date("2005-08-05")) {
#>         stop("NOAA HMS wildfire smoke data begins at August 05, 2005.")
#>     }
#>     directory_original <- amadeus::download_sanitize_path(directory_to_save)
#>     directories <- amadeus::download_setup_dir(directory_original, 
#>         zip = TRUE)
#>     directory_to_download <- directories[1]
#>     directory_to_save <- directories[2]
#>     if (!isTRUE(download)) {
#>         warning("Setting download=FALSE is deprecated.\n", call. = FALSE)
#>     }
#>     if (remove_command != FALSE) {
#>         warning("Parameter 'remove_command' is deprecated and ignored.\n", 
#>             call. = FALSE)
#>     }
#>     if (unzip == FALSE && remove_zip == TRUE) {
#>         stop(paste0("Arguments unzip = FALSE and remove_zip = TRUE are not ", 
#>             "acceptable together. Please change one.\n"))
#>     }
#>     date_sequence <- amadeus::generate_date_sequence(date[1], 
#>         date[2], sub_hyphen = TRUE)
#>     base <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/"
#>     all_urls <- character()
#>     all_destfiles <- character()
#>     for (f in seq_along(date_sequence)) {
#>         year <- substr(date_sequence[f], 1, 4)
#>         month <- substr(date_sequence[f], 5, 6)
#>         if (tolower(data_format) == "shapefile") {
#>             data_format <- "Shapefile"
#>             suffix <- ".zip"
#>             directory_to_cat <- directory_to_download
#>         }
#>         else if (tolower(data_format) == "kml") {
#>             data_format <- "KML"
#>             suffix <- ".kml"
#>             directory_to_cat <- directory_to_save
#>         }
#>         url <- paste0(base, data_format, "/", year, "/", month, 
#>             "/hms_smoke", date_sequence[f], suffix)
#>         if (f == 1) {
#>             if (!amadeus::check_url_status(url)) {
#>                 stop(paste0("Invalid date returns HTTP code 404. ", 
#>                   "Check `date` parameter.\n"))
#>             }
#>         }
#>         destfile <- paste0(directory_to_cat, "hms_smoke_", data_format, 
#>             "_", date_sequence[f], suffix)
#>         if (amadeus::check_destfile(destfile)) {
#>             all_urls <- c(all_urls, url)
#>             all_destfiles <- c(all_destfiles, destfile)
#>         }
#>     }
#>     if (!isTRUE(download)) {
#>         message(sprintf("Skipping download. Found %d files available for download.\n", 
#>             length(all_urls)))
#>         return(invisible(list(urls = all_urls, destfiles = all_destfiles, 
#>             n_files = length(all_urls))))
#>     }
#>     if (length(all_urls) == 0L) {
#>         message("All requested HMS files already exist. Nothing to download.\n")
#>         return(invisible(list(success = 0, failed = 0, skipped = length(date_sequence))))
#>     }
#>     download_result <- amadeus::download_run_method(urls = all_urls, 
#>         destfiles = all_destfiles, token = NULL, show_progress = show_progress, 
#>         max_tries = max_tries, rate_limit = rate_limit)
#>     if (data_format == "KML") {
#>         unlink(directory_to_download, recursive = TRUE)
#>         message("KML files cannot be unzipped.\n")
#>         if (hash) {
#>             return(amadeus::download_hash(hash = TRUE, directory_to_save))
#>         }
#>         else {
#>             return(invisible(download_result))
#>         }
#>     }
#>     for (d in seq_along(all_destfiles)) {
#>         amadeus::download_unzip(file_name = all_destfiles[d], 
#>             directory_to_unzip = directory_to_save, unzip = unzip)
#>     }
#>     amadeus::download_remove_zips(remove = remove_zip, download_name = all_destfiles)
#>     if (hash) {
#>         return(amadeus::download_hash(hash = TRUE, directory_to_save))
#>     }
#>     else {
#>         return(invisible(download_result))
#>     }
#> }
#> <bytecode: 0x55cafcd90130>
#> <environment: namespace:amadeus>
```
