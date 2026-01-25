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
data formats.

|     | Download Function         | Data Source                                                                                                                                                                                                  |
|:----|:--------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 14  | download_gridmet          | [Climatology Lab GridMet](https://www.climatologylab.org/gridmet.html)                                                                                                                                       |
| 13  | download_terraclimate     | [Climatology Lab TerraClimate](https://www.climatologylab.org/terraclimate.html)                                                                                                                             |
| 5   | download_koppen_geiger    | [Köppen-Geiger Climate Classification (Beck et al., 2018)](https://www.nature.com/articles/sdata2018214)                                                                                                     |
| 8   | download_nlcd             | [MRLC Consortium National Land Cover Database (NLCD)](https://www.mrlc.gov/data)                                                                                                                             |
| 3   | download_geos             | [NASA Goddard Earth Observing System Composition Forecasting (GEOS-CF)](https://gmao.gsfc.nasa.gov/GEOS_systems/)                                                                                            |
| 12  | download_modis            | [NASA Moderate Resolution Imaging Spectroradiometer (MODIS)](https://modis.gsfc.nasa.gov/data/)                                                                                                              |
| 6   | download_merra2           | [NASA Modern-Era Retrospective analysis for Research and Applications, Version 2 (MERRA-2)](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/)                                                                  |
| 10  | download_sedac_groads     | [NASA SEDAC Global Roads Open Access Data Set](https://data.earthdata.nasa.gov/nasa-earth/human-dimensions/sedac-root/downloads/data/groads/groads-global-roads-open-access-v1)                              |
| 11  | download_sedac_population | [NASA SEDAC UN WPP-Adjusted Population Density](https://data.earthdata.nasa.gov/nasa-earth/human-dimensions/sedac-root/downloads/data/gpw-v4/population-density-adjusted-to-2015-unwpp-country-totals-rev11) |
| 9   | download_hms              | [NOAA Hazard Mapping System Fire and Smoke Product](https://www.ospo.noaa.gov/products/land/hms.html#0)                                                                                                      |
| 7   | download_narr             | [NOAA NCEP North American Regional Reanalysis (NARR)](https://psl.noaa.gov/data/gridded/data.narr.html)                                                                                                      |
| 15  | download_osm              | [OpenGeoHub Foundation OpenLandMap](https://opengeohub.org/about-openlandmap/)                                                                                                                               |
| 16  | download_prism            | [Parameter Elevation Regression on Independent Slopes Model (PRISM)](https://elibrary.asabe.org/abstract.asp??JID=3&AID=3101&CID=t2000&v=43&i=6&T=1)                                                         |
| 1   | download_aqs              | [US EPA Air Data Pre-Generated Data Files](https://aqs.epa.gov/aqsweb/airdata/download_files.html)                                                                                                           |
| 2   | download_ecoregion        | [US EPA Ecoregions](https://www.epa.gov/eco-research/ecoregions)                                                                                                                                             |
| 17  | download_nei              | [US EPA National Emissions Inventory (NEI)](https://www.epa.gov/air-emissions-inventories)                                                                                                                   |
| 18  | download_tri              | [US EPA Toxic Release Inventory (TRI) Program](https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-present)                                                    |
| 4   | download_gmted            | [USGS Global Multi-resolution Terrain Elevation Data (GMTED2010)](https://www.usgs.gov/coastal-changes-and-impacts/gmted2010)                                                                                |
| 19  | download_huc              | [USGS National Hydrography Dataset (NHD)](https://www.sciencebase.gov/catalog/item/4f5545cce4b018de15819ca9)                                                                                                 |

Source-specific download functions and data sources

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
#> [1] "data_format"       "date"              "directory_to_save"
#> [4] "acknowledgement"   "download"          "remove_command"   
#> [7] "unzip"             "remove_zip"        "hash"
names(formals(download_narr))
#> [1] "variables"         "year"              "directory_to_save"
#> [4] "acknowledgement"   "download"          "remove_command"   
#> [7] "hash"
```

The two functions have different required parameters because
`download_hms` uses a daily temporal resolution while `download_narr`
uses yearly, but they share some common, standard parameters.

#### Standard parameters

Four parameters are included in all of the data download functions.

| Parameter         | Type      | Description                                                                                                                                                                                             |
|:------------------|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| directory_to_save | Character | There must be a directory to save downloaded data. Default = ‘./input/DATASET_NAME/’.                                                                                                                   |
| acknowledgement   | Logical   | User must acknowledge that downloading geospatial data can be very lage and may use lots of machine storage and memory.                                                                                 |
| download          | Logical   | Run or skip the data download. Utilized primarily for unit tests (see [Unit Tests](#unit-tests)).                                                                                                       |
| remove_command    | Logical   | Remove or retain the text file containing the generated download commands. Utilized primarily for unit tests (see [Unit Tests](#unit-tests) and [4. Initiate “…commands.txt”](#initiate-commands.txt)). |

Additionally, the `dataset_name` parameter must be specified when using
`download_data`, but is assumed when using a source-specific download
function.

### Function Structure

Although each source-specific download function is unique, they all
follow the same general structure. The following chunks of code have
been adopted from `download_hms` to demonstrate the functions’
structure.

[1. Clean Parameters](#clean-parameters)

[2. Generate Download URLs](#generate-download-urls)

[3. Generate download file names](#generate-download-file-names)

[4. Initiate “…commands.txt”](#initiate-commands.txt)

[5. Concatenate download commands](#concatenate-download-commands)

[6. Finalize “…commands.txt”](#finalize-commands.txt)

[7. Run commands in “…commands.txt”](#run-commands-in-commands.txt)

[8. Zip files (if applicable)](#zip-files-if-applicable)

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

#### 2. Generate download URLs

The URL base and pattern are identified by manually inspecting the
download link on the source-specific web page. `download_hms` utilizes
the year, month, date, and data format to generate the download url.

``` r
# user defined parameters
data_format <- "Shapefile"
suffix <- ".zip"
```

``` r
urls <- NULL
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
  urls <- c(urls, url)
}
urls
#> [1] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2023/12/hms_smoke20231228.zip"
#> [2] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2023/12/hms_smoke20231229.zip"
#> [3] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2023/12/hms_smoke20231230.zip"
#> [4] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2023/12/hms_smoke20231231.zip"
#> [5] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2024/01/hms_smoke20240101.zip"
#> [6] "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2024/01/hms_smoke20240102.zip"
```

A download URL is created for each date in `date_sequence` based on the
fixed pattern.

#### 3. Generate download file names

The generation of download file names also follows a fixed pattern,
typically a combination of the user-defined download directory, dataset
name, spatiotemporal characteristic, data type, and, if applicable,
specific variable name. Unlike the download URLs, the download file
names can be defined in any way by the writer of the function, but using
the previously defined characteristics is useful for identification.

``` r
# user defined parameters
directory_to_download <- "./data/"
```

``` r
download_file_names <- NULL
for (d in seq_along(date_sequence)) {
  download_file_name <- paste0(
    directory_to_download,
    "hms_smoke_",
    data_format,
    "_",
    date_sequence[d],
    suffix
  )
  download_file_names <- c(download_file_names, download_file_name)
}
download_file_names
#> [1] "./data/hms_smoke_Shapefile_20231228.zip"
#> [2] "./data/hms_smoke_Shapefile_20231229.zip"
#> [3] "./data/hms_smoke_Shapefile_20231230.zip"
#> [4] "./data/hms_smoke_Shapefile_20231231.zip"
#> [5] "./data/hms_smoke_Shapefile_20240101.zip"
#> [6] "./data/hms_smoke_Shapefile_20240102.zip"
```

A download URL is created for each date in `date_sequence` based on the
fixed pattern.

#### 4. Initiate “…commands.txt”

An important aspect of the data download function is its
`sink...cat...sink` structure. Rather than using the
[`utils::download.file`](https://rdrr.io/r/utils/download.file.html)
function, a text file is created to store all of the download commands
generated from the URLs and file names.

This structure is utilized for several reasons:

- Consistent structure for all the source-specific download functions.

- The `download.file` function cannot accept vectors of URLs and
  destination files for downloading. An additional `for` loop to
  download data will increase function complexity and may reduce
  performance.

- Writing commands in Bash (Unix shell) script allows for specific
  arguments and flags.

- Storing the download URLs without immediately running the download
  allows for unit testing and URL checking (more on this in [Unit
  Tests](#unit-tests)).

The text file containing the download commands is named based on the
dataset, temporal range, and data transfer method.

``` r
commands_txt <- paste0(
  directory_to_download,
  "hms_smoke_",
  head(date_sequence, n = 1),
  "_",
  tail(date_sequence, n = 1),
  "_curl_commands.txt"
)
```

Create and sink the text file.

``` r
sink(commands_txt)
```

#### 5. Concatenate download commands

The Linux-based download commands are written according to the data
transfer method, download URL, download file name, and additional
arguments. Which additional arguments are included, and their order,
depend on the data transfer method and URL type.

For more information on `curl` and `wget`, the two data transfer methods
utilized by the data download functions, see [curl.1 the man
page](https://curl.se/docs/manpage.html) and [GNU Wget 1.21.1-dirty
Manual](https://www.gnu.org/software/wget/manual/wget.html) (latest
version as of January 8, 2024).

The [`cat()`](https://rdrr.io/r/base/cat.html) function will store each
of the download commands written in the `for` loop to the previously
sunk commands text file (`commands_txt`).

``` r
for (d in seq_along(date_sequence)) {
  download_comamnd <- paste0(
    "curl -s -o ",
    download_file_names[d],
    " --url ",
    urls[d],
    "\n"
  )
  cat(download_comamnd)
}
```

#### 6. Finalize “…commands.txt”

After the download commands have been concatenated to the commands text
file, a second `sink` command is run to finalize the file and stop the
appending of R output.

``` r
sink()
```

#### 7. Run commands in “…commands.txt”

A “system command” must be created to run all of the download commands
stored in the commands text file. In bash script, `.` indicates to run
all of the commands within a given script. In this case, we will run all
of the commands within the commands text file.

``` r
system_command <- paste0(
  ". ",
  commands_txt,
  "\n"
)
system_command
#> [1] ". ./data/hms_smoke_20231228_20240102_curl_commands.txt\n"
```

Running the `system_command` deploys an “auxiliary” function,
`download_run`, a function created to reduce repeated code across the
source-specific download functions. The function takes two parameters,
`system_command`, which indicates the command to be run, and `download`,
a user-defined logical parameter.

``` r
download_run <- function(
  download = FALSE,
  system_command = NULL
) {
  if (download == TRUE) {
    cat(paste0("Downloading requested files...\n"))
    system(command = system_command)
    cat(paste0("Requested files have been downloaded.\n"))
  } else {
    cat(paste0("Skipping data download.\n"))
    return(NULL)
  }
}
```

The data download is initiated by running `download_run` with the system
command identified and `download = TRUE`.

``` r
download_run(
  download = TRUE,
  system_command = system_command
)
```

Checking the download directory shows that all of the requested files
have been downloaded.

``` r
list.files(path = directory_to_download)
```

    #> [1] "hms_smoke_Shapefile_20231228.zip" "hms_smoke_Shapefile_20231229.zip"
    #> [3] "hms_smoke_Shapefile_20231230.zip" "hms_smoke_Shapefile_20231231.zip"
    #> [5] "hms_smoke_Shapefile_20240101.zip" "hms_smoke_Shapefile_20240102.zip"

#### 8. Zip files (if applicable)

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
for (f in seq_along(download_file_names)) {
  download_unzip(
    file_name = download_file_names[f],
    directory_to_unzip = directory_to_download,
    unzip = TRUE
  )
}
download_remove_zips(
  download_name = download_file_names,
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
list.files(path = directory_to_download)
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
runs properly and that URLs produced by [2. Generate download
URLs](#generate-download-urls) are valid and accessible. Like the
download functions, the unit tests rely on “helper” functions to reduce
repeated code across the tests.

### Helper functions

`read_commands` imports the commands text file and converts the data
frame to a vector.

``` r
read_commands <- function(
  commands_path = commands_path
) {
  commands <- utils::read.csv(commands_path, header = FALSE)
  commands <- commands[seq_len(nrow(commands)), ]
  return(commands)
}
```

`extract_urls` extracts each download URL from the vector of commands.
The `position` of the URL within the download command is determined in
[5. Concatenate download commands](#concatenate-download-commands).

``` r
# function to extract URLs from vector
extract_urls <- function(
  commands = commands,
  position = NULL
) {
  if (is.null(position)) {
    cat(paste0("URL position in command is not defined.\n"))
    return(NULL)
  }
  url_list <- NULL
  for (c in seq_along(commands)) {
    url <- stringr::str_split_i(commands[c], " ", position)
    url_list <- c(url_list, url)
  }
  return(url_list)
}
```

`check_url_status` is the most important of the download test “helper”
functions. This function utilizes
[`httr::HEAD`](https://httr.r-lib.org/reference/HEAD.html) and
[`httr::GET`](https://httr.r-lib.org/reference/GET.html) to check the
HTTP response status of a given URL. The desired HTTP response status is
200, which means the URL is valid and accessible. `check_url_status`
returns a logical value to indicate whether the URL returns HTTP status
200 (`TRUE`) or not (`FALSE`). For more information on HTTP status’, see
[HTTP response status
codes](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status).

``` r
check_url_status <- function(
  url,
  method = "HEAD"
) {
  http_status_ok <- 200
  if (method == "HEAD") {
    hd <- httr::HEAD(url)
  } else if (method == "GET") {
    hd <- httr::GET(url)
  }
  status <- hd$status_code
  return(status == http_status_ok)
}
```

`check_urls` applies `check_url_status` to a random sample of URLs
extracted by `extract_urls`. The sample size will vary based on the
dataset and spatio-temporal parameters being tested. The function
returns a logical vector containing the output from `check_url_status`.

``` r
check_urls <- function(
  urls = urls,
  size = NULL,
  method = "HEAD"
) {
  if (is.null(size)) {
    cat(paste0("URL sample size is not defined.\n"))
    return(NULL)
  }
  if (length(urls) < size) {
    size <- length(urls)
  }
  url_sample <- sample(urls, size, replace = FALSE)
  url_status <- sapply(url_sample,
    check_url_status,
    method = method
  )
  return(url_status)
}
```

### testthat

To demonstrate a test in action, test the URLs generated by
`download_data` for the NOAA HMS Smoke dataset.

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
    # download
    download_data(
      dataset_name = "hms",
      date = c(test_start, test_end),
      data_format = "Shapefile",
      directory_to_save = test_directory,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE,
      unzip = FALSE,
      remove_zip = FALSE
    )
    commands_path <- paste0(
      test_directory,
      "hms_smoke_",
      gsub("-", "", test_start),
      "_",
      gsub("-", "", test_end),
      "_curl_commands.txt"
    )
    # helpers
    commands <- read_commands(commands_path = commands_path)
    urls <- extract_urls(commands = commands, position = 6)
    url_status <- check_urls(urls = urls, size = 6, method = "HEAD")
    # test for true
    expect_true(all(url_status))
  }
)
```

    #> Test passed with 1 success 🎊.

Although the `testthat::test_that(...)` chunk contains 32 lines of code,
the unit test is performed by `expect_true(all(url_status))`. In words,
this line is expecting (`expect_true`) that all (`all`) of the sampled
URLs return HTTP response status 200 (`url_status`). Since this
expectation was met, the test passed!

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
        directory_to_download = test_directory,
        directory_to_save = test_directory,
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = FALSE,
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
        directory_to_download = test_directory,
        directory_to_save = test_directory,
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = FALSE,
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
#> [1] "data_format"       "date"              "directory_to_save"
#> [4] "acknowledgement"   "download"          "remove_command"   
#> [7] "unzip"             "remove_zip"        "hash"
```

Define the parameters.

``` r
dates <- c("2023-12-28", "2024-01-02")
data_format <- "Shapefile"
data_directory <- "./download_example/"
acknowledgement <- TRUE
download <- TRUE # run data download
remove_command <- TRUE # delete "...commands.txt" file
unzip <- TRUE # inflate (unzip) downloaded zip files
remove_zip <- FALSE # retain downloaded zip files
```

Download the data.

``` r
download_data(
  dataset_name = "hms",
  date = dates,
  directory_to_save = data_directory,
  acknowledgement = acknowledgement,
  download = download,
  remove_command = remove_command,
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
#>     directory_to_save = NULL, acknowledgement = FALSE, download = FALSE, 
#>     remove_command = FALSE, unzip = TRUE, remove_zip = FALSE, 
#>     hash = FALSE) 
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
#>     if (unzip == FALSE && remove_zip == TRUE) {
#>         stop(paste0("Arguments unzip = FALSE and remove_zip = TRUE are not ", 
#>             "acceptable together. Please change one.\n"))
#>     }
#>     date_sequence <- amadeus::generate_date_sequence(date[1], 
#>         date[2], sub_hyphen = TRUE)
#>     base <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/"
#>     commands_txt <- paste0(directory_original, "hms_smoke_", 
#>         utils::head(date_sequence, n = 1), "_", utils::tail(date_sequence, 
#>             n = 1), "_curl_commands.txt")
#>     amadeus::download_sink(commands_txt)
#>     download_names <- NULL
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
#>             if (!(amadeus::check_url_status(url))) {
#>                 sink()
#>                 file.remove(commands_txt)
#>                 stop(paste0("Invalid date returns HTTP code 404. ", 
#>                   "Check `date` parameter.\n"))
#>             }
#>         }
#>         destfile <- paste0(directory_to_cat, "hms_smoke_", data_format, 
#>             "_", date_sequence[f], suffix)
#>         download_names <- c(download_names, destfile)
#>         command <- paste0("curl -s -o ", destfile, " --url ", 
#>             url, "\n")
#>         if (amadeus::check_destfile(destfile)) {
#>             cat(command)
#>         }
#>     }
#>     sink()
#>     amadeus::download_run(download = download, commands_txt = commands_txt, 
#>         remove = remove_command)
#>     if (data_format == "KML") {
#>         unlink(directory_to_download, recursive = TRUE)
#>         message(paste0("KML files cannot be unzipped.\n"))
#>         return(TRUE)
#>     }
#>     for (d in seq_along(download_names)) {
#>         amadeus::download_unzip(file_name = download_names[d], 
#>             directory_to_unzip = directory_to_save, unzip = unzip)
#>     }
#>     amadeus::download_remove_zips(remove = remove_zip, download_name = download_names)
#>     return(amadeus::download_hash(hash, directory_to_save))
#> }
#> <bytecode: 0x5623de245f20>
#> <environment: namespace:amadeus>
```
