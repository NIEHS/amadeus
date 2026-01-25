# Downloading EPA Daily Data

## Downloading and pre-processing pre-generated EPA AQS data from their website

This script downloads pre-processed data from EPA’s AQS data for the
desired variable, year(s), and temporal resolution.

The script also joins multiple years’ data into a single data frame, and
downloads a file with metadata about all the monitors included in the
dataset.

The first version of this script (August 2023) is written to download
daily PM2.5 data for the period 2018-2022.

Available datasets can be found at the website
<https://aqs.epa.gov/aqsweb/airdata/download_files.html>.

#### 1. Setting up for data download

Specifying temporal resolution, parameter of interest, and year

``` r
resolution <- "daily"
parameter_code <- 88101 # Parameter Code for PM2.5 local conditions
startyear <- 2018
endyear <- 2022
```

Create a list of file URLs

``` r
file_urls <- sprintf(
  paste("https://aqs.epa.gov/aqsweb/airdata/", resolution,
    "_", parameter_code, "_%.0f.zip",
    sep = ""
  ),
  startyear:endyear
)
file_urls
```

    ## [1] "https://aqs.epa.gov/aqsweb/airdata/daily_88101_2018.zip"
    ## [2] "https://aqs.epa.gov/aqsweb/airdata/daily_88101_2019.zip"
    ## [3] "https://aqs.epa.gov/aqsweb/airdata/daily_88101_2020.zip"
    ## [4] "https://aqs.epa.gov/aqsweb/airdata/daily_88101_2021.zip"
    ## [5] "https://aqs.epa.gov/aqsweb/airdata/daily_88101_2022.zip"

Specify download folder and desired name of the downloaded zip files

``` r
download_dir <- "../input/aqs/"
download_names <- sprintf(
  paste(download_dir,
    "download_output_%.0f.zip",
    sep = ""
  ),
  startyear:endyear
)
download_names
```

    ## [1] "../input/aqs/download_output_2018.zip"
    ## [2] "../input/aqs/download_output_2019.zip"
    ## [3] "../input/aqs/download_output_2020.zip"
    ## [4] "../input/aqs/download_output_2021.zip"
    ## [5] "../input/aqs/download_output_2022.zip"

#### 2. Downloading data

Download zip files from website

``` r
download.file(file_urls, download_names, method = "libcurl")
```

Construct string with unzipped file names

``` r
csv_names <- sprintf(
  paste(download_dir, resolution, "_",
    parameter_code, "_%.0f.csv",
    sep = ""
  ),
  startyear:endyear
)
```

#### 3. Processing data

Unzip and read in .csv files, process and join in one dataframe. The
unique site identifier “ID.Code” is a string with the structure
State-County-Site-Parameter-POC

``` r
for (n in seq_along(file_urls)) {
  # Unzips file to same folder it was downloaded to
  unzip(download_names[n], exdir = download_dir)

  # Read in dataframe
  print(paste("reading and processing file:", csv_names[n], "..."))
  data <- read.csv(csv_names[n], stringsAsFactors = FALSE)

  # Make unique site identifier: State-County-Site-Parameter-POC
  data$ID.Code <- paste(data$State.Code, data$County.Code,
    data$Site.Num, data$Parameter.Code,
    data$POC,
    sep = "-"
  )

  # Concatenate with other years
  if (n == 1) {
    data_all <- data
  } else {
    data_all <- rbind(data_all, data)
  }
}
```

    ## [1] "reading and processing file:../input/aqs/daily_88101_2018.csv..."
    ## [1] "reading and processing file:../input/aqs/daily_88101_2019.csv..."
    ## [1] "reading and processing file:../input/aqs/daily_88101_2020.csv..."
    ## [1] "reading and processing file:../input/aqs/daily_88101_2021.csv..."
    ## [1] "reading and processing file:../input/aqs/daily_88101_2022.csv..."

#### 4. Downloading monitor metadata file and filter for relevant sites

Download monitors file

``` r
destfile <- paste(download_dir, "aqs_monitors.zip", sep = "")
download.file("https://aqs.epa.gov/aqsweb/airdata/aqs_monitors.zip", destfile)
```

Unzip and read in

``` r
unzip(destfile, exdir = download_dir)
monitors <- read.csv("../input/aqs/aqs_monitors.csv", stringsAsFactors = FALSE)
```

Create site identifier

``` r
# Convert from string to numeric to get rid of leading zeros,
# the NAs introduced are from monitors in Canada with site number="CC"
monitors$State.Code <- as.numeric(monitors$State.Code)
monitors$ID.Code <- paste(monitors$State.Code, monitors$County.Code,
  monitors$Site.Num, monitors$Parameter.Code,
  monitors$POC,
  sep = "-"
)
monitors <- read.csv("../input/aqs/aqs_monitors.csv",
  stringsAsFactors = FALSE
)
```

Filter monitors file to include only monitors in our csv

``` r
monitors_filter <- monitors[which(monitors$ID.Code %in% data_all$ID.Code), ]
```

#### 5. Uploading data to desired folder

``` r
savepath <- "../input/aqs/"

write.csv(data_all, paste(savepath, resolution, "_", parameter_code, "_",
  startyear, "-", endyear, ".csv",
  sep = ""
))
write.csv(monitors_filter, paste(savepath, "monitors_", parameter_code, "_",
  startyear, "-", endyear, ".csv",
  sep = ""
))
```
