## test for download functions

testthat::test_that("Error when acknowledgement = FALSE", {
  download_datasets <- c("aqs", "ecoregion", "geos", "gmted", "koppen",
                         "koppengeiger", "merra2", "merra", "narr_monolevel",
                         "narr_p_levels", "nlcd", "noaa", "sedac_groads",
                         "sedac_population", "groads", "population", "plevels",
                         "p_levels", "monolevel", "hms", "smoke")
  for (d in seq_along(download_datasets)) {
    expect_error(
      download_data(dataset_name = download_datasets[d],
                    acknowledgement = FALSE),
      paste0("Please refer to the argument list and ",
             "the error message above to rectify the error.\n")
    )
  }
})

testthat::test_that("Error when one parameter is NULL.", {
  download_datasets <- c("aqs", "ecoregion", "geos", "gmted", "koppen",
                         "koppengeiger", "merra2", "merra", "narr_monolevel",
                         "narr_p_levels", "nlcd", "noaa", "sedac_groads",
                         "sedac_population", "groads", "population", "plevels",
                         "p_levels", "monolevel", "hms", "smoke")
  for (d in seq_along(download_datasets)) {
    expect_error(
      download_data(dataset_name = download_datasets[d],
                    acknowledgement = TRUE,
                    directory_to_save = NULL),
      paste0("Please refer to the argument list and ",
             "the error message above to rectify the error.\n")
    )
  }
})

testthat::test_that("Errors when temporal ranges invalid.", {
  expect_error(
    download_geos_data(
      date_start = "1900-01-01",
      collection = "aqc_tavg_1hr_g1440x721_v1",
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    )
  )
  expect_error(
    download_aqs_data(
      year_start = 1900,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", ""),
      directory_to_download = testthat::test_path("..", "testdata/", "")
    )
  )
  expect_error(
    download_narr_monolevel_data(
      year_start = 1900,
      variables = "air.sfc",
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    )
  )
  expect_error(
    download_narr_p_levels_data(
      year_start = 1900,
      variables = "omega",
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    )
  )
  expect_error(
    download_merra2_data(
      date_start = "1900-01-01",
      collection = "inst1_2d_asm_Nx",
      directory_to_save = testthat::test_path("..", "testdata/", ""),
      acknowledgement = TRUE
    )
  )
  expect_error(
    download_hms_data(
      date_start = "1900-01-01",
      directory_to_save = testthat::test_path("..", "testdata/", ""),
      directory_to_download = testthat::test_path("..", "testdata/", ""),
      acknowledgement = TRUE
    )
  )
})

testthat::test_that("EPA AQS download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year_start <- 2018
  year_end <- 2022
  resolution_temporal <- "daily"
  parameter_code <- 88101
  directory_to_download <- testthat::test_path("..", "testdata/", "")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  # run download function
  download_data(dataset_name = "aqs",
                year_start = year_start,
                year_end = year_end,
                directory_to_save = directory_to_save,
                directory_to_download = directory_to_download,
                acknowledgement = TRUE,
                unzip = FALSE,
                remove_zip = FALSE,
                download = FALSE,
                remove_command = FALSE)
  # define file path with commands
  commands_path <-
    paste0(
           directory_to_download,
           "aqs_",
           parameter_code,
           "_",
           year_start, "_", year_end,
           "_",
           resolution_temporal,
           "_curl_commands.txt")
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 2)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  # implement unit tets
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})


testthat::test_that("Ecoregion download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  directory_to_download <- testthat::test_path("..", "testdata/", "")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  certificate <- system.file("extdata/cacert_gaftp_epa.pem",
                             package = "amadeus")
  # run download function
  download_data(dataset_name = "ecoregion",
                directory_to_save = directory_to_save,
                directory_to_download = directory_to_download,
                acknowledgement = TRUE,
                unzip = FALSE,
                remove_zip = FALSE,
                download = FALSE,
                remove_command = FALSE,
                epa_certificate_path = certificate)
  # define file path with commands
  commands_path <- paste0(
    directory_to_download,
    "us_eco_l3_state_boundaries_",
    Sys.Date(),
    "_wget_command.txt"
  )
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 3)
  # check HTTP URL status
  url_status <-
    httr::HEAD(urls, config = httr::config(cainfo = certificate))
  url_status <- url_status$status_code
  # implement unit tets
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})

testthat::test_that("GEOS-CF download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  date_start <- "2019-09-09"
  date_end <- "2019-09-21"
  collections <- c("aqc_tavg_1hr_g1440x721_v1",
                   "chm_inst_1hr_g1440x721_p23")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  for (c in seq_along(collections)) {
    # run download function
    download_data(dataset_name = "geos",
                  date_start = date_start,
                  date_end = date_end,
                  collection = collections[c],
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE)
    # define file path with commands
    commands_path <- paste0(directory_to_save,
                            collections[c],
                            "_",
                            date_start,
                            "_",
                            date_end,
                            "_wget_commands.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 2)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 20L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
})

testthat::test_that("GMTED download URLs have HTTP status 200.", {
  withr::local_package("httr")
  # function parameters
  statistics <- c("Breakline Emphasis",
                  # "Systematic Subsample",
                  # "Median Statistic", "Minimum Statistic",
                  # "Mean Statistic", "Maximum Statistic",
                  "Standard Deviation Statistic")
  resolution <- "7.5 arc-seconds"
  directory_to_download <- testthat::test_path("..", "testdata/", "")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  for (s in seq_along(statistics)) {
    # run download function
    download_data(dataset_name = "gmted",
                  statistic = statistics[s],
                  resolution = resolution,
                  directory_to_download = directory_to_download,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  unzip = FALSE,
                  remove_zip = FALSE,
                  download = FALSE)
    # define file path with commands
    commands_path <- paste0(directory_to_download,
                            "gmted_",
                            gsub(" ", "", statistics[s]),
                            "_",
                            gsub(" ", "", resolution),
                            "_",
                            Sys.Date(),
                            "_curl_command.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 6)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
})

testthat::test_that("MERRA2 download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  date_start <- "2022-02-14"
  date_end <- "2022-03-08"
  collections <- c("inst1_2d_asm_Nx", "inst3_3d_asm_Np")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  for (c in seq_along(collections)) {
    # run download function
    download_data(dataset_name = "merra2",
                  date_start = date_start,
                  date_end = date_end,
                  collection = collections[c],
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE)
    # define path with commands
    commands_path <- paste0(directory_to_save,
                            collections[c],
                            "_",
                            date_start,
                            "_",
                            date_end,
                            "_wget_commands.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 2)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 3L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
})

testthat::test_that("MERRA2 returns message with unrecognized collection.", {
  # function parameters
  collections <- "uNrEcOgNiZeD"
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  testthat::expect_error(
    download_data(
      dataset_name = "merra",
      collection = collections,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    )
  )
})

testthat::test_that("NARR monolevel download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year_start <- 2018
  year_end <- 2018
  variables <- c("weasd", "air.2m")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  # run download function
  download_data(dataset_name = "narr_monolevel",
                year_start = year_start,
                year_end = year_end,
                variables = variables,
                directory_to_save = directory_to_save,
                acknowledgement = TRUE,
                download = FALSE)
  # define path with commands
  commands_path <- paste0(directory_to_save,
                          "narr_monolevel_",
                          year_start, "_", year_end,
                          "_curl_commands.txt")
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 6)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 5L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})

testthat::test_that("NARR monolevel error with invalid years.", {
  testthat::expect_error(
    download_data(
      dataset_name = "narr_monolevel",
      variables = "weasd",
      year_start = 10,
      year_end = 11,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    )
  )
})

testthat::test_that("NARR p-levels download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year_start <- 2020
  year_end <- 2021
  variables <- c("shum", "omega")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  # run download function
  download_data(dataset_name = "narr_p_levels",
                year_start = year_start,
                year_end = year_end,
                variables = variables,
                directory_to_save = directory_to_save,
                acknowledgement = TRUE,
                download = FALSE)
  # define file path with commands
  commands_path <- paste0(directory_to_save,
                          "narr_p_levels_",
                          year_start, "_", year_end,
                          "_curl_commands.txt")
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 6)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 20L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})

testthat::test_that("NOAA HMS Smoke download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  date_start <- "2022-08-12"
  date_end <- "2022-09-21"
  directory_to_download <- testthat::test_path("..", "testdata/", "")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  data_formats <- c("Shapefile", "KML")
  for (d in seq_along(data_formats)) {
    # run download function
    download_data(dataset_name = "smoke",
                  date_start = date_start,
                  date_end = date_end,
                  data_format = data_formats[d],
                  directory_to_download = directory_to_download,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE,
                  unzip = FALSE,
                  remove_zip = FALSE)
    # define file path with commands
    commands_path <- paste0(directory_to_download,
                            "hms_smoke_",
                            gsub("-", "", date_start),
                            "_",
                            gsub("-", "", date_end),
                            "_curl_commands.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 6)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 3L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
})

testthat::test_that("download_hms_data error for unzip and directory.", {
  testthat::expect_error(
    download_data(
      dataset_name = "hms",
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", ""),
      directory_to_download = testthat::test_path("..", "testdata/", ""),
      unzip = FALSE,
      remove_zip = TRUE
    )
  )
})

testthat::test_that("NLCD download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  years <- c(2021, 2019, 2016)
  collections <- c(rep("Coterminous United States", 2), "Alaska")
  collection_codes <- c(rep("l48", 2), "ak")
  directory_to_download <- testthat::test_path("..", "testdata/", "")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  # run download function
  for (y in seq_along(years)) {
    download_data(dataset_name = "nlcd",
                  year = years[y],
                  collection = collections[y],
                  directory_to_download = directory_to_download,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE,
                  unzip = FALSE,
                  remove_zip = FALSE)
    # define file path with commands
    commands_path <- paste0(directory_to_download,
                            "nlcd_",
                            years[y],
                            "_land_cover_",
                            collection_codes[y],
                            "_",
                            Sys.Date(),
                            "_curl_command.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 5)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_download = directory_to_download,
                            directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
  testthat::expect_error(
    download_data(dataset_name = "nlcd",
                  year = 2000,
                  collection = "Coterminous United States",
                  directory_to_download = directory_to_download,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = TRUE,
                  unzip = FALSE,
                  remove_zip = FALSE)
  )

})

testthat::test_that("SEDAC groads download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  data_regions <- c("Americas", "Global")
  data_formats <- c("Geodatabase", "Shapefile")
  directory_to_download <- testthat::test_path("..", "testdata/", "")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  # run download function
  for (r in seq_along(data_regions)) {
    data_region <- data_regions[r]
    for (f in seq_along(data_formats)) {
      download_data(dataset_name = "sedac_groads",
                    directory_to_save = directory_to_save,
                    acknowledgement = TRUE,
                    data_format = data_formats[f],
                    data_region = data_region,
                    directory_to_download = directory_to_download,
                    download = FALSE,
                    unzip = FALSE,
                    remove_zip = FALSE,
                    remove_command = FALSE)
      # define file path with commands
      commands_path <- paste0(directory_to_download,
                              "sedac_groads_",
                              gsub(" ", "_", tolower(data_region)),
                              "_",
                              Sys.Date(),
                              "_curl_command.txt")
      # import commands
      commands <- read_commands(commands_path = commands_path)
      # extract urls
      urls <- extract_urls(commands = commands, position = 11)
      # check HTTP URL status
      url_status <- check_urls(urls = urls, size = 1L, method = "GET")
      # implement unit tests
      test_download_functions(directory_to_download = directory_to_download,
                              directory_to_save = directory_to_save,
                              commands_path = commands_path,
                              url_status = url_status)
      # remove file with commands after test
      file.remove(commands_path)
    }
  }

  testthat::expect_message(
    download_data(dataset_name = "sedac_groads",
                  data_format = "Shapefile",
                  data_region = "Global",
                  directory_to_download = directory_to_download,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  unzip = FALSE,
                  remove_zip = FALSE,
                  remove_command = TRUE)
  )
})

testthat::test_that("SEDAC population download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  years <- c("2020")
  data_formats <- c("GeoTIFF")
  data_resolutions <- cbind(c("30 second"),
                            c("30_sec"))
  directory_to_download <- testthat::test_path("..", "testdata/", "")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  for (f in seq_along(data_formats)) {
    data_format <- data_formats[f]
    for (y in seq_along(years)) {
      year <- years[y]
      for (r in seq_len(nrow(data_resolutions))) {
        # run download function
        download_data(dataset_name = "sedac_population",
                      year = year,
                      data_format = data_format,
                      data_resolution = data_resolutions[r, 1],
                      directory_to_download = directory_to_download,
                      directory_to_save = directory_to_save,
                      acknowledgement = TRUE,
                      download = FALSE,
                      unzip = FALSE,
                      remove_zip = FALSE,
                      remove_command = FALSE)
        # define file path with commands
        if (year == "all") {
          year <- "totpop"
        } else {
          year <- year
        }
        if (year == "totpop" && data_resolutions[r, 2] == "30_sec") {
          resolution <- "2pt5_min"
        } else {
          resolution <- data_resolutions[r, 2]
        }
        commands_path <- paste0(directory_to_download,
                                "sedac_population_",
                                year,
                                "_",
                                resolution,
                                "_",
                                Sys.Date(),
                                "_curl_commands.txt")
        # import commands
        commands <- read_commands(commands_path = commands_path)
        # extract urls
        urls <- extract_urls(commands = commands, position = 11)
        # check HTTP URL status
        url_status <- check_urls(urls = urls, size = 1L, method = "GET")
        # implement unit tests
        test_download_functions(directory_to_download = directory_to_download,
                                directory_to_save = directory_to_save,
                                commands_path = commands_path,
                                url_status = url_status)
        # remove file with commands after test
        file.remove(commands_path)
      }
    }
  }
})

testthat::test_that("SEDAC population data types are coerced.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year <- c("totpop")
  data_formats <- c("GeoTIFF", "ASCII", "netCDF")
  data_resolutions <- c("30 second", "2pt5_min")
  directory_to_download <- testthat::test_path("..", "testdata/", "")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  for (f in seq_along(data_formats)) {
    download_data(dataset_name = "sedac_population",
                  year = year,
                  data_format = data_formats[f],
                  data_resolution = data_resolutions[1],
                  directory_to_download = directory_to_download,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  unzip = FALSE,
                  remove_zip = FALSE,
                  remove_command = FALSE)
    commands_path <- paste0(directory_to_download,
                            "sedac_population_",
                            year,
                            "_",
                            data_resolutions[2],
                            "_",
                            Sys.Date(),
                            "_curl_commands.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 11)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 1L, method = "GET")
    # implement unit tests
    test_download_functions(directory_to_download = directory_to_download,
                            directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
})

testthat::test_that("Koppen Geiger download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  time_periods <- c("Present", "Future")
  data_resolutions <- c("0.0083")
  directory_to_download <- testthat::test_path("..", "testdata/", "")
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  # run download function
  for (p in seq_along(time_periods)) {
    time_period <- time_periods[p]
    for (d in seq_along(data_resolutions)) {
      download_data(dataset_name = "koppen",
                    time_period = time_period,
                    data_resolution = data_resolutions[d],
                    directory_to_download = directory_to_download,
                    directory_to_save = directory_to_save,
                    acknowledgement = TRUE,
                    unzip = FALSE,
                    remove_zip = FALSE,
                    download = FALSE,
                    remove_command = FALSE)
      # define file path with commands
      commands_path <- paste0(directory_to_download,
                              "koppen_geiger_",
                              time_period,
                              "_",
                              gsub("\\.",
                                   "p",
                                   data_resolutions[d]),
                              "_",
                              Sys.Date(),
                              "_wget_command.txt")
      # import commands
      commands <- read_commands(commands_path = commands_path)
      # extract urls
      urls <- extract_urls(commands = commands, position = 2)
      # check HTTP URL status
      url_status <- check_urls(urls = urls, size = 1L, method = "GET")
      # implement unit tests
      test_download_functions(directory_to_download = directory_to_download,
                              directory_to_save = directory_to_save,
                              commands_path = commands_path,
                              url_status = url_status)
      # remove file with commands after test
      file.remove(commands_path)
    }
  }
})

testthat::test_that("MODIS-MOD09GA download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  years <- 2020
  product <- "MOD09GA"
  version <- "61"
  horizontal_tiles <- c(12, 13)
  vertical_tiles <- c(5, 6)
  nasa_earth_data_token <- "tOkEnPlAcEhOlDeR"
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  for (y in seq_along(years)) {
    date_start <- paste0(years[y], "-06-20")
    date_end <- paste0(years[y], "-06-24")
    # run download function
    download_data(dataset_name = "modis",
                  date_start = date_start,
                  date_end = date_end,
                  product = product,
                  version = version,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
    # define file path with commands
    commands_path <- paste0(
      directory_to_save,
      product,
      "_",
      date_start,
      "_",
      date_end,
      "_wget_commands.txt"
    )
    # import commands
    commands <- read_commands(commands_path = commands_path)[, 2]
    # extract urls
    urls <- extract_urls(commands = commands, position = 4)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 10L, method = "SKIP")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
})


testthat::test_that("MODIS-MOD06L2 download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  product <- "MOD06_L2"
  version <- "61"
  date_start <- "2019-02-18"
  date_end <- "2019-02-18"
  nasa_earth_data_token <- "tOkEnPlAcEhOlDeR"
  horizontal_tiles <- c(8, 10)
  vertical_tiles <- c(4, 5)
  directory_to_save <- testthat::test_path("..", "testdata/", "")

  testthat::expect_error(
    kax <- download_data(dataset_name = "modis",
                    date_start = date_start,
                    date_end = date_end,
                    product = product,
                    version = version,
                    horizontal_tiles = horizontal_tiles,
                    vertical_tiles = vertical_tiles,
                    nasa_earth_data_token = nasa_earth_data_token,
                    directory_to_save = directory_to_save,
                    acknowledgement = TRUE,
                    download = FALSE,
                    mod06_links = NULL,
                    remove_command = FALSE)
  )
  # link check
  tdir <- tempdir()
  faux_urls <-
    rbind(
      c(4387858920,
        "/archive/allData/61/MOD06_L2/2019/049/MOD06_L2.A2019049.0720.061.2019049194350.hdf",
        28267915)
    )

  faux_urls <- data.frame(faux_urls)
  mod06_scenes <- paste0(tdir, "/mod06_example.csv")
  write.csv(faux_urls, mod06_scenes, row.names = FALSE)

  download_data(dataset_name = "modis",
                  date_start = date_start,
                  date_end = date_end,
                  product = product,
                  version = version,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  mod06_links = mod06_scenes,
                  remove_command = FALSE)
  
  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    product,
    "_",
    date_start,
    "_",
    date_end,
    "_wget_commands.txt"
  )
  # import commands
  commands <- read_commands(commands_path = commands_path)[, 2]
  # extract urls
  urls <- extract_urls(commands = commands, position = 4)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "SKIP")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})


testthat::test_that("MODIS download error cases.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  years <- 2020
  product <- c("MOD09GA", "MOD11A1", "MOD13A2", "MCD19A2")
  product <- sample(product, 1L)
  version <- "61"
  horizontal_tiles <- c(12, 13)
  vertical_tiles <- c(5, 6)
  nasa_earth_data_token <- "tOkEnPlAcEhOlDeR"
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  date_start <- paste0(years, "-06-25")
  date_end <- paste0(years, "-06-28")

  # no token
  testthat::expect_no_error(
    download_data(dataset_name = "modis",
                  date_start = date_start,
                  date_end = date_end,
                  product = product,
                  version = version,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )

  # no token
  testthat::expect_error(
    download_data(dataset_name = "modis",
                  date_start = date_start,
                  date_end = date_end,
                  product = product,
                  version = version,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = NULL,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )

  # year difference between date_start and date_end
  testthat::expect_error(
    download_data(dataset_name = "modis",
                  date_start = date_start,
                  date_end = "2024-03-28",
                  product = "MOD11A1",
                  version = version,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )

  # null version
  testthat::expect_error(
    download_data(dataset_name = "modis",
                  date_start = date_start,
                  date_end = date_end,
                  product = product,
                  version = NULL,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )

  # invalid tile range (horizontal)
  testthat::expect_error(
    download_data(dataset_name = "modis",
                  date_start = date_start,
                  date_end = date_end,
                  product = product,
                  version = "61",
                  horizontal_tiles = c(-13, -3),
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )

  # invalid tile range (horizontal)
  testthat::expect_error(
    download_data(dataset_name = "modis",
                  date_start = date_start,
                  date_end = date_end,
                  product = product,
                  version = "61",
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = c(100, 102),
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )


  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    product,
    "_",
    date_start,
    "_",
    date_end,
    "_wget_commands.txt"
  )
  # import commands
  commands <- read_commands(commands_path = commands_path)[, 2]
  # extract urls
  urls <- extract_urls(commands = commands, position = 4)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 2L, method = "SKIP")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})



testthat::test_that("EPA TRI download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  # run download function
  download_data(dataset_name = "tri",
                directory_to_save = directory_to_save,
                acknowledgement = TRUE,
                download = FALSE,
                remove_command = FALSE)
  year_start <- 2018L
  year_end <- 2022L
  
  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    "TRI_",
    year_start, "_", year_end,
    "_",
    Sys.Date(),
    "_curl_commands.txt"
  )

  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 3)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "SKIP")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})


testthat::test_that("EPA NEI (AADT) download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  certificate <- system.file("extdata/cacert_gaftp_epa.pem",
                             package = "amadeus")
  # run download function
  year_target <- c(2017L, 2020L)
  download_data(dataset_name = "nei",
                directory_to_save = directory_to_save,
                acknowledgement = TRUE,
                download = FALSE,
                year_target = year_target,
                remove_command = FALSE,
                epa_certificate_path = certificate
                )
  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    "NEI_AADT_",
    paste(year_target, collapse = "-"),
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )

  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 3)
  # check HTTP URL status
  url_status <-
    httr::HEAD(urls[1], config = httr::config(cainfo = certificate))
  url_status <- url_status$status_code
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})

testthat::test_that("Test error cases in EPA gaftp sources 1", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  tdir <- tempdir()
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  certificate <- file.path(tdir, "cacert_gaftp_epa.pem")
  # remove if there is a preexisting file
  if (file.exists(certificate)) {
    file.remove(certificate)
    file.remove(gsub("pem", "crt", certificate))
  }

  # run download function
  year_target <- c(2017L)
  testthat::expect_message(
    download_data(dataset_name = "nei",
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  year_target = year_target,
                  remove_command = FALSE,
                  epa_certificate_path = certificate
                  )
  )
  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    "NEI_AADT_",
    paste(year_target, collapse = "-"),
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )
  # remove file with commands after test
  testthat::expect_true(file.exists(commands_path))
  file.remove(commands_path)
})

testthat::test_that("Test error cases in EPA gaftp sources 2", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  tdir <- tempdir(check = TRUE)
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  certificate <- file.path(tdir, "cacert_gaftp_epa.pem")
  # remove if there is a preexisting file
  if (file.exists(certificate)) {
    file.remove(certificate)
    file.remove(gsub("pem", "crt", certificate))
  }

  # run download function

  testthat::expect_message(
    download_data(dataset_name = "ecoregion",
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE,
                  unzip = FALSE,
                  remove_zip = FALSE,
                  directory_to_download = directory_to_save,
                  epa_certificate_path = certificate
                  )
  )
  # unlink dir
  unlink(tdir)

  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    "us_eco_l3_state_boundaries_",
    Sys.Date(),
    "_wget_command.txt"
  )

  # remove file with commands after test
  testthat::expect_true(file.exists(commands_path))
  file.remove(commands_path)
})


testthat::test_that("epa certificate", {
  testthat::expect_error(
    download_epa_certificate("file.txt")
  )
  testthat::expect_message(
    download_epa_certificate(file.path(tempdir(), "file.pem"))
  )
  testthat::expect_no_error(
    download_epa_certificate(
      system.file("extdata/cacert_gaftp_epa.pem", package = "amadeus")
    )
  )
})


testthat::test_that("extract_urls returns NULL undefined position.", {
  commands <- paste0(
    "curl -s -o ",
    "/PATH/hms_smoke_Shapefile_20230901.zip --url ",
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/",
    "Shapefile/2023/09/hms_smoke20230901.zip"
    )
  urls <- extract_urls(commands = commands)
  testthat::expect_true(
    is.null(urls)
  )
})

testthat::test_that("check_urls returns NULL undefined size.", {
  urls <- paste0(
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/",
    "Shapefile/2023/09/hms_smoke20230901.zip"
  )
  url_status <- check_urls(urls = urls, method = "HEAD")
  testthat::expect_true(
    is.null(url_status)
  )
})

testthat::test_that("download_hms_data LIVE run.", {
  # function parameters
  date <- "2018-01-01"
  directory <- testthat::test_path("..", "testdata", "hms_live")
  # create file to be deleted
  dir.create(directory)
  file.create(
    paste0(
      directory,
      "/hms_smoke_20180101_20180101_curl_commands.txt"
    )
  )
  # run download function
  download_data(
    dataset_name = "hms",
    date_start = date,
    date_end = date,
    directory_to_save = directory,
    directory_to_download = directory,
    acknowledgement = TRUE,
    download = TRUE,
    unzip = TRUE,
    remove_zip = TRUE,
    remove_command = FALSE
  )
  testthat::expect_true(
    length(list.files(directory)) == 5
  )
  commands <- list.files(directory, pattern = ".txt", full.names = TRUE)
  testthat::expect_true(
    file.exists(commands)
  )
  Sys.sleep(1.5)
  # remove directory
  files <- list.files(directory, full.names = TRUE)
  sapply(files, file.remove)
  file.remove(directory)
})




testthat::test_that("download_cropscape_data throws an error for invalid year", {
  # Set up test data
  invalid_year <- 1996
  testthat::expect_error(download_cropscape_data(year = 2020, source = "CMU"))
  # Call the function and expect an error
  testthat::expect_error(download_cropscape_data(year = invalid_year, source = "GMU"))
  testthat::expect_error(download_cropscape_data(year = 2000, source = "USDA"))
})

testthat::test_that("download_cropscape_data generates correct download commands (GMU)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # Set up test data
  year <- 2010
  directory_to_save <- testthat::test_path("../testdata/")

  # Call the function
  testthat::expect_no_error(
    download_cropscape_data(
      year = year,
      source = "GMU",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "CropScape_CDL_",
    "GMU",
    "_",
    year,
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )

  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 5)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_download = directory_to_save,
                          directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)

})


test_that("download_cropscape_data generates correct download commands (USDA)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # Set up test data
  year <- 2010
  directory_to_save <- testthat::test_path("../testdata/")

  # Call the function
  testthat::expect_no_error(
    download_cropscape_data(
      year = year,
      source = "USDA",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "CropScape_CDL_",
    "USDA",
    "_",
    year,
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )

  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 5)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_download = directory_to_save,
                          directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)

})


testthat::test_that("download_prism_data downloads the correct data files", {
  # Set up test data
  time <- seq(201005, 201012, by = 1)
  element <- c("ppt", "tmin", "tmax", "tmean", "tdmean",
               "vpdmin", "vpdmax")
  # in case of multiple test runs
  # note that PRISM download for the same data element
  # is allowed up to twice a day. IP address could be blocked
  # if the limit is exceeded
  time <- sample(time, 1)
  element <- sample(element, 1)
  data_type <- "ts"
  format <- "nc"
  directory_to_save <- testthat::test_path("..", "testdata/")
  acknowledgement <- TRUE
  download <- FALSE
  remove_command <- FALSE
  unzip <- FALSE

  # Call the function
  download_prism_data(
    time = time,
    element = element,
    data_type = data_type,
    format = format,
    directory_to_save = directory_to_save,
    acknowledgement = acknowledgement,
    download = download,
    remove_command = remove_command,
    unzip = unzip
  )

  testthat::expect_message(
    download_prism_data(
      time = time,
      element = "ppt",
      data_type = "normals",
      format = "asc",
      directory_to_save = directory_to_save,
      acknowledgement = acknowledgement,
      download = download,
      remove_command = TRUE,
      unzip = FALSE
    )
  )

  commands_path <- paste0(
    directory_to_save,
    "PRISM_",
    element,
    "_",
    data_type,
    "_",
    time,
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 6)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_download = directory_to_save,
                          directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)

  # Set up test data
  time <- "202105"
  element <- "soltotal"
  data_type <- "ts"
  format <- "nc"
  directory_to_save <- testthat::test_path("..", "testdata/")
  acknowledgement <- TRUE
  download <- FALSE
  remove_command <- FALSE
  unzip <- TRUE

  # Call the function and expect an error
  testthat::expect_error(download_prism_data(
    time = time,
    element = element,
    data_type = data_type,
    format = format,
    directory_to_save = directory_to_save,
    acknowledgement = acknowledgement,
    download = download,
    remove_command = remove_command,
    unzip = unzip
  ))

})



testthat::test_that("list_stac_files returns a character vector of file links", {
  withr::local_package("rstac")
  # Set up test data
  stac_json <- "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json"
  format <- "tif"
  which <- 64

  # Call the function
  testthat::expect_message(
    result <- list_stac_files(stac_json, format, which)
  )
  # Check the return type
  testthat::expect_true(is.character(result))
  # Check if all elements end with the specified format
  testthat::expect_true(all(grepl(sprintf("%s$", format), result)))

  # string search keyword
  keyword <- "bulkdens"
  testthat::expect_message(
    result1 <- list_stac_files(stac_json, format, keyword)
  )
  testthat::expect_true(is.character(result1))

  # retrieve ids only
  testthat::expect_no_error(
    result2 <- list_stac_files(stac_json, format, keyword, id_only = TRUE)
  )
  testthat::expect_true(is.character(result2))

})


testthat::test_that("download_huc_data works",
  {
    withr::local_package("httr")

    directory_to_save <- testthat::test_path("..", "testdata/")
    allregions <- c("Lower48", "Islands")
    alltypes <- c("Seamless", "OceanCatchment")

    for (region in allregions) {
      for (type in alltypes) {
        testthat::expect_no_error(
          download_huc_data(
            region, type,
            directory_to_save,
            acknowledgement = TRUE,
            download = FALSE,
            unzip = FALSE
          )
        )
        commands_path <- paste0(
          directory_to_save,
          "USGS_NHD_",
          region,
          "_",
          type,
          "_",
          Sys.Date(),
          "_wget_commands.txt"
        )
        # import commands
        commands <- read_commands(commands_path = commands_path)
        # extract urls
        urls <- extract_urls(commands = commands, position = 5)
        # check HTTP URL status
        url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
        # implement unit tests
        test_download_functions(directory_to_download = directory_to_save,
                                directory_to_save = directory_to_save,
                                commands_path = commands_path,
                                url_status = url_status)
        # remove file with commands after test
        file.remove(commands_path)
    
        }
      }
    
      testthat::expect_error(
        download_huc_data(
          "Lower48", "OceanCatchment",
          tempdir(),
          acknowledgement = TRUE,
          download = TRUE,
          unzip = TRUE
        )
      )
  })
