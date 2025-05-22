################################################################################
##### unit and integration tests for EDGAR functions

testthat::test_that("download_edgar (no errors, yearly with sectors)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  download_edgar(
    species = "CO",
    temp_res = "yearly",
    sector_yearly = "ENE",
    year_range = c(2021, 2022),
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    download = FALSE,
    unzip = FALSE
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_yearly_curl_commands.txt"
  )
  commands <- read_commands(commands_path = commands_path)
  urls <- extract_urls(commands = commands, position = 4)
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (monthly, no sector)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  download_edgar(
    species = "SO2",
    temp_res = "monthly",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    download = FALSE,
    unzip = FALSE
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_monthly_curl_commands.txt"
  )
  commands <- read_commands(commands_path = commands_path)
  urls <- extract_urls(commands = commands, position = 4)
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (single year)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  download_edgar(
    species = "NOx",
    temp_res = "yearly",
    sector_yearly = "AGS",
    year_range = 2022,
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    download = FALSE,
    unzip = FALSE
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_yearly_curl_commands.txt"
  )
  commands <- read_commands(commands_path = commands_path)
  urls <- extract_urls(commands = commands, position = 4)
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (invalid year_range length)", {
  expect_error(
    download_edgar(
      species = "CO",
      temp_res = "yearly",
      sector_yearly = "ENE",
      year_range = c(2015, 2016, 2017),
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = paste0(tempdir(), "/e/"),
      unzip = FALSE
    ),
    "year_range must be of length 1 or 2"
  )
})

testthat::test_that("download_edgar (invalid species)", {
  expect_error(
    download_edgar(
      species = "XYZ",
      temp_res = "yearly",
      sector_yearly = "ENE",
      year_range = c(2021, 2022),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/e/"),
      unzip = FALSE
    ),
    regexp = "No valid URLs were constructed"
  )
})

testthat::test_that("download_edgar (incompatible output-format)", {
  expect_error(
    download_edgar(
      species = "CO",
      temp_res = "monthly",
      output = "flx",
      format = "txt",
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/e/"),
      unzip = FALSE
    ),
    regexp = "Output 'flux' is only supported for format 'nc'."
  )
})

testthat::test_that("download_edgar (VOC with sector_voc)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  download_edgar(
    version = "8.1_voc",
    voc = "1",
    sector_voc = "AGRICULTURE",
    year_range = c(2018, 2019),
    acknowledgement = TRUE,
    download = FALSE,
    directory_to_save = directory_to_save,
    unzip = FALSE
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar__curl_commands.txt"
  )
  commands <- read_commands(commands_path = commands_path)
  urls <- extract_urls(commands = commands, position = 4)
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (default year_range)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  download_edgar(
    species = "SO2",
    temp_res = "yearly",
    sector_yearly = "AWB",
    acknowledgement = TRUE,
    download = FALSE,
    directory_to_save = directory_to_save,
    unzip = FALSE
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_yearly_curl_commands.txt"
  )
  commands <- read_commands(commands_path = commands_path)
  urls <- extract_urls(commands = commands, position = 4)
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (missing acknowledgement triggers error)", {
  expect_error(
    download_edgar(
      species = "CO",
      temp_res = "monthly",
      directory_to_save = paste0(tempdir(), "/e/"),
      unzip = FALSE
    ),
    regexp = "acknowledge"
  )
})
