################################################################################
##### unit and integration tests for EDGAR functions
testthat::test_that("download_edgar (no errors, yearly with sectors)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_no_error(
    amadeus::download_edgar(
      species = "CO",
      temp_res = "yearly",
      sector_yearly = "ENE",
      year_range = c(2021, 2022),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      unzip = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_yearly_curl_commands.txt"
  )
  commands <- amadeus::read_commands(commands_path = commands_path)
  urls <- amadeus::extract_urls(commands = commands, position = 4)
  url_status <- amadeus::check_urls(
    urls = urls,
    size = length(urls),
    method = "HEAD"
  )
  amadeus::test_download_functions(
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
  testthat::expect_no_error(
    amadeus::download_edgar(
      species = "SO2",
      temp_res = "monthly",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      unzip = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_monthly_curl_commands.txt"
  )
  commands <- amadeus::read_commands(commands_path = commands_path)
  urls <- amadeus::extract_urls(commands = commands, position = 4)
  url_status <- amadeus::check_urls(
    urls = urls,
    size = length(urls),
    method = "HEAD"
  )
  amadeus::test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (monthly, w/sector)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_no_error(
    amadeus::download_edgar(
      species = "SO2",
      temp_res = "monthly",
      sector_monthly = "BUILDINGS",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      unzip = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_monthly_curl_commands.txt"
  )
  commands <- amadeus::read_commands(commands_path = commands_path)
  urls <- amadeus::extract_urls(commands = commands, position = 4)
  url_status <- amadeus::check_urls(
    urls = urls,
    size = length(urls),
    method = "HEAD"
  )
  amadeus::test_download_functions(
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
  testthat::expect_no_error(
    amadeus::download_edgar(
      species = "NOx",
      temp_res = "yearly",
      sector_yearly = "AGS",
      year_range = 2022,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      unzip = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_yearly_curl_commands.txt"
  )
  commands <- amadeus::read_commands(commands_path = commands_path)
  urls <- amadeus::extract_urls(commands = commands, position = 4)
  url_status <- amadeus::check_urls(
    urls = urls,
    size = length(urls),
    method = "HEAD"
  )
  amadeus::test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (invalid year_range length)", {
  testthat::expect_error(
    amadeus::download_edgar(
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
  testthat::expect_error(
    amadeus::download_edgar(
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
  testthat::expect_error(
    amadeus::download_edgar(
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

  testthat::expect_error(
    amadeus::download_edgar(
      species = "CO",
      temp_res = "monthly",
      output = "nc",
      format = "txt",
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/e/"),
      unzip = FALSE
    )
  )
})

testthat::test_that("download_edgar (VOC with sector_voc)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_no_error(
    amadeus::download_edgar(
      version = "8.1_voc",
      voc = "1",
      sector_voc = "AGRICULTURE",
      year_range = c(2018, 2019),
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = directory_to_save,
      unzip = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar__curl_commands.txt"
  )
  commands <- amadeus::read_commands(commands_path = commands_path)
  urls <- amadeus::extract_urls(commands = commands, position = 4)
  url_status <- amadeus::check_urls(
    urls = urls,
    size = length(urls),
    method = "HEAD"
  )
  amadeus::test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (VOC w/out year_range)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_no_error(
    amadeus::download_edgar(
      version = "8.1_voc",
      voc = "1",
      sector_voc = "AGRICULTURE",
      year_range = NULL,
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = directory_to_save,
      unzip = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar__curl_commands.txt"
  )
  commands <- amadeus::read_commands(commands_path = commands_path)
  urls <- amadeus::extract_urls(commands = commands, position = 4)
  url_status <- amadeus::check_urls(
    urls = urls,
    size = length(urls),
    method = "HEAD"
  )
  amadeus::test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (VOC w/out sector_voc)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_no_error(
    amadeus::download_edgar(
      version = "8.1_voc",
      voc = "1",
      sector_voc = NULL,
      year_range = NULL,
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = directory_to_save,
      unzip = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar__curl_commands.txt"
  )
  commands <- amadeus::read_commands(commands_path = commands_path)
  urls <- amadeus::extract_urls(commands = commands, position = 4)
  url_status <- amadeus::check_urls(
    urls = urls,
    size = length(urls),
    method = "HEAD"
  )
  amadeus::test_download_functions(
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
  testthat::expect_no_error(
    amadeus::download_edgar(
      species = "SO2",
      temp_res = "yearly",
      sector_yearly = "AWB",
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = directory_to_save,
      unzip = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_yearly_curl_commands.txt"
  )
  commands <- amadeus::read_commands(commands_path = commands_path)
  urls <- amadeus::extract_urls(commands = commands, position = 4)
  url_status <- amadeus::check_urls(
    urls = urls,
    size = length(urls),
    method = "HEAD"
  )
  amadeus::test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (NULL sector_yearly)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_no_error(
    amadeus::download_edgar(
      species = "SO2",
      temp_res = "yearly",
      sector_yearly = NULL,
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = directory_to_save,
      unzip = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_yearly_curl_commands.txt"
  )
  commands <- amadeus::read_commands(commands_path = commands_path)
  urls <- amadeus::extract_urls(commands = commands, position = 4)
  url_status <- amadeus::check_urls(
    urls = urls,
    size = length(urls),
    method = "HEAD"
  )
  amadeus::test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (NULL sector_yearly + year_range)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_no_error(
    amadeus::download_edgar(
      species = "SO2",
      temp_res = "yearly",
      sector_yearly = NULL,
      year_range = c(2018, 2019),
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = directory_to_save,
      unzip = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_yearly_curl_commands.txt"
  )
  commands <- amadeus::read_commands(commands_path = commands_path)
  urls <- amadeus::extract_urls(commands = commands, position = 4)
  url_status <- amadeus::check_urls(
    urls = urls,
    size = length(urls),
    method = "HEAD"
  )
  amadeus::test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (NULL sector_yearly + year_range)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_warning(
    amadeus::download_edgar(
      species = "SO2",
      temp_res = "yearly",
      sector_yearly = NULL,
      year_range = c(2018, 2030), # some invalid
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = directory_to_save,
      unzip = FALSE
    )
  )
})

testthat::test_that("download_edgar (timeseries)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_no_error(
    amadeus::download_edgar(
      species = "SO2",
      temp_res = "timeseries",
      sector_yearly = "AWB",
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = directory_to_save,
      unzip = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "/edgar_timeseries_curl_commands.txt"
  )
  commands <- amadeus::read_commands(commands_path = commands_path)
  urls <- amadeus::extract_urls(commands = commands, position = 4)
  url_status <- amadeus::check_urls(
    urls = urls,
    size = length(urls),
    method = "HEAD"
  )
  amadeus::test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (missing acknowledgement triggers error)", {
  testthat::expect_error(
    amadeus::download_edgar(
      species = "CO",
      temp_res = "monthly",
      directory_to_save = paste0(tempdir(), "/e/"),
      unzip = FALSE
    ),
    regexp = "acknowledge"
  )
})

testthat::test_that("download_edgar (bad version)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_error(
    amadeus::download_edgar(
      version = "unacceptable version",
      voc = "1",
      sector_voc = "AGRICULTURE",
      year_range = c(2018, 2019),
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = directory_to_save,
      unzip = FALSE
    )
  )
})

testthat::test_that("download_edgar (bad year)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_error(
    amadeus::download_edgar(
      species = "SO2",
      temp_res = "yearly",
      sector_yearly = NULL,
      year_range = c(2018, 2019, 2022),
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = directory_to_save,
      unzip = FALSE
    )
  )
})

testthat::test_that("download_edgar (bad temp_res)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  testthat::expect_error(
    amadeus::download_edgar(
      species = "SO2",
      temp_res = "not_recognized",
      sector_yearly = NULL,
      year_range = c(2018, 2019),
      acknowledgement = TRUE,
      download = FALSE,
      directory_to_save = directory_to_save,
      unzip = FALSE
    )
  )
})
