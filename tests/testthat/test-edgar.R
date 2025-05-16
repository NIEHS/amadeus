################################################################################
##### unit and integration tests for EDGAR functions
# nolint start
################################################################################
##### download_edgar
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
        download = FALSE
    )
    commands_path <- paste0(
        directory_to_save,
        "/edgar_yearly_curl_commands.txt"
    )
    commands <- read_commands(commands_path = commands_path)
    urls <- extract_urls(commands = commands, position = 6)
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
        download = FALSE
    )
    commands_path <- paste0(
        directory_to_save,
        "/edgar_monthly_curl_commands.txt"
    )
    commands <- read_commands(commands_path = commands_path)
    urls <- extract_urls(commands = commands, position = 6)
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
        sector_yearly = "IND",
        year_range = c(2022, 2022),
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE
    )
    commands_path <- paste0(
        directory_to_save,
        "/edgar_yearly_curl_commands.txt"
    )
    commands <- read_commands(commands_path = commands_path)
    urls <- extract_urls(commands = commands, position = 6)
    url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
    test_download_functions(
        directory_to_save = directory_to_save,
        commands_path = commands_path,
        url_status = url_status
    )
    file.remove(commands_path)
    unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (invalid species)", {
    expect_error(
        download_edgar(
            species = "XYZ",
            temp_res = "yearly",
            sector_yearly = "ENE",
            year_range = c(2021, 2022),
            acknowledgement = TRUE,
            directory_to_save = paste0(tempdir(), "/e/")
        )
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
            directory_to_save = paste0(tempdir(), "/e/")
        )
    )
})

testthat::test_that("download_edgar (VOC with sector_voc)", {
    withr::local_package("httr")
    withr::local_package("stringr")
    directory_to_save <- paste0(tempdir(), "/edgar/")
    download_edgar(
        species = "VOC",
        temp_res = "yearly",
        sector_voc = "solvents",
        year_range = c(2018, 2019),
        acknowledgement = TRUE,
        download = FALSE,
        directory_to_save = directory_to_save
    )
    commands_path <- paste0(
        directory_to_save,
        "/edgar_yearly_curl_commands.txt"
    )
    commands <- read_commands(commands_path = commands_path)
    urls <- extract_urls(commands = commands, position = 6)
    url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
    test_download_functions(
        directory_to_save = directory_to_save,
        commands_path = commands_path,
        url_status = url_status
    )
    file.remove(commands_path)
    unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (VOC without sector_voc should error)", {
    expect_error(
        download_edgar(
            species = "VOC",
            temp_res = "yearly",
            year_range = c(2020, 2021),
            acknowledgement = TRUE,
            directory_to_save = paste0(tempdir(), "/e/")
        )
    )
})

testthat::test_that("download_edgar (default year_range)", {
    withr::local_package("httr")
    withr::local_package("stringr")
    directory_to_save <- paste0(tempdir(), "/edgar/")
    download_edgar(
        species = "SO2",
        temp_res = "yearly",
        sector_yearly = "TRA",
        acknowledgement = TRUE,
        download = FALSE,
        directory_to_save = directory_to_save
    )
    commands_path <- paste0(
        directory_to_save,
        "/edgar_yearly_curl_commands.txt"
    )
    commands <- read_commands(commands_path = commands_path)
    urls <- extract_urls(commands = commands, position = 6)
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
            directory_to_save = paste0(tempdir(), "/e/")
        )
    )
})
