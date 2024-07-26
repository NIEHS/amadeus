
testthat::test_that(
  "Download OpenLandMap using STAC",
  {
    withr::local_package("rstac")
    links <-
      readRDS(
        system.file("extdata", "openlandmap_assets.rds", package = "amadeus")
      )
    product <- "no2_s5p.l3.trop.tmwm"
    format <- "p50_p90_2km*.*tif"
    directory_to_save <- testthat::test_path("..", "testdata", "olm_temp/")
    acknowledgement <- TRUE
    download <- FALSE

    testthat::expect_no_error(
      download_olm(
        product = product,
        format = format,
        directory_to_save = directory_to_save,
        acknowledgement = acknowledgement,
        download = download,
        remove_command = FALSE
      )
    )

    commands_path <- paste0(
      directory_to_save,
      "OLM_queried_",
      product,
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
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
    unlink(directory_to_save, recursive = TRUE)
  }
)
