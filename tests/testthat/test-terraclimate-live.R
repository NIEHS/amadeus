################################################################################
# Live network tests for download_terraclimate(). Mocked tests: test-terraclimate.R.
################################################################################

expect_terraclimate_live_download <- function(expr, dir) {
  tryCatch(
    expr,
    error = function(e) {
      skip_if_transient_live_issue(e)
      stop(e)
    }
  )
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  testthat::expect_gt(length(files), 0)
  testthat::expect_gt(sum(file.info(files)$size > 0), 0)
}

testthat::test_that(
  paste0(
    "download_terraclimate(variables='ppt', year=c(2022,2022)): ",
    "downloads precipitation file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    expect_terraclimate_live_download(
      amadeus::download_terraclimate(
        variables = "ppt",
        year = c(2022, 2022),
        directory_to_save = dir,
        acknowledgement = TRUE
      ),
      dir
    )
  }
)

testthat::test_that(
  paste0(
    "download_terraclimate(variables='tmax', year=c(2022,2022)): ",
    "downloads maximum temperature file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    expect_terraclimate_live_download(
      amadeus::download_terraclimate(
        variables = "tmax",
        year = c(2022, 2022),
        directory_to_save = dir,
        acknowledgement = TRUE
      ),
      dir
    )
  }
)

testthat::test_that(
  paste0(
    "download_terraclimate(variables='vpd', year=c(2022,2022)): ",
    "downloads vapor pressure deficit file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    expect_terraclimate_live_download(
      amadeus::download_terraclimate(
        variables = "vpd",
        year = c(2022, 2022),
        directory_to_save = dir,
        acknowledgement = TRUE
      ),
      dir
    )
  }
)
