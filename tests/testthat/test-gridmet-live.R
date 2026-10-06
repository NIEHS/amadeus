################################################################################
# Live network tests for download_gridmet(). Mocked tests: test-gridmet.R.
################################################################################

expect_gridmet_live_download <- function(expr, dir, expected_files = NULL) {
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
  if (!is.null(expected_files)) {
    testthat::expect_setequal(
      basename(files),
      expected_files
    )
  }
}

testthat::test_that(
  paste0(
    "download_gridmet(variables='pr', year=c(2022,2022)): ",
    "downloads precipitation file"
    #"AMADEUS_LIVE_TESTS=true",
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    expect_gridmet_live_download(
      amadeus::download_gridmet(
        variables = "pr",
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
    "download_gridmet(variables='tmmx', year=c(2022,2022)): ",
    "downloads maximum temperature file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    expect_gridmet_live_download(
      amadeus::download_gridmet(
        variables = "tmmx",
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
    "download_gridmet(variables='vs', year=c(2022,2022)): ",
    "downloads wind speed file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    expect_gridmet_live_download(
      amadeus::download_gridmet(
        variables = "vs",
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
    "download_gridmet(variables='pr', year=2022): ",
    "expands a scalar year and downloads one precipitation file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    expect_gridmet_live_download(
      amadeus::download_gridmet(
        variables = "pr",
        year = 2022,
        directory_to_save = dir,
        acknowledgement = TRUE
      ),
      dir,
      expected_files = "pr_2022.nc"
    )
  }
)

testthat::test_that(
  paste0(
    "download_gridmet(variables=c('pr','tmmn'), year=c(2022,2022)): ",
    "downloads one file for each requested variable"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    expect_gridmet_live_download(
      amadeus::download_gridmet(
        variables = c("pr", "tmmn"),
        year = c(2022, 2022),
        directory_to_save = dir,
        acknowledgement = TRUE
      ),
      dir,
      expected_files = c("pr_2022.nc", "tmmn_2022.nc")
    )
  }
)
