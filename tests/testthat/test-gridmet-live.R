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

# Availability checks are small; full files require a separate opt-in because
# each native multi-year drought NetCDF is approximately 6 GB.
for (index in c("spei", "eddi")) {
  local({
    code <- paste0(index, "30d")
    testthat::test_that(
      sprintf("download_gridmet(variables='%s'): native file is accessible", code),
      {
        skip_if_no_live_tests()
        response <- tryCatch(
          httr2::request(paste0(
            "https://www.northwestknowledge.net/metdata/data/", code, ".nc"
          )) |>
            httr2::req_method("HEAD") |>
            httr2::req_timeout(30) |>
            httr2::req_perform(),
          error = function(e) {
            skip_if_transient_live_issue(e)
            stop(e)
          }
        )
        testthat::expect_equal(httr2::resp_status(response), 200L)
        testthat::expect_gt(
          as.numeric(httr2::resp_header(response, "content-length")), 0
        )
      }
    )
    testthat::test_that(
      sprintf("download_gridmet(variables='%s'): processes and calculates native 4-km pentad data", code),
      {
        skip_if_no_live_tests()
        testthat::skip_if(
          Sys.getenv("AMADEUS_GRIDMET_DROUGHT_LIVE") != "true",
          "Set AMADEUS_GRIDMET_DROUGHT_LIVE=true for multi-GB downloads."
        )
        directory <- Sys.getenv("AMADEUS_GRIDMET_CACHE", unset = "")
        if (!nzchar(directory)) directory <- withr::local_tempdir()
        downloaded <- download_gridmet(
          variables = code, year = 2020, directory_to_save = directory,
          acknowledgement = TRUE, max_tries = 3L
        )
        testthat::expect_equal(downloaded$failed, 0)
        path <- file.path(directory, code)
        testthat::expect_gt(file.size(file.path(path, paste0(code, ".nc"))), 0)
        raster <- process_gridmet(
          variable = code, date = c("2020-01-01", "2020-01-31"), path = path,
          extent = terra::ext(-79, -78.8, 35.8, 36)
        )
        testthat::expect_s4_class(raster, "SpatRaster")
        testthat::expect_equal(terra::res(raster), c(1 / 24, 1 / 24), tolerance = 1e-5)
        testthat::expect_true(terra::same.crs(raster, "EPSG:4326"))
        dates <- as.Date(terra::time(raster))
        testthat::expect_gt(length(dates), 1L)
        testthat::expect_equal(as.numeric(diff(dates)), rep(5, length(dates) - 1L))
        testthat::expect_true(all(dates >= as.Date("2020-01-01") & dates <= as.Date("2020-01-31")))
        loc <- data.frame(id = "001", lon = -78.9, lat = 35.9)
        result <- calculate_gridmet(raster, locs = loc, locs_id = "id", radius = 0)
        testthat::expect_equal(nrow(result), terra::nlyr(raster))
        testthat::expect_identical(as.Date(result$time), dates)
        column <- setdiff(names(result), c("id", "time"))
        testthat::expect_length(column, 1L)
        testthat::expect_match(column, code)
        testthat::expect_true(all(is.finite(result[[column]])))
        expected <- as.numeric(unlist(
          terra::extract(raster, as.matrix(loc[c("lon", "lat")])),
          use.names = FALSE
        ))
        testthat::expect_equal(result[[column]], expected)
      }
    )
  })
}
