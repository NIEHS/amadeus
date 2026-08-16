write_xis_fixture <- function(path) {
  xis <- expand.grid(
    date = as.Date(c("2019-01-01", "2019-01-02")),
    y = c(0, 1000),
    x = c(0, 1000)
  )
  xis <- xis[order(xis$date, xis$y, xis$x), ]
  xis$map <- "us.pm"
  xis$prediction <- c(1:4, 11:14)
  humid <- xis
  humid$map <- "us.humid"
  humid$prediction <- humid$prediction + 100
  nanoparquet::write_parquet(
    rbind(xis, humid),
    file.path(path, "prediction_maps.parquet")
  )
}

xis_point <- function(site_id = "site_a", x = 0, y = 0) {
  terra::vect(
    data.frame(site_id = site_id, x = x, y = y),
    geom = c("x", "y"),
    crs = "EPSG:2163"
  )
}

testthat::test_that(
  "download_xis(download=FALSE): returns the configured Zenodo endpoint",
  {
    withr::with_tempdir({
      result <- suppressWarnings(amadeus::download_xis(
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      ))

      testthat::expect_identical(
        result$urls,
        paste0(
          "https://zenodo.org/records/17815670/files/",
          "prediction_maps.parquet"
        )
      )
      testthat::expect_identical(
        basename(result$destfiles),
        "prediction_maps.parquet"
      )
      testthat::expect_identical(result$n_files, 1)
    })
  }
)


testthat::test_that(
  "download_xis(download=TRUE): downloads the configured Parquet file",
  {
    captured <- NULL
    testthat::local_mocked_bindings(
      check_destfile = function(...) TRUE,
      download_run_method = function(...) {
        captured <<- list(...)
        list(success = 1, failed = 0, skipped = 0)
      },
      .package = "amadeus"
    )

    withr::with_tempdir({
      result <- amadeus::download_xis(
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    })

    testthat::expect_identical(result$success, 1)
    testthat::expect_identical(
      captured$urls,
      paste0(
        "https://zenodo.org/records/17815670/files/",
        "prediction_maps.parquet"
      )
    )
    testthat::expect_identical(
      basename(captured$destfiles),
      "prediction_maps.parquet"
    )
  }
)

testthat::test_that(
  "download_data(dataset_name='xis'): dispatches to download_xis()",
  {
    testthat::local_mocked_bindings(
      download_xis = function(...) "xis_called",
      .package = "amadeus"
    )

    testthat::expect_identical(
      amadeus::download_data(
        dataset_name = "xis",
        directory_to_save = tempdir(),
        acknowledgement = TRUE
      ),
      "xis_called"
    )
  }
)

testthat::test_that(
  "process_xis(variable='pm',date=range): returns a dated SpatRaster",
  {
    path <- withr::local_tempdir()
    write_xis_fixture(path)

    result <- amadeus::process_xis(
      path = path,
      variable = "pm",
      date = c("2019-01-01", "2019-01-02")
    )

    testthat::expect_s4_class(result, "SpatRaster")
    testthat::expect_equal(terra::nlyr(result), 2)
    testthat::expect_identical(
      names(result),
      c("us.pm_20190101", "us.pm_20190102")
    )
    testthat::expect_equal(
      as.Date(terra::time(result)),
      as.Date(c("2019-01-01", "2019-01-02"))
    )
    testthat::expect_identical(unique(terra::varnames(result)), "us.pm")
    testthat::expect_false(terra::is.lonlat(result))
  }
)

testthat::test_that(
  "process_xis(path=file,variable='humid'): filters map and single date",
  {
    path <- withr::local_tempdir()
    write_xis_fixture(path)

    result <- amadeus::process_xis(
      path = file.path(path, "prediction_maps.parquet"),
      variable = "humid",
      date = "2019-01-01"
    )

    testthat::expect_length(result, 1)
    testthat::expect_identical(names(result), "us.humid_20190101")
    testthat::expect_equal(
      sort(terra::values(result)[, 1]),
      101:104
    )
  }
)

testthat::test_that(
  "process_xis(extent=window): crops the returned XIS raster",
  {
    path <- withr::local_tempdir()
    write_xis_fixture(path)

    result <- amadeus::process_xis(
      path = path,
      variable = "pm",
      date = "2019-01-01",
      extent = c(-1, 1001, -1, 1)
    )

    testthat::expect_equal(terra::ncell(result), 2)
    testthat::expect_equal(sort(terra::values(result)[, 1]), 1:2)
  }
)

testthat::test_that(
  "process_xis(path='missing'): reports that the Parquet file was not found",
  {
    testthat::expect_error(
      amadeus::process_xis(path = tempfile(), variable = "pm"),
      "Could not find exactly one"
    )
  }
)

testthat::test_that(
  "process_xis(date=unavailable): reports an empty selection",
  {
    path <- withr::local_tempdir()
    write_xis_fixture(path)

    testthat::expect_error(
      amadeus::process_xis(
        path = path,
        variable = "pm",
        date = "2020-01-01"
      ),
      "No XIS predictions matched"
    )
  }
)

testthat::test_that(
  "process_covariates(covariate=xis): dispatches to process_xis()",
  {
    testthat::local_mocked_bindings(
      process_xis = function(path, ...) path,
      .package = "amadeus"
    )

    testthat::expect_identical(
      amadeus::process_covariates(covariate = "xis", path = "xis.parquet"),
      "xis.parquet"
    )
  }
)

testthat::test_that(
  "calculate_xis(radius=0): extracts every XIS time layer at a point",
  {
    path <- withr::local_tempdir()
    write_xis_fixture(path)
    xis <- amadeus::process_xis(
      path = path,
      variable = "pm",
      date = c("2019-01-01", "2019-01-02")
    )

    result <- suppressMessages(amadeus::calculate_xis(
      from = xis,
      locs = xis_point(),
      radius = 0
    ))

    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_identical(result$site_id, rep("site_a", 2))
    testthat::expect_equal(result$us.pm_0, c(1, 11))
    testthat::expect_equal(
      as.Date(result$time),
      as.Date(c("2019-01-01", "2019-01-02"))
    )
  }
)

testthat::test_that(
  "calculate_xis(radius=1000): averages raster cells within the buffer",
  {
    path <- withr::local_tempdir()
    write_xis_fixture(path)
    xis <- amadeus::process_xis(
      path = path,
      variable = "pm",
      date = c("2019-01-01", "2019-01-02")
    )

    result <- suppressMessages(amadeus::calculate_xis(
      from = xis,
      locs = xis_point(site_id = "buffered", x = 500, y = 500),
      radius = 1000,
      fun = "mean"
    ))

    testthat::expect_identical(result$site_id, rep("buffered", 2))
    testthat::expect_equal(result$us.pm_1000, c(2.5, 12.5))
  }
)

testthat::test_that(
  "calculate_xis(.by_time=month): summarizes extracted daily values",
  {
    path <- withr::local_tempdir()
    write_xis_fixture(path)
    xis <- amadeus::process_xis(
      path = path,
      variable = "pm",
      date = c("2019-01-01", "2019-01-02")
    )

    result <- suppressMessages(amadeus::calculate_xis(
      from = xis,
      locs = xis_point(),
      .by_time = "month"
    ))

    testthat::expect_equal(nrow(result), 1)
    testthat::expect_equal(result$us.pm_0, 6)
    testthat::expect_equal(as.Date(result$time), as.Date("2019-01-01"))
  }
)

testthat::test_that(
  "calculate_xis(geom=sf): returns extraction locations with geometry",
  {
    path <- withr::local_tempdir()
    write_xis_fixture(path)
    xis <- amadeus::process_xis(
      path = path,
      variable = "pm",
      date = "2019-01-01"
    )

    result <- suppressMessages(amadeus::calculate_xis(
      from = xis,
      locs = xis_point(),
      geom = "sf"
    ))

    testthat::expect_s3_class(result, "sf")
    testthat::expect_equal(nrow(result), 1)
  }
)

testthat::test_that(
  "calculate_xis(from=data.frame): rejects unprocessed XIS data",
  {
    testthat::expect_error(
      amadeus::calculate_xis(
        from = data.frame(),
        locs = data.frame(site_id = 1, lon = 0, lat = 0)
      ),
      "must be a SpatRaster"
    )
  }
)

testthat::test_that(
  "calculate_xis(radius=-1): rejects a negative buffer radius",
  {
    from <- terra::rast(
      nrows = 1,
      ncols = 1,
      vals = 1,
      crs = "EPSG:2163"
    )
    testthat::expect_error(
      amadeus::calculate_xis(
        from = from,
        locs = xis_point(),
        radius = -1
      ),
      "greater than or equal to 0"
    )
  }
)

testthat::test_that(
  "calculate_covariates(covariate=xis): dispatches to calculate_xis()",
  {
    testthat::local_mocked_bindings(
      calculate_xis = function(from, locs, locs_id, ...) locs_id,
      .package = "amadeus"
    )

    testthat::expect_identical(
      amadeus::calculate_covariates(
        covariate = "xis",
        from = data.frame(),
        locs = data.frame(),
        locs_id = "record_id"
      ),
      "record_id"
    )
  }
)
