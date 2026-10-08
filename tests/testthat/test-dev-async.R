################################################################################
# Development tests for async implementation with {mirai} and {mori}.
################################################################################
testthat::test_that(
  paste0(
    "download_narr_map(variables='air.sfc', year=c(2022,2022)): ",
    "downloads non-empty file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    mirai::daemons(1)
    download_narr_map(
      variables = "air.sfc",
      year = c(2022, 2022),
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    mirai::daemons(0)
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

################################################################################
testthat::test_that("calculate_narr_mirai", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  testthat::expect_true(
    is.function(calculate_narr_mirai)
  )
  mirai::daemons(1)
  for (v in seq_along(variables)) {
    variable <- variables[v]
    for (r in seq_along(radii)) {
      narr <-
        amadeus::process_narr(
          date = "2018-01-01",
          variable = variable,
          path = testthat::test_path(
            "..",
            "testdata",
            "narr",
            variable
          )
        )
      narr_covariate <-
        calculate_narr_mirai(
          from = narr,
          locs = ncp,
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      narr_covariate <- amadeus::calc_setcolumns(
        from = narr_covariate,
        lag = 0,
        dataset = "narr",
        locs_id = "site_id"
      )
      # expect output is data.frame
      testthat::expect_true(
        class(narr_covariate) == "data.frame"
      )
      if (variable == "weasd") {
        # expect 3 columns (no pressure level)
        testthat::expect_true(
          ncol(narr_covariate) == 3
        )
        # expect numeric value
        testthat::expect_true(
          class(narr_covariate[, 3]) == "numeric"
        )
      } else {
        # expect 4 columns
        testthat::expect_true(
          ncol(narr_covariate) == 4
        )
        # expect numeric value
        testthat::expect_true(
          class(narr_covariate[, 4]) == "numeric"
        )
      }
      # expect $time is class Date
      testthat::expect_true(
        "POSIXct" %in% class(narr_covariate$time)
      )
    }
  }
  # with geometry terra
  testthat::expect_no_error(
    narr_covariate_terra <- calculate_narr_mirai(
      from = narr,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "terra"
    )
  )
  testthat::expect_equal(
    ncol(narr_covariate_terra),
    4 # 4 columns because omega has pressure levels
  )
  testthat::expect_true(
    "SpatVector" %in% class(narr_covariate_terra)
  )
  # with geometry sf
  testthat::expect_no_error(
    narr_covariate_sf <- calculate_narr_mirai(
      from = narr,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "sf"
    )
  )
  testthat::expect_equal(
    ncol(narr_covariate_sf),
    5 # 5 columns because omega has pressure levels
  )
  testthat::expect_true(
    "sf" %in% class(narr_covariate_sf)
  )

  testthat::expect_error(
    calculate_narr_mirai(
      from = narr,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  mirai::daemons(0)
})

testthat::test_that("calculate_narr_mirai supports .by_time summaries", {
  withr::local_package("terra")
  locs <- data.frame(
    lon = -78.8277,
    lat = 35.95013,
    site_id = "3799900018810101"
  )
  narr <- amadeus::process_narr(
    date = "2018-01-01",
    variable = "omega",
    path = testthat::test_path("..", "testdata", "narr", "omega")
  )

  mirai::daemons(1)
  by_time <- calculate_narr_mirai(
    from = narr,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    .by_time = "day",
    fun = "mean"
  )
  mirai::daemons(0)

  testthat::expect_true("time" %in% names(by_time))
  testthat::expect_s3_class(by_time$time, "POSIXct")
  testthat::expect_true("level" %in% names(by_time))
})

testthat::test_that("calculate_narr_mirai errors when deprecated .by is supplied", {
  withr::local_package("terra")
  locs <- data.frame(
    lon = -78.8277,
    lat = 35.95013,
    site_id = "3799900018810101"
  )
  narr <- amadeus::process_narr(
    date = "2018-01-01",
    variable = "omega",
    path = testthat::test_path("..", "testdata", "narr", "omega")
  )
  mirai::daemons(1)

  testthat::expect_error(
    calculate_narr_mirai(
      from = narr,
      locs = locs,
      locs_id = "site_id",
      radius = 0,
      .by = "day",
      fun = "mean"
    ),
    regexp = "no longer supported"
  )
  mirai::daemons(0)
})
