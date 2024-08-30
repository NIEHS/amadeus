################################################################################
##### unit and integration tests for NASA MERRA2 functions

##### download_merra2
testthat::test_that("download_merra2 (no errors)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  date_start <- "2022-02-14"
  date_end <- "2022-03-08"
  collections <- c("inst1_2d_asm_Nx", "inst3_3d_asm_Np")
  directory_to_save <- paste0(tempdir(), "/merra2/")
  # run download function
  testthat::expect_no_error(
    download_data(dataset_name = "merra2",
                  date = c(date_start, date_end),
                  collection = collections,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE)
  )
  # define path with commands
  commands_path <- paste0(directory_to_save,
                          "merra2_",
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
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_merra2 (expected errors)", {
  # expected error due to unrecognized collection
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

##### process_merra2
testthat::test_that("process_merra2", {
  withr::local_package("terra")
  #* indicates three dimensional data that has subset to single
  #* pressure level for test data set
  collection <- c(
    "inst1_2d_int_Nx", "inst3_2d_gas_Nx", "inst3_3d_chm_Nv", #*
    "inst6_3d_ana_Np", #*
    "statD_2d_slv_Nx", "tavg1_2d_chm_Nx", "tavg3_3d_udt_Np" #*
  )
  variable <- c(
    "CPT", "AODANA", "AIRDENS", #*
    "SLP", #*
    "HOURNORAIN", "COCL", "DUDTANA" #*
  )
  merra2_df <- data.frame(collection, variable)
  # expect function
  expect_true(
    is.function(process_merra2)
  )
  for (c in seq_along(merra2_df$collection)) {
    merra2 <-
      process_merra2(
        date = c("2018-01-01", "2018-01-01"),
        variable = merra2_df$variable[c],
        path =
        testthat::test_path(
          "..",
          "testdata",
          "merra2",
          merra2_df$collection[c]
        )
      )
    # expect output is SpatRaster
    expect_true(
      class(merra2)[1] == "SpatRaster"
    )
    # expect values
    expect_true(
      terra::hasValues(merra2)
    )
    # expect non-null coordinate reference system
    expect_false(
      terra::crs(merra2) == ""
    )
    # expect lon and lat dimensions to be > 1
    expect_false(
      any(c(0, 1) %in% dim(merra2)[1:2])
    )
    # expect non-numeric and non-empty time
    expect_false(
      any(c("", 0) %in% terra::time(merra2))
    )
    # expect time dimension is POSIXt for hourly
    expect_true(
      "POSIXt" %in% class(terra::time(merra2))
    )
    # expect seconds in time information
    expect_true(
      "seconds" %in% terra::timeInfo(merra2)
    )
    # expect 8 levels for 3 hourly data
    expect_true(
      all(dim(merra2) == c(2, 3, 1))
    )
  }
  class(merra2)
  # test with cropping extent
  testthat::expect_no_error(
    merra2_ext <- process_merra2(
      date = c("2018-01-01", "2018-01-01"),
      variable = "CPT",
      path =
        testthat::test_path(
          "..",
          "testdata",
          "merra2",
          "inst1_2d_int_Nx"
        ),
      extent = terra::ext(merra2)
    )
  )
})

##### calc_merra2
testthat::test_that("calc_merra2", {
  withr::local_package("terra")
  withr::local_package("data.table")
  #* indicates three dimensional data that has subset to single
  #* pressure level for test data set
  collections <- c(
    "inst1_2d_int_Nx", "inst3_2d_gas_Nx", "inst3_3d_chm_Nv", #*
    "inst6_3d_ana_Np", #*
    "statD_2d_slv_Nx", "tavg1_2d_chm_Nx", "tavg3_3d_udt_Np" #*
  )
  variables <- c(
    "CPT", "AODANA", "AIRDENS", #*
    "SLP", #*
    "HOURNORAIN", "COCL", "DUDTANA" #*
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calc_merra2)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    variable <- variables[c]
    for (r in seq_along(radii)) {
      merra2 <-
        process_merra2(
          date = c("2018-01-01", "2018-01-01"),
          variable = variable,
          path =
          testthat::test_path(
            "..",
            "testdata",
            "merra2",
            collection
          )
        )
      merra2_covariate <-
        calc_merra2(
          from = merra2,
          locs = data.table::data.table(ncp),
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      merra2_covariate <- calc_setcolumns(
        from = merra2_covariate,
        lag = 0,
        dataset = "merra2",
        locs_id = "site_id"
      )
      # expect output is data.frame
      expect_true(
        class(merra2_covariate) == "data.frame"
      )
      if (grepl("lev", names(merra2)[1])) {
        # expect 4 columns
        expect_true(
          ncol(merra2_covariate) == 4
        )
        # expect numeric value
        expect_true(
          class(merra2_covariate[, 4]) == "numeric"
        )
      } else {
        # expect 3 columns
        expect_true(
          ncol(merra2_covariate) == 3
        )
        # expect numeric value
        expect_true(
          class(merra2_covariate[, 3]) == "numeric"
        )
      }
      # expect $time is class Date
      expect_true(
        "POSIXt" %in% class(merra2_covariate$time)
      )
    }
  }
  # with included geometry
  testthat::expect_no_error(
    merra2_covariate_geom <- calc_merra2(
      from = merra2,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(merra2_covariate_geom), 4
  )
  testthat::expect_true(
    "SpatVector" %in% class(merra2_covariate_geom)
  )
})
