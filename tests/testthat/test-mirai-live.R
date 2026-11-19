################################################################################
##### Live integration tests for mirai-enabled download functions.

################################################################################
testthat::test_that("download_hms; sequential and mirai", {
  # ----------------------------------------------------------------------------
  # Common arguments
  chr_dates <- c("2022-06-01", "2022-06-10")
  # ----------------------------------------------------------------------------
  # Sequential download.
  chr_tempdir1 <- file.path(tempdir(), "hms1")
  testthat::expect_no_error(
    time_hms1 <- system.time({
      list_hms1 <- amadeus::download_hms(
        date = chr_dates,
        directory_to_save = chr_tempdir1,
        acknowledgement = TRUE
      )
    })
  )
  # ----------------------------------------------------------------------------
  # Concurrent download.
  chr_tempdir2 <- file.path(tempdir(), "hms2")
  testthat::expect_no_error(
    time_hms2 <- system.time({
      mirai::daemons(2)
      list_hms2 <- amadeus::download_hms(
        date = chr_dates,
        directory_to_save = chr_tempdir2,
        acknowledgement = TRUE,
        rate_limit = 2 / 2
      )
      mirai::daemons(0)
    })
  )
  # ----------------------------------------------------------------------------
  # Identical output.
  chr_files1 <- list.files(chr_tempdir1, recursive = TRUE)
  chr_files2 <- list.files(chr_tempdir2, recursive = TRUE)
  testthat::expect_gt(length(chr_files1), 0L)
  testthat::expect_identical(chr_files1, chr_files2)
  testthat::expect_identical(list_hms1, list_hms2)
  # ----------------------------------------------------------------------------
  # Identical file contents, indexed by relative paths across both directories.
  chr_hashes1 <- stats::setNames(
    tools::md5sum(file.path(chr_tempdir1, chr_files1)),
    chr_files1
  )
  chr_hashes2 <- stats::setNames(
    tools::md5sum(file.path(chr_tempdir2, chr_files2)),
    chr_files2
  )
  testthat::expect_false(anyNA(chr_hashes1))
  testthat::expect_false(anyNA(chr_hashes2))
  testthat::expect_identical(chr_hashes1, chr_hashes2)
  # ----------------------------------------------------------------------------
  # Improved concurrent runtime is strictly less than sequential.
  testthat::expect_lt(time_hms2["elapsed"], time_hms1["elapsed"])
  # ----------------------------------------------------------------------------
  # Cleanup.
  file.remove(list.files(chr_tempdir1, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir1, recursive = TRUE)
  file.remove(list.files(chr_tempdir2, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir2, recursive = TRUE)
})

################################################################################
testthat::test_that("download_narr; sequential and mirai", {
  # ----------------------------------------------------------------------------
  # Common arguments
  int_years <- 2022:2024
  # ----------------------------------------------------------------------------
  # Sequential download.
  chr_tempdir1 <- file.path(tempdir(), "narr1")
  testthat::expect_no_error(
    time_narr1 <- system.time({
      list_narr1 <- amadeus::download_narr(
        variable = "air.sfc",
        year = int_years,
        directory_to_save = chr_tempdir1,
        acknowledgement = TRUE
      )
    })
  )
  # ----------------------------------------------------------------------------
  # Concurrent download.
  chr_tempdir2 <- file.path(tempdir(), "narr2")
  testthat::expect_no_error(
    time_narr2 <- system.time({
      mirai::daemons(3)
      list_narr2 <- amadeus::download_narr(
        variable = "air.sfc",
        year = int_years,
        directory_to_save = chr_tempdir2,
        acknowledgement = TRUE,
        rate_limit = 2 / 3
      )
      mirai::daemons(0)
    })
  )
  # ----------------------------------------------------------------------------
  # Identical output.
  chr_files1 <- list.files(chr_tempdir1, recursive = TRUE)
  chr_files2 <- list.files(chr_tempdir2, recursive = TRUE)
  testthat::expect_gt(length(chr_files1), 0L)
  testthat::expect_identical(chr_files1, chr_files2)
  testthat::expect_identical(list_narr1, list_narr2)
  # ----------------------------------------------------------------------------
  # Identical file contents, indexed by relative paths across both directories.
  chr_hashes1 <- stats::setNames(
    tools::md5sum(file.path(chr_tempdir1, chr_files1)),
    chr_files1
  )
  chr_hashes2 <- stats::setNames(
    tools::md5sum(file.path(chr_tempdir2, chr_files2)),
    chr_files2
  )
  testthat::expect_false(anyNA(chr_hashes1))
  testthat::expect_false(anyNA(chr_hashes2))
  testthat::expect_identical(chr_hashes1, chr_hashes2)
  # ----------------------------------------------------------------------------
  # Improved concurrent runtime is strictly less than sequential.
  testthat::expect_lt(time_narr2["elapsed"], time_narr1["elapsed"])
  # ----------------------------------------------------------------------------
  # Cleanup.
  file.remove(list.files(chr_tempdir1, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir1, recursive = TRUE)
  file.remove(list.files(chr_tempdir2, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir2, recursive = TRUE)
})

################################################################################
testthat::test_that("download_merra2; sequential and mirai", {
  # ----------------------------------------------------------------------------
  # Common arguments
  chr_dates <- c("2022-06-01", "2022-06-04")
  # ----------------------------------------------------------------------------
  # Sequential download.
  chr_tempdir1 <- file.path(tempdir(), "merra21")
  testthat::expect_no_error(
    time_merra21 <- system.time({
      list_merra21 <- amadeus::download_merra2(
        collection = "tavg1_2d_chm_Nx",
        date = chr_dates,
        directory_to_save = chr_tempdir1,
        acknowledgement = TRUE
      )
    })
  )
  # ----------------------------------------------------------------------------
  # Concurrent download.
  chr_tempdir2 <- file.path(tempdir(), "merra22")
  testthat::expect_no_error(
    time_merra22 <- system.time({
      mirai::daemons(2)
      list_merra22 <- amadeus::download_merra2(
        collection = "tavg1_2d_chm_Nx",
        date = chr_dates,
        directory_to_save = chr_tempdir2,
        acknowledgement = TRUE,
        rate_limit = 2 / 2
      )
      mirai::daemons(0)
    })
  )
  # ----------------------------------------------------------------------------
  # Identical output.
  chr_files1 <- list.files(chr_tempdir1, recursive = TRUE)
  chr_files2 <- list.files(chr_tempdir2, recursive = TRUE)
  testthat::expect_gt(length(chr_files1), 0L)
  testthat::expect_identical(chr_files1, chr_files2)
  testthat::expect_identical(list_merra21, list_merra22)
  # ----------------------------------------------------------------------------
  # Identical file contents, indexed by relative paths across both directories.
  chr_hashes1 <- stats::setNames(
    tools::md5sum(file.path(chr_tempdir1, chr_files1)),
    chr_files1
  )
  chr_hashes2 <- stats::setNames(
    tools::md5sum(file.path(chr_tempdir2, chr_files2)),
    chr_files2
  )
  testthat::expect_false(anyNA(chr_hashes1))
  testthat::expect_false(anyNA(chr_hashes2))
  testthat::expect_identical(chr_hashes1, chr_hashes2)
  # ----------------------------------------------------------------------------
  # Improved concurrent runtime is strictly less than sequential.
  testthat::expect_lt(time_merra22["elapsed"], time_merra21["elapsed"])
  # ----------------------------------------------------------------------------
  # Cleanup.
  file.remove(list.files(chr_tempdir1, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir1, recursive = TRUE)
  file.remove(list.files(chr_tempdir2, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir2, recursive = TRUE)
})
