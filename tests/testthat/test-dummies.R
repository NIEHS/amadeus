################################################################################
##### unit and integration tests for Temporal Dummy functions

################################################################################
##### calc_temporal_dummies
testthat::test_that("calc_temporal_dummies (no errors)", {

  site_faux <-
    data.frame(
      site_id = "37031000188101",
      lon = -78.90,
      lat = 35.97,
      time = as.POSIXlt("2022-01-01")
    )

  testthat::expect_no_error(
    dum_res <- calc_temporal_dummies(
      locs = site_faux,
      year = seq(2018L, 2022L)
    )
  )

  # the result is a data frame
  testthat::expect_s3_class(dum_res, "data.frame")
  # ncol is equal to 12 + 5 + 7 + 4
  testthat::expect_equal(ncol(dum_res), 28L)
  # should have each of the indicator groups
  testthat::expect_equal(sum(unlist(dum_res[, -1:-4])), 3L)

  # with geometry
  testthat::expect_no_error(
    dum_res_geom <- calc_temporal_dummies(
      locs = site_faux,
      year = seq(2018L, 2022L),
      geom = TRUE
    )
  )
  testthat::expect_s4_class(dum_res_geom, "SpatVector")

  # error cases
  site_faux_err <- site_faux
  colnames(site_faux_err)[4] <- "date"
  testthat::expect_error(
    dum_res <- calc_temporal_dummies(
      locs = site_faux_err
    )
  )

  testthat::expect_error(
    dum_res <- calc_temporal_dummies(
      locs = as.matrix(site_faux_err)
    )
  )

})

testthat::test_that("calc_temporal_dummies (expected errors)", {
  withr::local_package("terra")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  testthat::expect_error(
    calc_temporal_dummies(
      ncp
    )
  )
  testthat::expect_error(
    calc_temporal_dummies(
      terra::vect(ncp)
    )
  )
})
