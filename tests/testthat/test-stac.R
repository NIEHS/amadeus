################################################################################
##### unit and integration tests for rstac listing functions

################################################################################
##### list_stac_files
testthat::test_that("list_stac_files", {
  withr::local_package("rstac")
  # Set up test data
  stac_json <-
    "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json"
  format <- "tif"
  which <- 35

  # Call the function
  testthat::expect_message(
    result <- amadeus:::list_stac_files(stac_json, format, which)
  )
  # Check the return type
  testthat::expect_true(is.character(result))
  # Check if all elements end with the specified format
  testthat::expect_true(all(grepl(sprintf("%s$", format), result)))

  # string search keyword
  keyword <- "bulkdens"
  testthat::expect_message(
    result1 <- amadeus:::list_stac_files(stac_json, format, keyword)
  )
  testthat::expect_true(is.character(result1))

  # retrieve ids only
  testthat::expect_no_error(
    result2 <- amadeus:::list_stac_files(stac_json, format, keyword, id_only = TRUE)
  )
  testthat::expect_true(is.character(result2))

})
