
testthat::test_that("list_stac_files returns character vector of file links", {
  withr::local_package("rstac")
  # Set up test data
  stac_json <-
    "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json"
  format <- "tif"
  which <- 35

  # Call the function
  testthat::expect_message(
    result <- list_stac_files(stac_json, format, which)
  )
  # Check the return type
  testthat::expect_true(is.character(result))
  # Check if all elements end with the specified format
  testthat::expect_true(all(grepl(sprintf("%s$", format), result)))

  # string search keyword
  keyword <- "bulkdens"
  testthat::expect_message(
    result1 <- list_stac_files(stac_json, format, keyword)
  )
  testthat::expect_true(is.character(result1))

  # retrieve ids only
  testthat::expect_no_error(
    result2 <- list_stac_files(stac_json, format, keyword, id_only = TRUE)
  )
  testthat::expect_true(is.character(result2))

})
