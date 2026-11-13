################################################################################
# Unit tests for the packge startup functions and auxiliaries.

################################################################################
testthat::test_that(".amadeus", {
  # ----------------------------------------------------------------------------
  # Access the private package environment.
  env_amadeus <- amadeus:::.amadeus
  # ----------------------------------------------------------------------------
  # Verify that package data are stored in an environment.
  testthat::expect_true(is.environment(env_amadeus))
  # ----------------------------------------------------------------------------
  # Verify that the environment has no inherited bindings.
  testthat::expect_identical(parent.env(env_amadeus), emptyenv())
  # ----------------------------------------------------------------------------
  # Verify that the load hook initialized the CRS lookup as a list.
  testthat::expect_s3_class(env_amadeus$df_merra2, "data.frame")
})

################################################################################
testthat::test_that(".onLoad", {
  # ----------------------------------------------------------------------------
  # Create isolated storage so this test cannot change real package data.
  env_empty <- new.env(parent = emptyenv())
  # ----------------------------------------------------------------------------
  # Temporarily direct the load hook to the isolated environment.
  testthat::local_mocked_bindings(
    .amadeus = env_empty,
    .package = "amadeus"
  )
  # ----------------------------------------------------------------------------
  # Locate the packaged lookup file.
  chr_path <- system.file(
    "extdata",
    "df_merra2.csv",
    package = "amadeus",
    mustWork = TRUE
  )
  # ----------------------------------------------------------------------------
  # Read the expected lookup independently.
  df_merra2 <- read.csv(chr_path)
  # ----------------------------------------------------------------------------
  # Run the hook and verify its documented invisible return.
  # NULL lets system.file() use its default package lookup.
  testthat::expect_invisible(
    amadeus:::.onLoad(libname = NULL, pkgname = "amadeus")
  )
  # ----------------------------------------------------------------------------
  # Verify that the hook populated the environment with the complete lookup.
  testthat::expect_identical(.amadeus$df_merra2, df_merra2)
})
