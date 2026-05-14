################################################################################
# Live network tests for OpenLandMap (download_olm) and STAC listing.
#
# Migrated from tests/testskip/test-olm.R and tests/testskip/test-stac.R.
# These tests hit real upstream endpoints and are gated by
# `skip_if_no_live_tests()` so they only execute when AMADEUS_LIVE_TESTS is
# set (e.g. by .github/workflows/test-live.yaml).
################################################################################

# download_olm(download=FALSE): writes wget command file and URLs are reachable.
testthat::test_that(
  "download_olm(download=FALSE, remove_command=FALSE): writes commands and URLs respond",
  {
    skip_if_no_live_tests()
    skip_if_pkg_missing("rstac")
    withr::local_package("rstac")

    product <- "no2_s5p.l3.trop.tmwm"
    format <- "p50_p90_2km*.*tif"
    directory_to_save <- withr::local_tempdir()

    testthat::expect_no_error(
      amadeus:::download_olm(
        product = product,
        format = format,
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = FALSE
      )
    )

    commands_path <- file.path(
      directory_to_save,
      paste0("OLM_queried_", product, "_", Sys.Date(), "_wget_commands.txt")
    )
    testthat::expect_true(file.exists(commands_path))
    testthat::expect_gt(file.info(commands_path)$size, 0)

    commands <- read_commands(commands_path = commands_path)
    urls <- extract_urls(commands = commands, position = 5)
    testthat::expect_type(urls, "character")
    testthat::expect_gt(length(urls), 0)

    url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
    testthat::expect_true(any(url_status))
  }
)

# list_stac_files(): retrieves real STAC asset listings.
testthat::test_that(
  "list_stac_files(stac_json, format, which): returns character urls ending in format",
  {
    skip_if_no_live_tests()
    skip_if_pkg_missing("rstac")
    withr::local_package("rstac")

    stac_json <-
      "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json"
    format <- "tif"
    which <- 35

    testthat::expect_message(
      result <- amadeus:::list_stac_files(stac_json, format, which)
    )
    testthat::expect_type(result, "character")
    testthat::expect_true(all(grepl(sprintf("%s$", format), result)))
  }
)

testthat::test_that(
  "list_stac_files(stac_json, format, keyword): filters by keyword",
  {
    skip_if_no_live_tests()
    skip_if_pkg_missing("rstac")
    withr::local_package("rstac")

    stac_json <-
      "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json"
    keyword <- "bulkdens"

    testthat::expect_message(
      result <- amadeus:::list_stac_files(stac_json, "tif", keyword)
    )
    testthat::expect_type(result, "character")
  }
)

testthat::test_that(
  "list_stac_files(id_only=TRUE): returns character ids",
  {
    skip_if_no_live_tests()
    skip_if_pkg_missing("rstac")
    withr::local_package("rstac")

    stac_json <-
      "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json"
    testthat::expect_no_error(
      result <- amadeus:::list_stac_files(
        stac_json, "tif", "bulkdens", id_only = TRUE
      )
    )
    testthat::expect_type(result, "character")
  }
)
