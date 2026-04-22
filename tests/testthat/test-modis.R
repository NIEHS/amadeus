################################################################################
##### unit and integration tests for NASA MODIS functions
# nolint start

skip_if_not_local_modis_live <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_offline()

  if (Sys.getenv("NASA_EARTHDATA_TOKEN") == "") {
    testthat::skip("NASA_EARTHDATA_TOKEN not set")
  }
}

################################################################################
##### download_modis
testthat::test_that("download_modis (MODIS-MOD09GA)", {
  skip_if_not_local_modis_live()

  withr::local_package("httr2")
  withr::local_package("stringi")
  withr::local_package("jsonlite")

  # function parameters
  years <- 2020
  product <- "MOD09GA"
  version <- "061"
  directory_to_save <- paste0(tempdir(), "/mod/")

  for (y in seq_along(years)) {
    date_start <- paste0(years[y], "-06-20")
    date_end <- paste0(years[y], "-06-24")

    # Test with deprecation warning
    testthat::expect_warning(
      download_data(
        dataset_name = "modis",
        date = c(date_start, date_end),
        product = product,
        version = version,
        extent = c(-124, 25, -105, 40),
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = FALSE
      ),
      "Setting download=FALSE is deprecated"
    )

    testthat::expect_true(dir.exists(directory_to_save))
  }
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis (MODIS-MOD09GA + single date)", {
  skip_if_not_local_modis_live()

  withr::local_package("httr2")
  withr::local_package("stringr")

  # function parameters
  product <- "MOD09GA"
  version <- "061"
  directory_to_save <- paste0(tempdir(), "/mod/")
  date <- "2021-04-12"

  testthat::expect_warning(
    download_data(
      dataset_name = "modis",
      date = date,
      product = product,
      version = version,
      extent = c(-124, 25, -105, 40),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    ),
    "Setting download=FALSE is deprecated"
  )

  testthat::expect_true(dir.exists(directory_to_save))
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis (MODIS-MOD06L2)", {
  skip_if_not_local_modis_live()

  withr::local_package("httr2")
  withr::local_package("stringr")

  # function parameters
  product <- "MOD06_L2"
  version <- "061"
  date_start <- "2019-02-18"
  date_end <- "2019-02-28"
  directory_to_save <- paste0(tempdir(), "/mod/")

  testthat::expect_warning(
    download_data(
      dataset_name = "modis",
      date = c(date_start, date_end),
      product = product,
      version = version,
      extent = c(-124, 25, -105, 40),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    ),
    "Setting download=FALSE is deprecated"
  )

  testthat::expect_true(dir.exists(directory_to_save))
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis (expected errors)", {
  withr::local_package("httr2")
  withr::local_package("stringr")

  # function parameters
  years <- 2020
  product <- c("MOD09GA", "MOD11A1", "MOD13A2", "MCD19A2")
  product <- sample(product, 1L)
  version <- "061"
  directory_to_save <- paste0(tempdir(), "/mod/")
  date_start <- paste0(years, "-06-25")
  date_end <- paste0(years, "-06-30")
  vec_extent <- c(-124, 25, -105, 40)

  # with token (if available) - should work
  if (Sys.getenv("CI") != "true" && Sys.getenv("NASA_EARTHDATA_TOKEN") != "") {
    testthat::expect_warning(
      download_data(
        dataset_name = "modis",
        date = c(date_start, date_end),
        product = product,
        version = version,
        extent = vec_extent,
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = FALSE
      ),
      "Setting download=FALSE is deprecated"
    )
  }

  # no token - should error
  withr::local_envvar(NASA_EARTHDATA_TOKEN = "")
  testthat::expect_error(
    download_data(
      dataset_name = "modis",
      date = c(date_start, date_end),
      product = product,
      version = version,
      extent = vec_extent,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    ),
    "NASA_EARTHDATA_TOKEN"
  )

  # year difference between date_start and date_end
  testthat::expect_error(
    download_data(
      dataset_name = "modis",
      date = c(date_start, "2024-03-28"),
      product = "MOD11A1",
      version = version,
      extent = vec_extent,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    )
  )

  # null version
  testthat::expect_error(
    download_data(
      dataset_name = "modis",
      date = c(date_start, date_end),
      product = product,
      version = NULL,
      extent = vec_extent,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    )
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis (MOD + MYD products)", {
  skip_if_not_local_modis_live()

  withr::local_package("httr2")
  withr::local_package("stringr")

  # function parameters
  products <- c(
    "MOD09GA",
    "MYD09GA",
    "MOD09GQ",
    "MYD09GQ",
    "MOD14A1",
    "MYD14A1"
  )
  version <- "061"
  directory_to_save <- paste0(tempdir(), "/mod/")
  date_start <- "2023-12-27"
  date_end <- "2023-12-27"
  vec_extent <- c(-180, -90, 180, 90)

  for (p in seq_along(products)) {
    cat("Testing product:", products[p], "\n")

    testthat::expect_warning(
      download_data(
        dataset_name = "modis",
        date = c(date_start, date_end),
        product = products[p],
        version = version,
        extent = vec_extent,
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = FALSE
      ),
      "Setting download=FALSE is deprecated"
    )
  }
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis with NASA token", {
  skip_if_not_local_modis_live()

  directory_to_save <- paste0(tempdir(), "/mod_token/")

  # Test that download works with token
  testthat::expect_no_error(
    download_data(
      dataset_name = "modis",
      date = "2024-01-01",
      product = "MOD09GA",
      version = "061",
      extent = c(-80, 35, -75, 40),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    )
  )

  testthat::expect_true(dir.exists(directory_to_save))
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis remove_command warning and hash=TRUE (mock)", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )

  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(
                list(
                  rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                  href = paste0(
                    "https://e4ftl01.cr.usgs.gov/MOLT/MOD09GA.061/",
                    "2023.01.01/MOD09GA.A2023001.h10v05.061.hdf"
                  )
                )
              )
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  withr::with_tempdir({
    # Test remove_command=TRUE warning
    testthat::expect_warning(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          remove_command = TRUE,
          hash = FALSE
        )
      ),
      "remove_command.*deprecated"
    )

    # Test hash=TRUE return
    result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

testthat::test_that("download_modis warns when date range exceeds 31 days (mock)", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )

  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(
                list(
                  rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                  href = paste0(
                    "https://e4ftl01.cr.usgs.gov/MOLT/MOD09GA.061/",
                    "2023.01.01/MOD09GA.A2023001.h10v05.061.hdf"
                  )
                )
              )
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  withr::with_tempdir({
    # date range > 31 days in same year triggers warning
    testthat::expect_warning(
      suppressMessages(
        download_modis(
          date = c("2023-01-01", "2023-03-15"),
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = FALSE
        )
      ),
      "31 days"
    )
  })
})

################################################################################
##### process_modis*
testthat::test_that("process_modis_sds", {
  # main test
  txt_products <- c(
    "MOD11A1",
    "MOD13A2",
    "MOD13Q1",
    "MYD13Q1",
    "MOD09GA",
    "MCD19A2",
    "MOD14A1",
    "MYD14A1",
    "MOD14A2",
    "MYD14A2",
    "MOD16A2",
    "MYD16A2",
    "MCD64A1",
    "MCD64CMQ",
    "MCD12Q1",
    "VNP64A1"
  )
  txt_exp_output <-
    c(
      MOD11A1 = "(LST_)",
      MOD13A2 = "(NDVI)",
      MOD13Q1 = "250m 16 days (NDVI|EVI)",
      MYD13Q1 = "250m 16 days (NDVI|EVI)",
      MOD09GA = "(sur_refl_b0)",
      MCD19A2 = "(Optical_Depth)",
      MOD14A1 = "(FireMask)",
      MYD14A1 = "(FireMask)",
      MOD14A2 = "(FireMask)",
      MYD14A2 = "(FireMask)",
      MOD16A2 = "(ET_500m|PET_500m)",
      MYD16A2 = "(ET_500m|PET_500m)",
      MCD64A1 = "(Burn Date|BurnDate)",
      MCD64CMQ = "(Burn Date|BurnDate)",
      MCD12Q1 = "(LC_Type)",
      VNP64A1 = "(BurnDate)"
    )
  txt_exp_output <- unname(txt_exp_output)
  # expect
  testthat::expect_message(
    mcdtest <- amadeus:::process_modis_sds("MCD19A2")
  )
  testthat::expect_equal(
    mcdtest,
    "(Optical_Depth)"
  )
  testthat::expect_no_error(
    amadeus:::process_modis_sds("MCD19A2", "(cos|RelAZ|Angle)")
  )
  testthat::expect_message(
    amadeus:::process_modis_sds("MCD12Q1"),
    "LC_Type"
  )
  for (i in c(1:5, 7:length(txt_products))) {
    testthat::expect_equal(
      amadeus:::process_modis_sds(txt_products[i]),
      txt_exp_output[i]
    )
  }
  testthat::expect_no_error(
    filt_other <- amadeus:::process_modis_sds("ignored", "(cos)")
  )
  testthat::expect_equal(filt_other, "(cos)")
})

testthat::test_that("download_modis accepts MOD14A1 and MYD14A1 with mocked CMR results", {
  mock_product <- NULL
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )

  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(list(
                rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                href = paste0(
                  "https://example.com/",
                  mock_product,
                  ".A2023361.h11v05.061.2024001000000.hdf"
                )
              ))
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  withr::with_tempdir({
    for (mock_product in c("MOD14A1", "MYD14A1")) {
      result <- suppressWarnings(
        suppressMessages(
          download_modis(
            date = "2023-12-27",
            product = mock_product,
            version = "061",
            directory_to_save = ".",
            acknowledgement = TRUE,
            download = FALSE
          )
        )
      )

      testthat::expect_type(result, "list")
      testthat::expect_equal(result$n_files, 1L)
      testthat::expect_match(result$urls[[1]], mock_product)
    }
  })
})

testthat::test_that("download_modis accepts MOD14CM1 and MYD14CM1 monthly files (mock)", {
  mock_product <- NULL
  mock_stamp <- NULL
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )

  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(list(
                rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                href = paste0(
                  "https://example.com/",
                  mock_product,
                  ".",
                  mock_stamp,
                  ".005.01.hdf"
                )
              ))
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  mock_cases <- list(
    list(product = "MOD14CM1", stamp = "200011", date = "2000-11-15"),
    list(product = "MYD14CM1", stamp = "200207", date = "2002-07-15")
  )

  withr::with_tempdir({
    for (mock_case in mock_cases) {
      mock_product <- mock_case$product
      mock_stamp <- mock_case$stamp
      result <- suppressWarnings(
        suppressMessages(
          download_modis(
            date = mock_case$date,
            product = mock_product,
            version = "005",
            directory_to_save = ".",
            acknowledgement = TRUE,
            download = FALSE
          )
        )
      )

      testthat::expect_type(result, "list")
      testthat::expect_equal(result$n_files, 1L)
      testthat::expect_match(result$urls[[1]], mock_stamp)
    }
  })
})

testthat::test_that("download_modis accepts MCD14DL text files (mock)", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )

  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(list(
                rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                href = paste0(
                  "https://example.com/",
                  "MODIS_C6_1_Global_MCD14DL_NRT_2026074.txt"
                )
              ))
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = "2026-03-15",
          product = "MCD14DL",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE
        )
      )
    )

    testthat::expect_type(result, "list")
    testthat::expect_equal(result$n_files, 1L)
    testthat::expect_match(result$urls[[1]], "\\.txt$")
  })
})


testthat::test_that("download_modis fire products allow cross-year filtering", {
  mock_product <- NULL

  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )

  testthat::local_mocked_bindings(
    request = function(url) list(url = url),
    req_url_query = function(req, ...) req,
    req_options = function(req, ...) req,
    req_retry = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      hrefs <- switch(
        mock_product,
        MOD14CM1 = c(
          "https://example.com/MOD14CM1.200012.005.01.hdf",
          "https://example.com/MOD14CM1.200101.005.01.hdf"
        ),
        MCD64CMQ = c(
          "https://example.com/MCD64CMQ.200012.006.01.hdf",
          "https://example.com/MCD64CMQ.200101.006.01.hdf"
        ),
        MCD64A1 = c(
          "https://example.com/MCD64A1.A2026365.h11v05.061.2027001000000.hdf",
          "https://example.com/MCD64A1.A2027001.h11v05.061.2027002000000.hdf"
        ),
        VNP64A1 = c(
          "https://example.com/VNP64A1.A2026365.h11v05.001.2027001000000.h5",
          "https://example.com/VNP64A1.A2027001.h11v05.001.2027002000000.h5"
        ),
        MCD14DL = c(
          "https://example.com/MODIS_C6_1_Global_MCD14DL_NRT_2026365.txt",
          "https://example.com/MODIS_C6_1_Global_MCD14DL_NRT_2027001.txt"
        )
      )
      list(
        feed = list(
          entry = lapply(hrefs, function(href) {
            list(
              links = list(list(
                rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                href = href
              ))
            )
          })
        )
      )
    },
    .package = "httr2"
  )

  withr::with_tempdir({
    mock_product <- "MOD14CM1"
    monthly_result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = c("2000-12-15", "2001-01-15"),
          product = mock_product,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE
        )
      )
    )
    testthat::expect_equal(monthly_result$n_files, 2L)

    mock_product <- "MCD64CMQ"
    cmq_result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = c("2000-12-15", "2001-01-15"),
          product = mock_product,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE
        )
      )
    )
    testthat::expect_equal(cmq_result$n_files, 2L)

    mock_product <- "MCD64A1"
    burned_result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = c("2026-12-31", "2027-01-01"),
          product = mock_product,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE
        )
      )
    )
    testthat::expect_equal(burned_result$n_files, 2L)

    mock_product <- "VNP64A1"
    viirs_burned_result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = c("2026-12-31", "2027-01-01"),
          product = mock_product,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE
        )
      )
    )
    testthat::expect_equal(viirs_burned_result$n_files, 2L)

    mock_product <- "MCD14DL"
    txt_result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = c("2026-12-31", "2027-01-01"),
          product = mock_product,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE
        )
      )
    )
    testthat::expect_equal(txt_result$n_files, 2L)
    testthat::expect_true(all(grepl("\\.txt$", txt_result$urls)))
  })
})


testthat::test_that("download_modis fire products use product-specific versions and data links", {
  mock_product <- NULL

  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )

  testthat::local_mocked_bindings(
    request = function(url) list(url = url),
    req_url_query = function(req, ...) req,
    req_options = function(req, ...) req,
    req_retry = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      hrefs <- switch(
        mock_product,
        MOD14A1 = c(
          "https://example.com/metadata.xml",
          "https://example.com/MOD14A1.A2021227.h11v05.061.2021234567890.h5"
        ),
        MYD14CM1 = c(
          "https://example.com/browse.jpg",
          "https://example.com/MYD14CM1.200207.005.01.hdf"
        ),
        MCD14DL = c(
          "https://example.com/ignore.hdf",
          "https://example.com/MODIS_C6_1_Global_MCD14DL_NRT_2026074.txt"
        ),
        MCD64CMQ = c(
          "https://example.com/preview.png",
          "https://example.com/MCD64CMQ.200011.006.01.hdf"
        ),
        VNP64A1 = c(
          "https://example.com/metadata.xml",
          "https://example.com/VNP64A1.A2023001.h08v05.001.2023002000000.h5"
        )
      )
      list(
        feed = list(
          entry = lapply(hrefs, function(href) {
            list(
              links = list(list(
                rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                href = href
              ))
            )
          })
        )
      )
    },
    .package = "httr2"
  )

  cases <- list(
    list(
      product = "MOD14A1",
      date = "2021-08-15",
      expected_version = "061",
      expected_pattern = "\\.h5$"
    ),
    list(
      product = "MYD14CM1",
      date = "2002-07-15",
      expected_version = "005",
      expected_pattern = "MYD14CM1\\.200207"
    ),
    list(
      product = "MCD14DL",
      date = "2026-03-15",
      expected_version = "6.1NRT",
      expected_pattern = "\\.txt$"
    ),
    list(
      product = "MCD64CMQ",
      date = "2000-11-15",
      expected_version = "006",
      expected_pattern = "MCD64CMQ\\.200011"
    ),
    list(
      product = "VNP64A1",
      date = "2023-01-01",
      expected_version = NULL,
      expected_pattern = "\\.h5$"
    )
  )

  withr::with_tempdir({
    for (i in seq_along(cases)) {
      mock_product <- cases[[i]]$product
      result <- suppressWarnings(
        suppressMessages(
          download_modis(
            date = cases[[i]]$date,
            product = mock_product,
            directory_to_save = ".",
            acknowledgement = TRUE,
            download = FALSE
          )
        )
      )

      testthat::expect_equal(result$n_files, 1L)
      testthat::expect_match(result$urls[[1]], cases[[i]]$expected_pattern)
    }
  })
})


testthat::test_that("MODIS temporal helpers cover daily monthly and text patterns", {
  daily_path <- "MOD14A1.A2021227.h11v05.061.2021234567890.hdf"
  txt_path <- "MODIS_C6_1_Global_MCD14DL_NRT_2026074.txt"
  monthly_path <- "MOD14CM1.200011.005.01.hdf"
  unknown_path <- "unsupported.file"

  testthat::expect_equal(
    amadeus:::modis_extract_temporal_key(daily_path),
    "2021227"
  )
  testthat::expect_equal(
    amadeus:::modis_extract_temporal_key(txt_path),
    "2026074"
  )
  testthat::expect_equal(
    amadeus:::modis_extract_temporal_key(monthly_path),
    "200011"
  )
  testthat::expect_true(is.na(amadeus:::modis_extract_temporal_key(
    unknown_path
  )))

  testthat::expect_equal(
    amadeus:::modis_extract_temporal_scale(daily_path),
    "daily"
  )
  testthat::expect_equal(
    amadeus:::modis_extract_temporal_scale(txt_path),
    "daily"
  )
  testthat::expect_equal(
    amadeus:::modis_extract_temporal_scale(monthly_path),
    "monthly"
  )
  testthat::expect_true(is.na(amadeus:::modis_extract_temporal_scale(
    unknown_path
  )))

  parsed_dates <- amadeus:::modis_key_to_date(
    key = c("2021227", "200011"),
    scale = c("daily", "monthly")
  )
  testthat::expect_equal(
    as.character(parsed_dates),
    c("2021-08-15", "2000-11-01")
  )
  testthat::expect_equal(
    as.character(amadeus:::modis_key_to_date(c("2021227", "2021230"), "daily")),
    c("2021-08-15", "2021-08-18")
  )
  testthat::expect_true(is.na(amadeus:::modis_key_to_date(
    NA_character_,
    NA_character_
  )))
  testthat::expect_error(
    amadeus:::modis_key_to_date("2021227", "weekly"),
    "Unsupported MODIS temporal scale"
  )
  testthat::expect_error(
    amadeus:::modis_key_to_date(
      c("2021227", "2021230"),
      c("daily", "monthly", "daily")
    )
  )
})


testthat::test_that("modis_filter_paths_by_date covers helper branches", {
  daily_paths <- c(
    "MOD14A1.A2021227.h11v05.061.2021234567890.hdf",
    "MOD14A1.A2021230.h11v05.061.2021234567890.hdf"
  )
  monthly_paths <- c(
    "MOD14CM1.200011.005.01.hdf",
    "MOD14CM1.200012.005.01.hdf"
  )
  txt_paths <- c(
    "MODIS_C6_1_Global_MCD14DL_NRT_2026074.txt",
    "MODIS_C6_1_Global_MCD14DL_NRT_2026075.txt"
  )

  testthat::expect_identical(
    amadeus:::modis_filter_paths_by_date(character(0), "2021-08-15"),
    character(0)
  )
  testthat::expect_identical(
    amadeus:::modis_filter_paths_by_date("unsupported.file", "2021-08-15"),
    character(0)
  )
  testthat::expect_equal(
    amadeus:::modis_filter_paths_by_date(daily_paths, "2021-08-15"),
    daily_paths[1]
  )
  testthat::expect_identical(
    amadeus:::modis_filter_paths_by_date(
      daily_paths,
      c("2021-09-01", "2021-09-02")
    ),
    character(0)
  )
  testthat::expect_equal(
    amadeus:::modis_filter_paths_by_date(
      monthly_paths,
      c("2000-11-15", "2000-12-15")
    ),
    monthly_paths
  )
  testthat::expect_identical(
    amadeus:::modis_filter_paths_by_date(monthly_paths, "2001-01-15"),
    character(0)
  )
  testthat::expect_equal(
    amadeus:::modis_filter_paths_by_date(txt_paths, "2026-03-16"),
    txt_paths[2]
  )
  testthat::expect_error(
    amadeus:::modis_filter_paths_by_date(
      c(daily_paths[1], monthly_paths[1]),
      "2021-08-15"
    ),
    "mixed or unsupported temporal patterns"
  )
})


testthat::test_that("calculate_modis errors on mixed temporal patterns", {
  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = -78.8, lat = 35.9),
    coords = c("lon", "lat"),
    crs = 4326
  )

  testthat::expect_error(
    calculate_modis(
      from = c(
        "MOD09GA.A2021001.h10v05.061.2021001000000.hdf",
        "MOD14CM1.200101.005.01.hdf"
      ),
      locs = locs,
      locs_id = "site_id",
      name_covariates = "mock_cov_",
      preprocess = function(...) terra::rast()
    ),
    "mixed or unsupported temporal patterns"
  )
})

testthat::test_that("calculate_modis supports from_secondary fusion methods", {
  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = -78.8, lat = 35.9),
    coords = c("lon", "lat"),
    crs = 4326
  )
  from_primary <- "MOD09GA.A2021001.h10v05.061.2021001000000.hdf"
  from_secondary <- "MYD09GA.A2021001.h10v05.061.2021001000000.hdf"

  mock_preprocess <- function(path, date, ...) {
    is_secondary <- grepl("^MYD", basename(path[1]))
    out <- terra::rast(nrows = 1, ncols = 1, vals = if (is_secondary) 3 else 1)
    terra::ext(out) <- c(-79, -78, 35, 36)
    terra::crs(out) <- "EPSG:4326"
    names(out) <- "mock_layer"
    out
  }

  testthat::local_mocked_bindings(
    calculate_modis_daily = function(
      from,
      locs,
      locs_id,
      date,
      name_extracted,
      ...
    ) {
      out <- data.frame(
        site_id = as.character(locs[[locs_id]][1]),
        time = as.Date(date)
      )
      out[[name_extracted]] <- as.numeric(terra::values(from)[1])
      out
    },
    .package = "amadeus"
  )

  result_mean <- calculate_modis(
    from = from_primary,
    from_secondary = from_secondary,
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    preprocess = mock_preprocess,
    name_covariates = "cov_",
    subdataset = "mock",
    scale = "* 1",
    fusion_method = "mean"
  )
  testthat::expect_equal(result_mean$cov_00000, 2)

  result_primary <- calculate_modis(
    from = from_primary,
    from_secondary = from_secondary,
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    preprocess = mock_preprocess,
    name_covariates = "cov_",
    subdataset = "mock",
    scale = "* 1",
    fusion_method = "primary_first"
  )
  testthat::expect_equal(result_primary$cov_00000, 1)

  result_secondary <- calculate_modis(
    from = from_primary,
    from_secondary = from_secondary,
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    preprocess = mock_preprocess,
    name_covariates = "cov_",
    subdataset = "mock",
    scale = "* 1",
    fusion_method = "secondary_first"
  )
  testthat::expect_equal(result_secondary$cov_00000, 3)
})

testthat::test_that("calculate_modis from_secondary errors on incompatible geometry", {
  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = -78.8, lat = 35.9),
    coords = c("lon", "lat"),
    crs = 4326
  )
  from_primary <- "MOD09GA.A2021001.h10v05.061.2021001000000.hdf"
  from_secondary <- "MYD09GA.A2021001.h10v05.061.2021001000000.hdf"

  mock_preprocess <- function(path, date, ...) {
    is_secondary <- grepl("^MYD", basename(path[1]))
    out <- terra::rast(nrows = 1, ncols = 1, vals = if (is_secondary) 3 else 1)
    if (is_secondary) {
      terra::ext(out) <- c(-79, -77, 35, 36)
    } else {
      terra::ext(out) <- c(-79, -78, 35, 36)
    }
    terra::crs(out) <- "EPSG:4326"
    names(out) <- "mock_layer"
    out
  }

  testthat::expect_error(
    calculate_modis(
      from = from_primary,
      from_secondary = from_secondary,
      locs = locs,
      locs_id = "site_id",
      radius = 0L,
      preprocess = mock_preprocess,
      name_covariates = "cov_",
      subdataset = "mock",
      scale = "* 1",
      fusion_method = "mean"
    ),
    "incompatible geometry"
  )
})

testthat::test_that("calculate_modis from_secondary errors on layer mismatch", {
  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = -78.8, lat = 35.9),
    coords = c("lon", "lat"),
    crs = 4326
  )
  from_primary <- "MOD09GA.A2021001.h10v05.061.2021001000000.hdf"
  from_secondary <- "MYD09GA.A2021001.h10v05.061.2021001000000.hdf"

  mock_preprocess <- function(path, date, ...) {
    is_secondary <- grepl("^MYD", basename(path[1]))
    if (is_secondary) {
      out <- terra::rast(nrows = 1, ncols = 1, nlyrs = 2)
      terra::values(out) <- c(3, 4)
      names(out) <- c("mock_layer_1", "mock_layer_2")
    } else {
      out <- terra::rast(nrows = 1, ncols = 1, vals = 1)
      names(out) <- "mock_layer_1"
    }
    terra::ext(out) <- c(-79, -78, 35, 36)
    terra::crs(out) <- "EPSG:4326"
    out
  }

  testthat::expect_error(
    calculate_modis(
      from = from_primary,
      from_secondary = from_secondary,
      locs = locs,
      locs_id = "site_id",
      radius = 0L,
      preprocess = mock_preprocess,
      name_covariates = "cov_",
      subdataset = "mock",
      scale = "* 1",
      fusion_method = "mean"
    ),
    "different layer counts"
  )
})

testthat::test_that("calculate_modis validates from_secondary type", {
  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = -78.8, lat = 35.9),
    coords = c("lon", "lat"),
    crs = 4326
  )

  testthat::expect_error(
    calculate_modis(
      from = "MOD09GA.A2021001.h10v05.061.2021001000000.hdf",
      from_secondary = 1L,
      locs = locs,
      locs_id = "site_id",
      preprocess = function(...) terra::rast(),
      name_covariates = "cov_",
      scale = "* 1"
    ),
    "from_secondary should be a character vector"
  )
})

testthat::test_that("calculate_modis uses single-source fusion days", {
  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = -78.8, lat = 35.9),
    coords = c("lon", "lat"),
    crs = 4326
  )

  mock_preprocess <- function(path, date, ...) {
    is_secondary <- grepl("^MYD", basename(path[1]))
    out <- terra::rast(nrows = 1, ncols = 1, vals = if (is_secondary) 3 else 1)
    terra::ext(out) <- c(-79, -78, 35, 36)
    terra::crs(out) <- "EPSG:4326"
    names(out) <- "mock_layer"
    out
  }

  testthat::local_mocked_bindings(
    calculate_modis_daily = function(
      from,
      locs,
      locs_id,
      date,
      name_extracted,
      ...
    ) {
      out <- data.frame(
        site_id = as.character(locs[[locs_id]][1]),
        time = as.Date(date)
      )
      out[[name_extracted]] <- as.numeric(terra::values(from)[1])
      out
    },
    .package = "amadeus"
  )

  result <- calculate_modis(
    from = "MOD09GA.A2021001.h10v05.061.2021001000000.hdf",
    from_secondary = "MYD09GA.A2021002.h10v05.061.2021002000000.hdf",
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    preprocess = mock_preprocess,
    name_covariates = "cov_",
    subdataset = "mock",
    scale = "* 1",
    fusion_method = "mean"
  )

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_equal(result$cov_00000, c(1, 3))
})

################################################################################
##### calculate_modis .by wiring

testthat::test_that("calculate_modis no longer exposes legacy temporal args", {
  testthat::expect_false("fun_temporal" %in% names(formals(calculate_modis)))
  testthat::expect_false("time_bucket" %in% names(formals(calculate_modis)))
})

testthat::test_that("calculate_modis .by wiring aggregates multi-day rows", {
  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = -78.8, lat = 35.9),
    coords = c("lon", "lat"),
    crs = 4326
  )
  # Two files on the same day produce two per-day rows in mock
  from_files <- c(
    "MOD09GA.A2021001.h10v05.061.2021001000000.hdf",
    "MOD09GA.A2021002.h10v05.061.2021002000000.hdf"
  )
  call_count <- 0L
  testthat::local_mocked_bindings(
    calculate_modis_daily = function(
      from,
      locs,
      locs_id,
      date,
      name_extracted,
      ...
    ) {
      call_count <<- call_count + 1L
      data.frame(
        site_id = "site_1",
        time = as.Date(date),
        cov_00000 = as.numeric(call_count) * 10
      )
    },
    .package = "amadeus"
  )
  mock_preprocess <- function(path, date, ...) {
    r <- terra::rast(nrows = 1, ncols = 1, vals = 1)
    terra::ext(r) <- c(-79, -78, 35, 36)
    terra::crs(r) <- "EPSG:4326"
    names(r) <- "mock_layer"
    r
  }
  # NULL: backward compat — 2 rows (one per day)
  call_count <- 0L
  result_null <- suppressMessages(
    calculate_modis(
      from = from_files,
      locs = locs,
      locs_id = "site_id",
      radius = 0L,
      preprocess = mock_preprocess,
      name_covariates = "cov_",
      subdataset = "mock",
      scale = "* 1"
    )
  )
  testthat::expect_equal(nrow(result_null), 2L)
  # mean over 2 days → 1 row
  call_count <- 0L
  result_mean <- suppressMessages(
    calculate_modis(
      from = from_files,
      locs = locs,
      locs_id = "site_id",
      radius = 0L,
      preprocess = mock_preprocess,
      name_covariates = "cov_",
      subdataset = "mock",
      scale = "* 1",
      .by = "week"
    )
  )
  testthat::expect_equal(nrow(result_mean), 1L)
  testthat::expect_equal(result_mean$cov_00000, 15)
})

testthat::test_that("calculate_modis uses per-day preprocess before .by_time summarization", {
  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = -78.8, lat = 35.9),
    coords = c("lon", "lat"),
    crs = 4326
  )
  from_files <- c(
    "MOD09GA.A2021001.h10v05.061.2021001000000.hdf",
    "MOD09GA.A2021002.h10v05.061.2021002000000.hdf"
  )
  preprocess_dates <- character(0)
  testthat::local_mocked_bindings(
    process_modis_merge = function(path, date, subdataset, fun_agg = "mean", ...) {
      testthat::expect_length(date, 1L)
      preprocess_dates <<- c(preprocess_dates, as.character(date))
      r <- terra::rast(nrows = 1, ncols = 1, vals = 1)
      terra::ext(r) <- c(-79, -78, 35, 36)
      terra::crs(r) <- "EPSG:4326"
      names(r) <- "mock_layer"
      r
    },
    calculate_modis_daily = function(from, locs, locs_id, date, name_extracted, ...) {
      data.frame(
        site_id = "site_1",
        time = as.Date(date),
        cov_00000 = as.numeric(format(as.Date(date), "%d"))
      )
    },
    .package = "amadeus"
  )

  result <- suppressMessages(
    calculate_modis(
      from = from_files,
      locs = locs,
      locs_id = "site_id",
      radius = 0L,
      preprocess = amadeus::process_modis_merge,
      name_covariates = "cov_",
      subdataset = "mock",
      scale = "* 1",
      .by = "week"
    )
  )

  testthat::expect_equal(sort(unique(preprocess_dates)), c("2021-01-01", "2021-01-02"))
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$cov_00000, 1.5)
})


testthat::test_that("process_modis_sds returns fire mask regex for fire products", {
  testthat::expect_equal(amadeus:::process_modis_sds(product = "MOD14A1"), "(FireMask)")
  testthat::expect_equal(amadeus:::process_modis_sds(product = "MYD14A1"), "(FireMask)")
  testthat::expect_equal(amadeus:::process_modis_sds(product = "MOD14A2"), "(FireMask)")
  testthat::expect_equal(amadeus:::process_modis_sds(product = "MYD14A2"), "(FireMask)")
})


testthat::test_that("process_modis_merge supports secondary path fusion", {
  r_primary <- terra::rast(nrows = 1, ncols = 1, vals = 1)
  r_secondary <- terra::rast(nrows = 1, ncols = 1, vals = 3)
  terra::ext(r_primary) <- c(0, 1, 0, 1)
  terra::ext(r_secondary) <- c(0, 1, 0, 1)
  terra::crs(r_primary) <- "EPSG:4326"
  terra::crs(r_secondary) <- "EPSG:4326"

  testthat::local_mocked_bindings(
    modis_filter_paths_by_date = function(paths, date) paths,
    process_flatten_sds = function(path, subdataset, fun_agg) {
      if (grepl("secondary", path)) r_secondary else r_primary
    },
    .package = "amadeus"
  )

  fused_mean <- process_modis_merge(
    path = "primary.hdf",
    path_secondary = "secondary.hdf",
    date = "2023-01-01",
    subdataset = "mock",
    fusion_method = "mean"
  )
  testthat::expect_equal(as.numeric(terra::values(fused_mean)[1]), 2)

  fused_primary <- process_modis_merge(
    path = "primary.hdf",
    path_secondary = "secondary.hdf",
    date = "2023-01-01",
    subdataset = "mock",
    fusion_method = "primary_first"
  )
  testthat::expect_equal(as.numeric(terra::values(fused_primary)[1]), 1)

  fused_secondary <- process_modis_merge(
    path = "primary.hdf",
    path_secondary = "secondary.hdf",
    date = "2023-01-01",
    subdataset = "mock",
    fusion_method = "secondary_first"
  )
  testthat::expect_equal(as.numeric(terra::values(fused_secondary)[1]), 3)
})

testthat::test_that("process_modis_merge validates secondary fusion inputs and geometry", {
  r_primary <- terra::rast(nrows = 1, ncols = 1, vals = 1)
  terra::ext(r_primary) <- c(0, 1, 0, 1)
  terra::crs(r_primary) <- "EPSG:4326"

  testthat::expect_error(
    process_modis_merge(
      path = "primary.hdf",
      path_secondary = 1L,
      date = "2023-01-01",
      subdataset = "mock"
    ),
    "path_secondary"
  )

  testthat::local_mocked_bindings(
    modis_filter_paths_by_date = function(paths, date) paths,
    process_flatten_sds = function(path, subdataset, fun_agg) {
      if (grepl("secondary", path)) {
        r_secondary <- terra::rast(nrows = 1, ncols = 1, vals = 3)
        terra::ext(r_secondary) <- c(0, 2, 0, 1)
        terra::crs(r_secondary) <- "EPSG:4326"
        return(r_secondary)
      }
      r_primary
    },
    .package = "amadeus"
  )

  testthat::expect_error(
    process_modis_merge(
      path = "primary.hdf",
      path_secondary = "secondary.hdf",
      date = "2023-01-01",
      subdataset = "mock",
      fusion_method = "mean"
    ),
    "incompatible geometry"
  )
})

testthat::test_that("process_modis_merge errors when secondary layer counts differ", {
  r_primary <- terra::rast(nrows = 1, ncols = 1, vals = 1)
  terra::ext(r_primary) <- c(0, 1, 0, 1)
  terra::crs(r_primary) <- "EPSG:4326"

  r_secondary <- terra::rast(nrows = 1, ncols = 1, nlyrs = 2)
  terra::values(r_secondary) <- c(3, 4)
  terra::ext(r_secondary) <- c(0, 1, 0, 1)
  terra::crs(r_secondary) <- "EPSG:4326"

  testthat::local_mocked_bindings(
    modis_filter_paths_by_date = function(paths, date) paths,
    process_flatten_sds = function(path, subdataset, fun_agg) {
      if (grepl("secondary", path)) r_secondary else r_primary
    },
    .package = "amadeus"
  )

  testthat::expect_error(
    process_modis_merge(
      path = "primary.hdf",
      path_secondary = "secondary.hdf",
      date = "2023-01-01",
      subdataset = "mock",
      fusion_method = "mean"
    ),
    "different layer counts"
  )
})

testthat::test_that("process_modis_merge errors when no files match requested date", {
  testthat::local_mocked_bindings(
    modis_filter_paths_by_date = function(paths, date) character(0),
    .package = "amadeus"
  )

  testthat::expect_error(
    process_modis_merge(
      path = "primary.hdf",
      date = "2023-01-01",
      subdataset = "mock"
    ),
    "No MODIS files matched the requested date"
  )
})

testthat::test_that("process_modis_merge merges multiple secondary rasters", {
  r_primary <- terra::rast(nrows = 1, ncols = 1, vals = 1)
  terra::ext(r_primary) <- c(0, 1, 0, 1)
  terra::crs(r_primary) <- "EPSG:4326"

  r_secondary_a <- terra::rast(nrows = 1, ncols = 1, vals = 3)
  terra::ext(r_secondary_a) <- c(0, 1, 0, 1)
  terra::crs(r_secondary_a) <- "EPSG:4326"
  r_secondary_b <- terra::rast(nrows = 1, ncols = 1, vals = 5)
  terra::ext(r_secondary_b) <- c(0, 1, 0, 1)
  terra::crs(r_secondary_b) <- "EPSG:4326"

  testthat::local_mocked_bindings(
    modis_filter_paths_by_date = function(paths, date) paths,
    process_flatten_sds = function(path, subdataset, fun_agg) {
      if (path == "secondary_a.hdf") {
        return(r_secondary_a)
      }
      if (path == "secondary_b.hdf") {
        return(r_secondary_b)
      }
      r_primary
    },
    .package = "amadeus"
  )

  result <- process_modis_merge(
    path = "primary.hdf",
    path_secondary = c("secondary_a.hdf", "secondary_b.hdf"),
    date = "2023-01-01",
    subdataset = "mock",
    fusion_method = "secondary_first"
  )

  testthat::expect_s4_class(result, "SpatRaster")
})


testthat::test_that("process_modis_daily returns day-preserving list and stack", {
  testthat::local_mocked_bindings(
    process_modis_merge = function(
      path,
      date,
      subdataset,
      fun_agg = "mean",
      path_secondary = NULL,
      fusion_method = "mean",
      ...
    ) {
      day_num <- as.integer(gsub("-", "", date))
      r <- terra::rast(nrows = 1, ncols = 1, vals = day_num)
      terra::ext(r) <- c(0, 1, 0, 1)
      terra::crs(r) <- "EPSG:4326"
      names(r) <- subdataset
      r
    },
    .package = "amadeus"
  )

  result_list <- process_modis_daily(
    path = "primary.hdf",
    date = c("2023-01-01", "2023-01-03"),
    subdataset = "mock",
    return_type = "list"
  )
  testthat::expect_named(
    result_list,
    c("2023-01-01", "2023-01-02", "2023-01-03")
  )
  testthat::expect_length(result_list, 3L)
  testthat::expect_true(all(vapply(
    result_list,
    methods::is,
    logical(1),
    "SpatRaster"
  )))

  result_stack <- process_modis_daily(
    path = "primary.hdf",
    date = c("2023-01-01", "2023-01-03"),
    subdataset = "mock",
    return_type = "stack"
  )
  testthat::expect_s4_class(result_stack, "SpatRaster")
  testthat::expect_equal(terra::nlyr(result_stack), 3L)
  testthat::expect_equal(
    names(result_stack),
    c("mock_20230101", "mock_20230102", "mock_20230103")
  )
})


testthat::test_that("process_modis_daily skips unmatched days and errors when all days miss", {
  testthat::local_mocked_bindings(
    process_modis_merge = function(
      path,
      date,
      subdataset,
      fun_agg = "mean",
      path_secondary = NULL,
      fusion_method = "mean",
      ...
    ) {
      if (date == "2023-01-02") {
        stop("No MODIS files matched the requested date.\n")
      }
      r <- terra::rast(nrows = 1, ncols = 1, vals = 1)
      terra::ext(r) <- c(0, 1, 0, 1)
      terra::crs(r) <- "EPSG:4326"
      names(r) <- subdataset
      r
    },
    .package = "amadeus"
  )

  result_stack <- process_modis_daily(
    path = "primary.hdf",
    date = c("2023-01-01", "2023-01-03"),
    subdataset = "mock",
    return_type = "stack"
  )
  testthat::expect_equal(terra::nlyr(result_stack), 2L)
  testthat::expect_equal(
    names(result_stack),
    c("mock_20230101", "mock_20230103")
  )

  testthat::local_mocked_bindings(
    process_modis_merge = function(
      path,
      date,
      subdataset,
      fun_agg = "mean",
      path_secondary = NULL,
      fusion_method = "mean",
      ...
    ) {
      stop("No MODIS files matched the requested date.\n")
    },
    .package = "amadeus"
  )

  testthat::expect_error(
    process_modis_daily(
      path = "primary.hdf",
      date = c("2023-01-01", "2023-01-03"),
      subdataset = "mock",
      return_type = "list"
    ),
    "No MODIS files matched any day"
  )
})


testthat::test_that("process_flatten_sds", {
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_options(list(sf_use_s2 = FALSE))

  mcd19 <- testthat::test_path(
    "..",
    "testdata",
    "modis",
    "MCD19A2.A2021227.h11v05.061.2023149160635.hdf"
  )
  mod09 <- testthat::test_path(
    "..",
    "testdata",
    "modis",
    "MOD09GA.A2021227.h11v05.061.2021229035936.hdf"
  )

  # main test: mcd19
  # expect warning for new strict terra/GDAL HDF4 warning
  testthat::expect_no_warning(
    mcdaggr <-
      process_flatten_sds(
        path = mcd19,
        subdataset = "Optical_Depth",
        fun_agg = "mean"
      )
  )
  testthat::expect_s4_class(mcdaggr, "SpatRaster")
  testthat::expect_equal(terra::nlyr(mcdaggr), 2L)
  testthat::expect_equal(
    all(grepl("^Optical", names(mcdaggr))),
    TRUE
  )

  # flatten error
  path_mod06 <-
    testthat::test_path(
      "..",
      "testdata",
      "modis",
      "MOD06_L2.A2021227.0320.061.2021227134022.hdf"
    )

  testthat::expect_error(
    process_flatten_sds(
      path = path_mod06,
      subdataset = "(Fraction)",
      fun_agg = "mean"
    )
  )

  # mod09 test
  mod09_sub <-
    sprintf("HDF4_EOS:EOS_GRID:%s:MODIS_Grid_500m_2D:sur_refl_b01_1", mod09)
  # main test: mcd19
  testthat::expect_no_error(
    modaggr <-
      process_flatten_sds(
        path = mod09_sub,
        subdataset = NULL,
        fun_agg = "mean"
      )
  )
  testthat::expect_s4_class(modaggr, "SpatRaster")
  testthat::expect_equal(terra::nlyr(modaggr), 1L)
  testthat::expect_true(grepl("^500m Surface", names(modaggr)))
})


testthat::test_that("process_modis_merge", {
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_mod11 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD11A1.A2021227.h11v05.061.2021228105320.hdf"
    )
  testthat::expect_no_warning(
    process_modis_merge(
      path = path_mod11,
      date = "2021-08-15",
      subdataset = "(LST_)"
    )
  )
  # case 2: standard mod13a2
  path_mod13 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD13A2.A2021225.h11v05.061.2021320163751.hdf"
    )
  testthat::expect_no_warning(
    process_modis_merge(
      path = path_mod13,
      date = "2021-08-13",
      subdataset = "(NDVI)"
    )
  )

  # case 3: standard mcd19a2
  path_mcd19 <-
    testthat::test_path(
      "../testdata/modis/",
      "MCD19A2.A2021227.h11v05.061.2023149160635.hdf"
    )
  testthat::expect_no_warning(
    process_modis_merge(
      path = path_mcd19,
      date = "2021-08-15",
      subdataset = "(Optical_Depth)"
    )
  )

  # case 3: standard mcd19a2
  path_mod09 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD09GA.A2021227.h11v05.061.2021229035936.hdf"
    )
  testthat::expect_no_warning(
    process_modis_merge(
      path = path_mod09,
      date = "2021-08-15",
      subdataset = "(sur_refl_b0)"
    )
  )

  # multiple files
  paths_mod13 <- list.files(
    testthat::test_path("../testdata/modis/"),
    pattern = "MOD13A2",
    full.names = TRUE
  )
  testthat::expect_no_warning(
    process_modis_merge(
      path = paths_mod13,
      date = "2021-08-13",
      subdataset = "(NDVI)"
    )
  )
  testthat::expect_error(
    process_modis_merge(
      path = paths_mod13,
      date = "2021-08-13",
      subdataset = "(NDVI)",
      fun_agg = 3L
    )
  )
})

testthat::test_that("process_modis_merge handles monthly MOD14CM1 names", {
  withr::local_package("terra")

  testthat::local_mocked_bindings(
    process_flatten_sds = function(
      path,
      subdataset = NULL,
      fun_agg = "mean",
      ...
    ) {
      out <- terra::rast(
        ncols = 1,
        nrows = 1,
        xmin = -1,
        xmax = 1,
        ymin = -1,
        ymax = 1,
        crs = "EPSG:4326"
      )
      terra::values(out) <- 1
      names(out) <- "fire_mask"
      out
    },
    .package = "amadeus"
  )

  monthly_paths <- c(
    "MOD14CM1.200011.005.01.hdf",
    "MOD14CM1.200012.005.01.hdf"
  )
  testthat::expect_no_error(
    monthly_result <- process_modis_merge(
      path = monthly_paths,
      date = "2000-11-15",
      subdataset = "(FireMask)"
    )
  )
  testthat::expect_s4_class(monthly_result, "SpatRaster")
  testthat::expect_equal(terra::nlyr(monthly_result), 1L)
})


testthat::test_that("process_mcd14dl covers directory filtering and error branches", {
  withr::local_package("terra")
  withr::local_package("data.table")

  testthat::expect_error(amadeus:::process_mcd14dl(), "path is required")

  withr::with_tempdir({
    txt_one <- file.path(".", "MODIS_C6_1_Global_MCD14DL_NRT_2026074.txt")
    txt_two <- file.path(".", "MODIS_C6_1_Global_MCD14DL_NRT_2026075.txt")
    bad_txt <- file.path(".", "bad_mcd14dl.txt")

    data.table::fwrite(
      data.frame(
        latitude = c(35.95013, 40),
        longitude = c(-78.8277, -120),
        acq_date = c("2026-03-15", "2026-03-16"),
        acq_time = c(1230, 45)
      ),
      txt_one
    )
    data.table::fwrite(
      data.frame(
        latitude = 35.951,
        longitude = -78.826,
        acq_date = "2026-03-16",
        acq_time = 30,
        frp = 7.5
      ),
      txt_two
    )
    data.table::fwrite(data.frame(latitude = 35), bad_txt)

    proc <- amadeus:::process_mcd14dl(
      path = ".",
      date = c("2026-03-15", "2026-03-16"),
      extent = c(-79, 35.9, -78.7, 36.0)
    )
    testthat::expect_s4_class(proc, "SpatVector")
    testthat::expect_equal(nrow(proc), 2)
    testthat::expect_true(all(proc$fire_count == 1L))
    testthat::expect_equal(proc$time, c(20260315L, 20260316L))
    testthat::expect_true(is.na(proc$frp[1]))
    testthat::expect_equal(proc$frp[2], 7.5)

    testthat::expect_error(
      amadeus:::process_mcd14dl(path = bad_txt),
      "missing one or more required columns"
    )
  })
})

testthat::test_that("process_mcd14dl handles no txt files and missing frp/date", {
  withr::local_package("terra")
  withr::local_package("data.table")

  withr::with_tempdir({
    file.create("not_mcd14dl.csv")
    testthat::expect_error(
      amadeus:::process_mcd14dl(path = "."),
      "No MCD14DL text files were found"
    )
  })

  withr::with_tempdir({
    txt_path <- file.path(".", "MODIS_C6_1_Global_MCD14DL_NRT_2026074.txt")
    data.table::fwrite(
      data.frame(
        latitude = 35.95013,
        longitude = -78.8277,
        acq_date = "2026-03-15",
        acq_time = 1230
      ),
      txt_path
    )
    proc <- amadeus:::process_mcd14dl(path = txt_path)
    testthat::expect_s4_class(proc, "SpatVector")
    testthat::expect_true(all(is.na(proc$frp)))
  })
})


testthat::test_that("process_blackmarble*", {
  withr::local_package("terra")

  path_vnp46 <-
    list.files(
      testthat::test_path("..", "testdata", "modis"),
      "^VNP46A2",
      full.names = TRUE
    )

  testthat::expect_no_error(
    corn <- process_blackmarble_corners()
  )
  testthat::expect_error(
    process_blackmarble_corners(hrange = c(99, 104))
  )

  # terra no longer produces "unknown extent" for VNP46 in newer versions
  testthat::expect_no_error(
    vnp46_proc <- process_blackmarble(
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018-08-13"
    )
  )

  testthat::expect_s4_class(vnp46_proc, "SpatRaster")
  testthat::expect_equal(terra::nlyr(vnp46_proc), 1L)

  testthat::expect_no_error(
    vnp46_proc2 <- process_blackmarble(
      path = path_vnp46[1],
      tile_df = corn,
      subdataset = c(3L, 5L),
      date = "2018-08-13"
    )
  )

  testthat::expect_s4_class(vnp46_proc2, "SpatRaster")
  testthat::expect_equal(terra::nlyr(vnp46_proc2), 2L)

  testthat::expect_error(
    process_blackmarble(
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018~08~13"
    )
  )
})


testthat::test_that("process_modis_warp + process_modis_swath", {
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_mod06 <-
    testthat::test_path(
      "..",
      "testdata",
      "modis",
      "MOD06_L2.A2021227.0320.061.2021227134022.hdf"
    )
  path_mod06 <-
    sprintf("HDF4_EOS:EOS_SWATH:%s:mod06:Cloud_Fraction_Night", path_mod06)
  # internal warning from stars
  testthat::expect_warning(
    warped <- process_modis_warp(
      path = path_mod06
    )
  )
  testthat::expect_s3_class(warped, "stars")
  testthat::expect_equal(
    unname(stars::st_res(warped)[1]),
    0.1,
    tolerance = 1e-6
  )

  path_mod06s <-
    list.files(
      testthat::test_path("..", "testdata", "modis"),
      pattern = "MOD06_L2",
      full.names = TRUE
    )

  testthat::expect_warning(
    warped4 <- process_modis_swath(
      path = path_mod06s,
      date = "2021-08-15",
      subdataset = c("Cloud_Fraction_Night", "Cloud_Fraction_Day")
    )
  )
  testthat::expect_s4_class(warped4, "SpatRaster")
})


testthat::test_that("process_modis (expected errors)", {
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_options(list(sf_use_s2 = FALSE))
  path_mod06 <-
    testthat::test_path(
      "..",
      "testdata",
      "modis",
      "MOD06_L2.A2021227.0320.061.2021227134022.hdf"
    )
  path_mod06e <-
    sprintf("HDF4_EOS:EOS_SWATH:%s:mod06:Cloud_Fraction_Night", path_mod06)

  testthat::expect_no_error(
    suppressWarnings(
      process_modis_swath(
        path = path_mod06,
        subdataset = "Cloud_Fraction_Night",
        date = "2021-08-15"
      )
    )
  )
  testthat::expect_error(
    process_modis_swath(
      path = path_mod06,
      subdataset = "Cloud_Fraction_Night",
      date = "2021~08~15"
    )
  )
  testthat::expect_error(
    process_modis_swath(
      path = path_mod06,
      subdataset = "Cloud_Fraction_Night",
      date = "2021-13-15"
    )
  )
  testthat::expect_error(
    process_modis_swath(
      path = path_mod06,
      subdataset = "Cloud_Fraction_Night",
      date = "2021-12-45"
    )
  )
})

################################################################################
##### calc_modis*
testthat::test_that("calculate_modis", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_package("lwgeom")
  withr::local_options(
    list(
      sf_use_s2 = FALSE
    )
  )

  site_faux <-
    data.frame(
      site_id = "37999904288101",
      lon = -89.87,
      lat = 39.8734,
      time = as.Date("2021-08-15")
    )
  site_faux <-
    terra::vect(
      site_faux,
      geom = c("lon", "lat"),
      keepgeom = FALSE,
      crs = "EPSG:4326"
    )

  # case 1: standard mod11a1
  path_mod11 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD11A1.A2021227.h11v05.061.2021228105320.hdf"
    )
  testthat::expect_no_warning(
    base_mod11 <-
      process_modis_merge(
        path = path_mod11,
        date = "2021-08-15",
        subdataset = "(LST_)",
        fun_agg = "mean"
      )
  )
  testthat::expect_s4_class(base_mod11, "SpatRaster")

  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11 <-
        calculate_modis(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          scale = NULL
        )
    )
  )
  testthat::expect_s3_class(calc_mod11, "data.frame")
  # all values are >14000 (unscaled values)
  testthat::expect_true(
    all(calc_mod11[, grep("MOD", names(calc_mod11))] > 14000)
  )

  # ... _add arguments test
  aux <- 0L
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11 <-
        calculate_modis(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          package_list_add = c("MASS"),
          export_list_add = c("aux"),
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          scale = "* 1"
        )
    )
  )

  # with geometry terra
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11_terra <-
        calculate_modis(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          package_list_add = c("MASS"),
          export_list_add = c("aux"),
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          geom = "terra",
          scale = "* 1"
        )
    )
  )
  testthat::expect_s4_class(calc_mod11_terra, "SpatVector")

  # with geometry sf
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11_sf <-
        calculate_modis(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          package_list_add = c("MASS"),
          export_list_add = c("aux"),
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          geom = "sf",
          scale = "* 1"
        )
    )
  )
  testthat::expect_true(inherits(calc_mod11_sf, "sf"))

  # with geometry error
  testthat::expect_error(
    calculate_modis(
      from = path_mod11,
      locs = sf::st_as_sf(site_faux),
      preprocess = process_modis_merge,
      package_list_add = c("MASS"),
      export_list_add = c("aux"),
      name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
      subdataset = "(LST_)",
      geom = TRUE,
      scale = "* 1"
    )
  )

  # no error with scale and convert to C
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11_scale <-
        calculate_modis(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          package_list_add = c("MASS"),
          export_list_add = c("aux"),
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          geom = FALSE,
          scale = "* 0.02 - 273.15"
        )
    )
  )
  # all values below 27 C (manually inspected) to ensure proper scaled
  testthat::expect_true(
    all(calc_mod11_scale[, grep("MOD", names(calc_mod11_scale))] < 27)
  )

  # warning with scale = NULL
  testthat::expect_warning(
    calc_mod11_scalew <-
      calculate_modis(
        from = path_mod11,
        locs = sf::st_as_sf(site_faux),
        preprocess = process_modis_merge,
        package_list_add = c("MASS"),
        export_list_add = c("aux"),
        name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
        subdataset = "(LST_)",
        geom = FALSE,
        scale = NULL
      )
  )

  # case 2: swath mod06l2
  path_mod06 <-
    list.files(
      testthat::test_path("..", "testdata/modis"),
      "MOD06",
      full.names = TRUE
    )
  testthat::expect_no_error(
    suppressWarnings(
      cloud0 <- process_modis_swath(
        path = path_mod06,
        subdataset = c("Cloud_Fraction_Day"),
        date = "2021-08-15"
      )
    )
  )

  testthat::expect_no_error(
    suppressWarnings(
      calc_mod06 <-
        calculate_modis(
          from = path_mod06,
          locs = site_faux,
          subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
          preprocess = process_modis_swath,
          name_covariates = c("MOD_CLFRN_0_", "MOD_CLFRD_0_")
        )
    )
  )
  testthat::expect_s3_class(calc_mod06, "data.frame")

  # with geometry terra
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod06_terra <-
        calculate_modis(
          from = path_mod06,
          locs = site_faux,
          subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
          preprocess = process_modis_swath,
          name_covariates = c("MOD_CLFRN_0_", "MOD_CLFRD_0_"),
          geom = "terra"
        )
    )
  )
  testthat::expect_s4_class(calc_mod06_terra, "SpatVector")

  # with geometry sf
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod06_sf <-
        calculate_modis(
          from = path_mod06,
          locs = site_faux,
          subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
          preprocess = process_modis_swath,
          name_covariates = c("MOD_CLFRN_0_", "MOD_CLFRD_0_"),
          geom = "sf"
        )
    )
  )
  testthat::expect_true(inherits(calc_mod06_sf, "sf"))

  # with geometry error
  testthat::expect_error(
    calculate_modis(
      from = path_mod06,
      locs = site_faux,
      subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
      preprocess = process_modis_swath,
      name_covariates = c("MOD_CLFRN_0_", "MOD_CLFRD_0_"),
      geom = TRUE
    )
  )

  # case 3: VIIRS (expect "unknown extent" warnings)
  path_vnp46 <-
    list.files(
      testthat::test_path("..", "testdata/modis"),
      "VNP46",
      full.names = TRUE
    )

  testthat::expect_no_error(
    base_vnp <- process_blackmarble(
      path = path_vnp46,
      date = "2018-08-13",
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5))
    )
  )

  testthat::expect_no_error(
    calc_vnp46 <-
      calculate_modis(
        from = path_vnp46,
        locs = site_faux,
        preprocess = process_blackmarble,
        name_covariates = c("MOD_NITLT_0_"),
        subdataset = 3L,
        tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
        scale = "* 1"
      )
  )
  testthat::expect_s3_class(calc_vnp46, "data.frame")

  # with geometry terra
  testthat::expect_no_error(
    calc_vnp46_terra <-
      calculate_modis(
        from = path_vnp46,
        locs = site_faux,
        preprocess = process_blackmarble,
        name_covariates = c("MOD_NITLT_0_"),
        subdataset = 3L,
        tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
        geom = "terra",
        scale = "* 1"
      )
  )
  testthat::expect_s4_class(calc_vnp46_terra, "SpatVector")

  # with geometry sf
  testthat::expect_no_error(
    calc_vnp46_sf <-
      calculate_modis(
        from = path_vnp46,
        locs = sf::st_as_sf(site_faux),
        preprocess = process_blackmarble,
        name_covariates = c("MOD_NITLT_0_"),
        subdataset = 3L,
        tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
        geom = "sf",
        scale = "* 1"
      )
  )
  testthat::expect_true("sf" %in% class(calc_vnp46_sf))

  # with geometry error
  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = sf::st_as_sf(site_faux),
      preprocess = process_blackmarble,
      name_covariates = c("MOD_NITLT_0_"),
      subdataset = 3L,
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
      geom = TRUE,
      scale = "* 1"
    )
  )

  # error cases
  testthat::expect_error(
    process_modis_merge(path = site_faux)
  )
  testthat::expect_error(
    process_modis_merge(
      path = path_mod11,
      date = "2021-08-15",
      fun_agg = 3L
    )
  )
  testthat::expect_error(
    process_modis_merge(
      path = path_mod11,
      date = "2021~08~15",
      fun_agg = "mean"
    )
  )

  site_faux_r <- site_faux
  names(site_faux_r)[1] <- "ID"
  testthat::expect_error(
    calculate_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = site_faux_r
    )
  )
  testthat::expect_error(
    calculate_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = matrix(c(1, 3, 4, 5), nrow = 2)
    )
  )
  testthat::expect_error(
    calculate_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux)
    )
  )
  testthat::expect_error(
    calculate_modis_daily(
      from = terra::rast(nrow = 3, ncol = 3, vals = 1:9, names = "a"),
      date = "2021-08-15",
      locs = array(1:12, dim = c(2, 2, 3))
    )
  )
  site_faux0 <- site_faux
  names(site_faux0)[2] <- "date"
  testthat::expect_error(
    calculate_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux0)
    )
  )
  site_faux2 <- site_faux
  # site_faux2[, 4] <- NULL

  path_mcd19 <-
    testthat::test_path(
      "../testdata/modis/",
      "MCD19A2.A2021227.h11v05.061.2023149160635.hdf"
    )
  testthat::expect_no_warning(
    mcd_merge <-
      process_modis_merge(
        path = path_mcd19,
        date = "2021-08-15",
        subdataset = "(Optical_Depth)"
      )
  )

  testthat::expect_no_error(
    calculate_modis_daily(
      from = mcd_merge,
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux2),
      radius = 1000,
      name_extracted = "MCD_EXTR_1K_"
    )
  )

  # test calculate_modis_daily directly with geometry terra
  testthat::expect_no_error(
    calc_mod_terra <- calculate_modis_daily(
      from = mcd_merge,
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux2),
      radius = 1000,
      name_extracted = "MCD_EXTR_1K_",
      geom = "terra"
    )
  )
  testthat::expect_s4_class(calc_mod_terra, "SpatVector")

  # test calculate_modis_daily directly with geometry sf
  testthat::expect_no_error(
    calc_mod_sf <- calculate_modis_daily(
      from = mcd_merge,
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux2),
      radius = 1000,
      name_extracted = "MCD_EXTR_1K_",
      geom = "sf"
    )
  )
  testthat::expect_true(inherits(calc_mod_sf, "sf"))

  testthat::expect_no_error({
    from_w <- terra::rast(
      nrows = 2,
      ncols = 2,
      xmin = 0,
      xmax = 2000,
      ymin = 0,
      ymax = 2000,
      crs = "EPSG:3857"
    )
    terra::values(from_w) <- c(1, 2, 3, 4)
    names(from_w) <- "ndvi"
    locs_w <- terra::vect(
      data.frame(lon = 1000, lat = 1000, site_id = "001"),
      geom = c("lon", "lat"),
      crs = "EPSG:3857",
      keepgeom = TRUE
    )
    weights_w <- from_w
    terra::values(weights_w) <- c(1, 1, 1, 10)

    mod_unweighted <- calculate_modis_daily(
      from = from_w,
      locs = locs_w,
      locs_id = "site_id",
      radius = 1200,
      date = "2021-08-15",
      name_extracted = "ndvi_01200",
      scale = "* 1"
    )
    mod_weighted <- calculate_modis_daily(
      from = from_w,
      locs = locs_w,
      locs_id = "site_id",
      radius = 1200,
      date = "2021-08-15",
      name_extracted = "ndvi_01200",
      weights = weights_w,
      scale = "* 1"
    )
    testthat::expect_true(
      mod_weighted$ndvi_01200 != mod_unweighted$ndvi_01200
    )
  })

  testthat::expect_error(
    calculate_modis(from = site_faux, scale = "* 1")
  )
  testthat::expect_error(
    calculate_modis(
      from = path_mod11,
      product = "MOD11A1",
      locs = list(1, 2, 3),
      scale = "* 1"
    )
  )
  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      preprocess = "fountain",
      name_covariates = c("MOD_NITLT_0_", "MOD_K1_"),
      subdataset = 3L
    )
  )

  # Test expects name_covariates warning (may also get scale or unknown extent)
  testthat::expect_warning(
    calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      preprocess = process_blackmarble,
      name_covariates = c("MOD_NITLT_0_", "MOD_K1_"),
      subdataset = 3L,
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5))
    ),
    "name_covariates|unknown extent|scale"
  )

  # Test with negative radius
  testthat::expect_no_error(
    flushed <- calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      name_covariates = c("MOD_NITLT_0_"),
      preprocess = process_blackmarble,
      subdataset = 3L,
      radius = c(-1000, 0L),
      scale = "* 1"
    )
  )
  testthat::expect_s3_class(flushed, "data.frame")
  testthat::expect_true(unlist(flushed[, 2]) == -99999)

  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      name_covariates = c("MOD_NITLT_0_"),
      preprocess = process_blackmarble,
      subdataset = 3L,
      radius = c(-1000, 0L),
      scale = 0.01
    )
  )
})

testthat::test_that("calculate_modis handles monthly MOD14CM1 filenames", {
  withr::local_package("terra")

  locs <- terra::vect(
    data.frame(site_id = "site-1", lon = 0, lat = 0),
    geom = c("lon", "lat"),
    keepgeom = TRUE,
    crs = "EPSG:4326"
  )

  mock_preprocess <- function(path, date, ...) {
    out <- terra::rast(
      ncols = 1,
      nrows = 1,
      xmin = -1,
      xmax = 1,
      ymin = -1,
      ymax = 1,
      crs = "EPSG:4326"
    )
    terra::values(out) <- 1
    names(out) <- "fire_mask"
    out
  }

  monthly_result <- calculate_modis(
    from = c(
      "MOD14CM1.200011.005.01.hdf",
      "MOD14CM1.200012.005.01.hdf"
    ),
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    preprocess = mock_preprocess,
    name_covariates = "fire_mask_",
    subdataset = "(FireMask)",
    scale = "* 1"
  )

  testthat::expect_true(is.data.frame(monthly_result))
  testthat::expect_equal(nrow(monthly_result), 2L)
  testthat::expect_true("fire_mask_00000" %in% names(monthly_result))
  testthat::expect_equal(
    format(as.Date(monthly_result$time), "%Y-%m-%d"),
    c("2000-11-01", "2000-12-01")
  )
})

testthat::test_that("calculate_modis supports direct SpatRaster mode and type pairing", {
  withr::local_package("terra")

  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = 0, lat = 0),
    coords = c("lon", "lat"),
    crs = 4326
  )

  r_primary <- terra::rast(
    ncols = 1,
    nrows = 1,
    xmin = -1,
    xmax = 1,
    ymin = -1,
    ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(r_primary) <- 10
  names(r_primary) <- "mock_layer"

  r_secondary <- r_primary
  terra::values(r_secondary) <- 20

  testthat::expect_no_error(
    direct_result <- calculate_modis(
      from = r_primary,
      locs = locs,
      locs_id = "site_id",
      radius = 0L,
      name_covariates = "mock_",
      preprocess = "ignored_in_raster_mode",
      scale = "* 1"
    )
  )
  testthat::expect_true(is.data.frame(direct_result))
  testthat::expect_true("mock_00000" %in% names(direct_result))
  testthat::expect_true(all(is.na(direct_result$time)))

  fused_primary <- calculate_modis(
    from = r_primary,
    from_secondary = r_secondary,
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    name_covariates = "mock_",
    fusion_method = "primary_first",
    scale = "* 1"
  )
  fused_secondary <- calculate_modis(
    from = r_primary,
    from_secondary = r_secondary,
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    name_covariates = "mock_",
    fusion_method = "secondary_first",
    scale = "* 1"
  )
  fused_mean <- calculate_modis(
    from = r_primary,
    from_secondary = r_secondary,
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    name_covariates = "mock_",
    fusion_method = "mean",
    scale = "* 1"
  )
  testthat::expect_equal(fused_primary$mock_00000, 10)
  testthat::expect_equal(fused_secondary$mock_00000, 20)
  testthat::expect_equal(fused_mean$mock_00000, 15, tolerance = 1e-5)

  testthat::expect_error(
    calculate_modis(
      from = r_primary,
      from_secondary = "MOD11A1.A2021001.h11v05.061.1234567890123.hdf",
      scale = "* 1"
    ),
    "from_secondary should be SpatRaster"
  )
  testthat::expect_error(
    calculate_modis(
      from = "MOD11A1.A2021001.h11v05.061.1234567890123.hdf",
      from_secondary = r_secondary,
      scale = "* 1"
    ),
    "from_secondary should be a character vector"
  )
})


testthat::test_that("calculate_modis drops insufficient dates and fills try-error extracts", {
  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = -78.8, lat = 35.9),
    coords = c("lon", "lat"),
    crs = 4326
  )

  fake_from <- c(
    "MOD09GA.A2021001.h10v05.061.2021001000000.hdf",
    "MOD09GA.A2021001.h11v05.061.2021001000001.hdf",
    "MOD09GA.A2021001.h12v05.061.2021001000002.hdf",
    "MOD09GA.A2021002.h10v05.061.2021002000000.hdf",
    "MOD09GA.A2021002.h11v05.061.2021002000001.hdf",
    "MOD09GA.A2021002.h12v05.061.2021002000002.hdf",
    "MOD09GA.A2021003.h10v05.061.2021003000000.hdf"
  )

  testthat::local_mocked_bindings(
    calculate_modis_daily = function(...) {
      structure("forced extract failure", class = "try-error")
    },
    .package = "amadeus"
  )

  fake_preprocess <- function(path, date, ...) {
    out <- terra::rast(
      ncols = 1,
      nrows = 1,
      xmin = -79,
      xmax = -78,
      ymin = 35,
      ymax = 36,
      crs = "EPSG:4326"
    )
    terra::values(out) <- 1
    names(out) <- "mock_layer"
    out
  }

  testthat::expect_message(
    result <- calculate_modis(
      from = fake_from,
      locs = locs,
      locs_id = "site_id",
      name_covariates = "mock_cov_",
      preprocess = fake_preprocess,
      radius = c(0L, 1000L),
      scale = "* 1"
    ),
    "insufficient"
  )

  testthat::expect_equal(
    as.character(attr(result, "dates_dropped")),
    "2021-01-03"
  )
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(
    as.character(result$time),
    c("2021-01-01", "2021-01-02")
  )
  testthat::expect_true(all(result$mock_cov_00000 == -99999))
  testthat::expect_true(all(result$mock_cov_01000 == -99999))
})

# nolint end

################################################################################
##### download_modis single-date and hash=FALSE branches (no skip_on_cran)

testthat::test_that("download_modis single date branch (mock, no skip)", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(
                list(
                  rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                  href = paste0(
                    "https://e4ftl01.cr.usgs.gov/MOLT/MOD09GA.061/",
                    "2023.01.01/MOD09GA.A2023001.h10v05.061.hdf"
                  )
                )
              )
            )
          )
        )
      )
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1)
  })
})

################################################################################
##### download_modis remove_command, 31-day warning, no granules (no skip)

testthat::test_that("download_modis no granules found path (no skip)", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(feed = list(entry = list())) # Empty entry list -> no granules
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_error(
      suppressWarnings(
        suppressMessages(
          download_modis(
            date = "2023-01-01",
            product = "MOD09GA",
            directory_to_save = ".",
            acknowledgement = TRUE,
            hash = FALSE
          )
        )
      ),
      "No granules found"
    )
  })
})

testthat::test_that("download_modis surfaces CMR query failures", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(...) stop("mock cmr outage"),
    .package = "httr2"
  )

  withr::with_tempdir({
    testthat::expect_error(
      download_modis(
        date = "2023-01-01",
        product = "MOD14A1",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      ),
      "Failed to query NASA CMR"
    )
  })
})

testthat::test_that("download_modis no in-range granules found after filtering", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(list(
                rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                href = paste0(
                  "https://example.com/",
                  "MOD14A1.A2021230.h11v05.061.2021234567890.hdf"
                )
              ))
            )
          )
        )
      )
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_error(
      suppressWarnings(
        suppressMessages(
          download_modis(
            date = "2021-08-15",
            product = "MOD14A1",
            directory_to_save = ".",
            acknowledgement = TRUE,
            download = FALSE
          )
        )
      ),
      "No granules matched the requested date range"
    )
  })
})

################################################################################
##### download_modis remove_command warning (covers lines 2707-2710)

testthat::test_that("download_modis remove_command deprecated warning", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      list(success = 1, failed = 0, skipped = 0)
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(list(
                rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                href = paste0(
                  "https://example.com/",
                  "MOD09GA.A2023001.h08v04.006.hdf"
                )
              ))
            )
          )
        )
      )
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          remove_command = TRUE,
          hash = FALSE
        )
      ),
      "remove_command.*deprecated"
    )
  })
})

################################################################################
##### download_modis 31-day warning (covers lines 2722-2724)

testthat::test_that("download_modis 31-day range warning (no skip)", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      list(success = 1, failed = 0, skipped = 0)
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(list(
                rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                href = paste0(
                  "https://example.com/",
                  "MOD09GA.A2023001.h08v04.006.hdf"
                )
              ))
            )
          )
        )
      )
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      suppressMessages(
        download_modis(
          date = c("2023-01-01", "2023-03-01"),
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = FALSE
        )
      ),
      "31 days"
    )
  })
})

################################################################################
##### download_modis hash=TRUE (covers line 2821)

testthat::test_that("download_modis hash=TRUE returns fakehash (no skip)", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      list(success = 1, failed = 0, skipped = 0)
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(list(
                rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                href = paste0(
                  "https://example.com/",
                  "MOD09GA.A2023001.h08v04.006.hdf"
                )
              ))
            )
          )
        )
      )
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

################################################################################
##### download_modis cross-year dates stop (covers lines 2690-2691)

testthat::test_that("download_modis cross-year dates non-MOD06_L2 stops", {
  testthat::expect_error(
    suppressMessages(
      download_modis(
        date = c("2022-12-01", "2023-01-01"),
        product = "MOD09GA",
        directory_to_save = ".",
        acknowledgement = TRUE,
        version = "061",
        nasa_earth_data_token = "mock-token"
      )
    ),
    "same year"
  )
})

################################################################################
##### download_modis NULL version is rejected before token lookup

testthat::test_that("download_modis version=NULL is rejected by null-parameter checks", {
  withr::with_tempdir({
    testthat::expect_error(
      suppressMessages(
        download_modis(
          date = c("2023-01-01", "2023-01-01"),
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          version = NULL,
          nasa_earth_data_token = "mock-token"
        )
      ),
      "null|NULL"
    )
  })
})

################################################################################
##### download_modis product=MOD06_L2 str_version (covers line 2730)

testthat::test_that("download_modis MOD06_L2 uses version 6.1 (no skip)", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      list(success = 1, failed = 0, skipped = 0)
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(list(
                rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                href = paste0(
                  "https://example.com/",
                  "MOD06_L2.A2023001.hdf"
                )
              ))
            )
          )
        )
      )
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "MOD06_L2",
          directory_to_save = ".",
          acknowledgement = TRUE,
          version = "006",
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
  })
})

################################################################################
##### download_modis product=VNP46A2 str_version=NULL (covers line 2732)

testthat::test_that("download_modis VNP46A2 sets str_version to NULL", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      list(success = 1, failed = 0, skipped = 0)
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(list(
                rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                href = paste0(
                  "https://example.com/",
                  "VNP46A2.A2023001.h08v05.001.hdf"
                )
              ))
            )
          )
        )
      )
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "VNP46A2",
          directory_to_save = ".",
          acknowledgement = TRUE,
          version = "001",
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
  })
})

################################################################################
##### calculate_modis no-files-for-fusion-date coverage

testthat::test_that("calculate_modis errors when no files match selected fusion date key", {
  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = -78.8, lat = 35.9),
    coords = c("lon", "lat"),
    crs = 4326
  )
  from_primary <- "MOD09GA.A2021001.h10v05.061.2021001000000.hdf"
  from_secondary <- "MYD09GA.A2021001.h10v05.061.2021001000000.hdf"

  call_count <- 0L
  # First two calls (building dates_available) return real key;
  # subsequent calls (has_primary / has_secondary checks) return a bogus key,
  # making !has_primary && !has_secondary TRUE.
  testthat::local_mocked_bindings(
    modis_extract_temporal_key = function(x, ...) {
      call_count <<- call_count + 1L
      if (call_count <= 2L) "2021001" else "0000000"
    },
    modis_extract_temporal_scale = function(x, ...) "daily",
    .package = "amadeus"
  )

  testthat::expect_error(
    calculate_modis(
      from = from_primary,
      from_secondary = from_secondary,
      locs = locs,
      locs_id = "site_id",
      radius = 0L,
      preprocess = function(...) terra::rast(),
      name_covariates = "cov_",
      subdataset = "mock",
      scale = "* 1",
      fusion_method = "mean"
    ),
    "No MODIS files found"
  )
})
