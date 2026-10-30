################################################################################
##### unit and integration tests for NASA SEDAC gRoads functions

################################################################################
##### download_groads
testthat::test_that("download_groads", {
  withr::local_package("httr2")
  withr::local_package("stringr")
  withr::local_envvar(c(NASA_EARTHDATA_TOKEN = "mock-token"))
  # function parameters
  data_regions <- c("Americas", "Global")
  data_formats <- c("Geodatabase", "Shapefile")
  directory_to_save <- paste0(tempdir(), "/groads/")

  # run download function
  for (r in seq_along(data_regions)) {
    data_region <- data_regions[r]
    for (f in seq_along(data_formats)) {
      # Clean directory before test
      if (dir.exists(directory_to_save)) {
        unlink(directory_to_save, recursive = TRUE)
      }

      testthat::expect_warning(
        download_data(
          dataset_name = "sedac_groads",
          directory_to_save = directory_to_save,
          acknowledgement = TRUE,
          data_format = data_formats[f],
          data_region = data_region,
          download = FALSE,
          unzip = FALSE,
          remove_zip = FALSE,
          remove_command = FALSE
        ),
        "download=FALSE is deprecated"
      )

      # Check that directory was created
      testthat::expect_true(
        dir.exists(directory_to_save)
      )

      # Check that subdirectories exist
      subdirs <- list.files(
        directory_to_save,
        include.dirs = TRUE,
        full.names = FALSE
      )

      testthat::expect_true(
        length(subdirs) >= 1
      )

      # define file path with commands
      commands_path <- paste0(
        download_sanitize_path(directory_to_save),
        "sedac_groads_",
        gsub(" ", "_", tolower(data_region)),
        "_",
        Sys.Date(),
        "_curl_command.txt"
      )

      # Only proceed with command file tests if it exists
      if (file.exists(commands_path)) {
        # import commands
        commands <- read_commands(commands_path = commands_path)
        # extract urls
        urls <- extract_urls(commands = commands, position = 11)
        # check HTTP URL status
        url_status <- check_urls(urls = urls, size = 1L)
        # implement unit tests
        test_download_functions(
          directory_to_save = directory_to_save,
          commands_path = commands_path,
          url_status = url_status
        )
        # remove file with commands after test
        file.remove(commands_path)
      }
    }
  }

  testthat::expect_warning(
    download_data(
      dataset_name = "sedac_groads",
      data_format = "Shapefile",
      data_region = "Global",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      unzip = FALSE,
      remove_zip = FALSE,
      remove_command = TRUE
    ),
    "download=FALSE|remove_command"
  )
  # remove temporary groads
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_groads mock download with hash", {
  withr::local_envvar(c(NASA_EARTHDATA_TOKEN = "mock-token"))
  testthat::local_mocked_bindings(
    download_run_method = function(...) invisible(NULL),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_groads(
          data_region = "Global",
          data_format = "Shapefile",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

################################################################################
##### process_groads
testthat::test_that("process_groads", {
  withr::local_package("terra")

  # main test
  testthat::expect_no_error(
    groads <- process_groads(
      path = testthat::test_path("../testdata/groads/groads_test.shp")
    )
  )
  # expect
  testthat::expect_s4_class(groads, "SpatVector")
  # error cases
  testthat::expect_error(
    process_groads(path = 1L),
    regexp = "must be a single file path to a \\.shp or \\.gdb roads file"
  )
  testthat::expect_error(
    process_groads(path = "does/not/exist.shp"),
    regexp = "`path` does not exist"
  )
  withr::with_tempdir({
    bad_path <- file.path(".", "roads.txt")
    writeLines("not a roads vector file", con = bad_path)
    testthat::expect_error(
      process_groads(path = bad_path),
      regexp = "must point to a \\.shp or \\.gdb file"
    )
  })
  testthat::expect_error(
    process_groads(path = NA_character_),
    regexp = "must be a single file path to a \\.shp or \\.gdb roads file"
  )
  # test with cropping extent
  testthat::expect_no_error(
    groads_ext <- process_groads(
      path = testthat::test_path("../testdata/groads/groads_test.shp"),
      extent = terra::ext(groads)
    )
  )
})

################################################################################
##### calculate_groads
# An equatorial geodesic has length a * longitude_difference (radians).
# This gives an independent ellipsoidal reference, including survey feet.
groads_equatorial_fixture <- function(half_length_m = 500 * 1200 / 3937) {
  delta <- half_length_m / 6378137 * 180 / pi
  roads <- terra::vect(
    sprintf("LINESTRING (%.15f 0, %.15f 0)", 3 - delta, 3 + delta),
    crs = "EPSG:4326"
  )
  list(
    roads = roads,
    locs = data.frame(site_id = "center", lon = 3, lat = 0),
    length_km = 2 * half_length_m / 1000
  )
}

groads_test_crs <- c(
  geographic = "EPSG:4326",
  metres = "+proj=utm +zone=31 +datum=WGS84 +units=m",
  feet = "+proj=utm +zone=31 +datum=WGS84 +units=ft",
  survey_feet = "+proj=utm +zone=31 +datum=WGS84 +units=us-ft"
)

for (crs_name in names(groads_test_crs)) {
  testthat::test_that(
    sprintf(
      "calculate_groads(from=process_groads(), crs='%s'): returns km and km/km2",
      crs_name
    ),
    {
      fixture <- groads_equatorial_fixture()
      roads <- terra::project(fixture$roads, groads_test_crs[[crs_name]])
      # A road ID with the same name must not replace the location ID.
      roads$site_id <- "road-id"
      path <- file.path(withr::local_tempdir(), "roads.shp")
      terra::writeVector(roads, path)
      processed <- process_groads(path)
      testthat::expect_equal(terra::linearUnits(processed),
                             terra::linearUnits(roads))
      locs <- terra::vect(fixture$locs, geom = c("lon", "lat"),
                          crs = "EPSG:4326")
      locs <- terra::project(locs, groads_test_crs[[crs_name]])
      result <- calculate_groads(
        from = processed, locs = locs, locs_id = "site_id", radius = 1000,
        geom = "terra"
      )
      testthat::expect_s4_class(result, "SpatVector")
      testthat::expect_identical(result$site_id, "center")
      testthat::expect_equal(result$GRD_TOTAL_01000, fixture$length_km,
                             tolerance = 1e-6)
      # 720-sided geographic buffer approximates a 1 km radius circle.
      testthat::expect_equal(result$GRD_DENKM_01000,
                             fixture$length_km / pi, tolerance = 2e-5)
      testthat::expect_identical(terra::crs(result), terra::crs(processed))
      testthat::expect_equal(
        terra::expanse(result, unit = "km"), pi, tolerance = 2e-5
      )
    }
  )
  testthat::test_that(
    sprintf(
      "calculate_groads(crs='%s', radius=1000): clips a crossing road to 2 km",
      crs_name
    ),
    {
      fixture <- groads_equatorial_fixture(half_length_m = 2000)
      # Tabular WGS84 locations deliberately differ from projected roads.
      result <- calculate_groads(
        from = terra::project(fixture$roads, groads_test_crs[[crs_name]]),
        locs = fixture$locs, locs_id = "site_id", radius = 1000
      )
      testthat::expect_equal(result$GRD_TOTAL_01000, 2, tolerance = 1e-6)
      testthat::expect_equal(result$GRD_DENKM_01000, 2 / pi,
                             tolerance = 2e-5)
    }
  )
}

testthat::test_that(
  "calculate_groads(latitude=60, CRS=m/ft/us-ft): agrees with geographic input",
  {
    roads <- terra::vect("LINESTRING (2.997 60, 3.003 60)", crs = "EPSG:4326")
    locs <- data.frame(site_id = "north", lon = 3, lat = 60)
    reference <- calculate_groads(roads, locs, "site_id")
    testthat::expect_equal(reference$GRD_TOTAL_01000,
                           terra::perim(roads) / 1000, tolerance = 1e-8)
    for (crs in groads_test_crs[-1]) {
      result <- calculate_groads(terra::project(roads, crs), locs, "site_id")
      testthat::expect_equal(result, reference, tolerance = 1e-6)
    }
  }
)

testthat::test_that(
  "calculate_groads(locs=sf, geom='sf'): transforms mixed CRSs and retains order",
  {
    fixture <- groads_equatorial_fixture()
    locs <- sf::st_as_sf(
      data.frame(site_id = c("z", "a"), lon = c(3, 3), lat = c(0, 0)),
      coords = c("lon", "lat"), crs = 4326
    )
    locs <- sf::st_transform(locs, groads_test_crs[["survey_feet"]])
    roads <- terra::project(fixture$roads, groads_test_crs[["metres"]])
    roads$site_id <- "road-id"
    roads$.amadeus_site <- 999L
    result <- calculate_groads(roads, locs, "site_id", geom = "sf")
    testthat::expect_s3_class(result, "sf")
    testthat::expect_identical(result$site_id, c("z", "a"))
    testthat::expect_equal(result$GRD_TOTAL_01000,
                           rep(fixture$length_km, 2), tolerance = 1e-6)
    testthat::expect_equal(sf::st_crs(result), sf::st_crs(terra::crs(roads)))
  }
)

testthat::test_that(
  "calculate_groads(locs=polygons, radius=100): divides by each site's own area",
  {
    locs <- terra::vect(c(
      "POLYGON ((2.999 -0.001,3.001 -0.001,3.001 0.001,2.999 0.001,2.999 -0.001))",
      "POLYGON ((3.045 -0.005,3.055 -0.005,3.055 0.005,3.045 0.005,3.045 -0.005))"
    ), crs = "EPSG:4326")
    locs$site_id <- c("z", "a")
    roads <- terra::vect("LINESTRING (2.98 0,3.07 0)", crs = "EPSG:4326")
    result <- calculate_groads(roads, locs, "site_id", radius = 100,
                                geom = "terra")
    areas <- terra::expanse(result, unit = "km")
    testthat::expect_gt(areas[2], 5 * areas[1])
    testthat::expect_equal(result$GRD_DENKM_00100,
                           result$GRD_TOTAL_00100 / areas, tolerance = 1e-8)
    testthat::expect_identical(result$site_id, c("z", "a"))
  }
)

for (return_geom in list(FALSE, "sf", "terra")) {
  testthat::test_that(
    sprintf(
      "calculate_groads(geom='%s', drop=FALSE/TRUE): handles all-outside sites",
      return_geom
    ),
    {
      fixture <- groads_equatorial_fixture()
      # The combined extent includes the road, but neither buffer does.
      locs <- data.frame(site_id = c("west", "east"), lon = c(2, 4), lat = 0)
      result <- calculate_groads(fixture$roads, locs, "site_id",
                                 geom = return_geom)
      testthat::expect_equal(nrow(result), 2L)
      testthat::expect_equal(result$GRD_TOTAL_01000, c(0, 0))
      testthat::expect_equal(result$GRD_DENKM_01000, c(0, 0))
      dropped <- calculate_groads(fixture$roads, locs, "site_id",
                                  geom = return_geom, drop = TRUE)
      testthat::expect_equal(nrow(dropped), 0L)
      if (identical(return_geom, "sf")) {
        testthat::expect_s3_class(dropped, "sf")
      } else if (identical(return_geom, "terra")) {
        testthat::expect_s4_class(dropped, "SpatVector")
      }
      empty <- calculate_groads(fixture$roads, locs[0, ], "site_id",
                                geom = return_geom)
      testthat::expect_equal(nrow(empty), 0L)
    }
  )
}

testthat::test_that(
  "calculate_groads(locs=partial_overlap, drop=TRUE): retains only matching sites",
  {
    fixture <- groads_equatorial_fixture()
    locs <- rbind(fixture$locs, data.frame(site_id = "outside", lon = 4, lat = 0))
    result <- calculate_groads(fixture$roads, locs, "site_id")
    testthat::expect_identical(result$site_id, c("center", "outside"))
    testthat::expect_equal(result$GRD_TOTAL_01000,
                           c(fixture$length_km, 0), tolerance = 1e-6)
    dropped <- calculate_groads(fixture$roads, locs, "site_id", drop = TRUE)
    testthat::expect_identical(dropped$site_id, "center")
    empty_roads <- fixture$roads[FALSE, ]
    empty <- calculate_groads(empty_roads, fixture$locs, "site_id")
    testthat::expect_equal(empty$GRD_TOTAL_01000, 0)
  }
)

testthat::test_that(
  "calculate_groads(from=split_lines, fun=sum): is invariant to segmentation",
  {
    fixture <- groads_equatorial_fixture()
    coordinates <- terra::crds(fixture$roads)
    split <- terra::vect(c(
      sprintf("LINESTRING (%.15f 0, 3 0)", coordinates[1, 1]),
      sprintf("LINESTRING (3 0, %.15f 0)", coordinates[2, 1])
    ), crs = "EPSG:4326")
    result <- calculate_groads(split, fixture$locs, "site_id", fun = base::sum)
    testthat::expect_equal(result$GRD_TOTAL_01000, fixture$length_km,
                           tolerance = 1e-8)
  }
)

testthat::test_that(
  "calculate_groads(radius=invalid, CRS=missing): rejects invalid inputs",
  {
    fixture <- groads_equatorial_fixture()
    for (radius in list(0, -1, NA_real_, Inf, numeric(0), c(1, 2), "1000", 1.5)) {
      testthat::expect_error(
        calculate_groads(fixture$roads, fixture$locs, "site_id", radius = radius),
        "radius.*single positive"
      )
    }
    testthat::expect_error(
      calculate_groads(fixture$roads, fixture$locs, "site_id", fun = "mean"),
      "fun.*sum"
    )
    testthat::expect_error(
      calculate_groads(fixture$roads, fixture$locs, "site_id", weights = 1),
      "weights.*not supported"
    )
    testthat::expect_error(
      calculate_groads(fixture$roads, fixture$locs, "site_id", drop = NA),
      "drop.*single logical"
    )
    testthat::expect_error(
      calculate_groads(fixture$roads, fixture$locs, "site_id", geom = TRUE)
    )
    invalid <- fixture$locs
    invalid$lat <- NA_real_
    testthat::expect_error(
      calculate_groads(fixture$roads, invalid, "site_id")
    )
    duplicate <- rbind(fixture$locs, fixture$locs)
    testthat::expect_error(
      calculate_groads(fixture$roads, duplicate, "site_id"), "unique"
    )
    missing <- fixture$locs
    missing$site_id <- NA_character_
    testthat::expect_error(
      calculate_groads(fixture$roads, missing, "site_id"), "non-missing"
    )
    invalid_road <- terra::vect("LINESTRING (3 0,3 0)", crs = "EPSG:4326")
    testthat::expect_error(
      calculate_groads(invalid_road, fixture$locs, "site_id"),
      "Road geometries.*valid"
    )
    locs <- terra::vect(fixture$locs, geom = c("lon", "lat"), crs = "EPSG:4326")
    testthat::expect_error(
      calculate_groads(locs, fixture$locs, "site_id"), "road lines"
    )
    terra::crs(locs) <- ""
    testthat::expect_error(
      calculate_groads(fixture$roads, locs, "site_id"), "locs.*known CRS"
    )
    terra::crs(fixture$roads) <- ""
    testthat::expect_error(
      calculate_groads(fixture$roads, fixture$locs, "site_id"), "from.*known CRS"
    )
  }
)

################################################################################
##### download_groads hash=FALSE and file-already-exists branches

testthat::test_that("download_groads mock download hash=FALSE", {
  withr::local_envvar(c(NASA_EARTHDATA_TOKEN = "mock-token"))
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_groads(
          data_region = "Americas",
          data_format = "Shapefile",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      )
    )
    testthat::expect_null(result)
  })
})

testthat::test_that("download_groads file already exists path", {
  withr::local_envvar(c(NASA_EARTHDATA_TOKEN = "mock-token"))
  testthat::local_mocked_bindings(
    check_destfile = function(...) FALSE,
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    msgs <- character(0)
    withCallingHandlers(
      suppressWarnings(
        download_groads(
          data_region = "Americas",
          data_format = "Shapefile",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    testthat::expect_true(any(grepl("already exists", msgs)))
  })
})

################################################################################
##### download_groads: NASA EarthData token authentication

testthat::test_that("download_groads has nasa_earth_data_token parameter", {
  params <- names(formals(download_groads))
  testthat::expect_true("nasa_earth_data_token" %in% params)
  testthat::expect_null(formals(download_groads)$nasa_earth_data_token)
})

testthat::test_that("download_groads passes token to download_run_method", {
  captured_token <- NULL
  testthat::local_mocked_bindings(
    download_run_method = function(urls, destfiles, token = NULL, ...) {
      captured_token <<- token
      writeLines("ok", destfiles)
      invisible(list(success = 1, failed = 0))
    },
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    get_token = function(token, env_var) {
      if (is.null(token)) "mock-token" else token
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressWarnings(suppressMessages(
      download_groads(
        data_region = "Americas",
        data_format = "Geodatabase",
        nasa_earth_data_token = "test-token-abc",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        unzip = FALSE,
        hash = FALSE
      )
    ))
  })
  testthat::expect_equal(captured_token, "test-token-abc")
})

testthat::test_that("download_groads reads token from env var via get_token", {
  get_token_called_with <- NULL
  testthat::local_mocked_bindings(
    download_run_method = function(urls, destfiles, token = NULL, ...) {
      writeLines("ok", destfiles)
      invisible(list(success = 1, failed = 0))
    },
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    get_token = function(token, env_var) {
      get_token_called_with <<- list(token = token, env_var = env_var)
      "mock-env-token"
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressWarnings(suppressMessages(
      download_groads(
        data_region = "Americas",
        data_format = "Geodatabase",
        nasa_earth_data_token = NULL,
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        unzip = FALSE,
        hash = FALSE
      )
    ))
  })
  testthat::expect_equal(
    get_token_called_with$env_var,
    "NASA_EARTHDATA_TOKEN"
  )
})
