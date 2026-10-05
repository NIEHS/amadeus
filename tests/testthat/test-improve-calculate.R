improve_calc_path <- testthat::test_path("..", "testdata", "improve")
improve_calc_locs <- function() {
  data.frame(id = c("009", "001", "outside"),
             lon = c(-103.1774, -68.2608, 0),
             lat = c(29.3025, 44.3771, 0))
}

testthat::test_that(
  "calculate_improve(from=<processed products>): preserves matching records",
  {
    for (product in c("raw", "rhr2", "rhr3")) {
      for (format in c("terra", "sf", "data.table")) {
        from <- process_covariates("improve", path = improve_calc_path,
                                   product = product, return_format = format)
        out <- calculate_covariates("IMPROVE", from = from,
                                    locs = improve_calc_locs(), locs_id = "id")
        expected <- as.data.frame(process_improve(
          improve_calc_path, product = product, return_format = "data.table"
        ))
        expected <- expected[order(match(expected$SiteCode,
                                         c("BIBE1", "ACAD1"))), ]
        testthat::expect_s3_class(out, "data.frame")
        testthat::expect_equal(out$id,
          ifelse(expected$SiteCode == "BIBE1", "009", "001"))
        for (column in c("SiteCode", "FactDate", "ParamCode", "FactValue",
                         "Units", "Status", "POC", "MethodID")) {
          testthat::expect_equal(out[[column]], expected[[column]])
        }
        if (product == "raw") {
          testthat::expect_equal(out$FactValue[out$ParamCode == "FPM"],
                                 c(1.98, 2.05, 2.85, 3.12))
          testthat::expect_equal(sum(out$Status == "M1"), 1L)
        }
      }
    }
  }
)

testthat::test_that(
  "calculate_improve(radius=1000, geom=<formats>): aligns buffers and IDs",
  {
    from <- process_improve(improve_calc_path)
    locs <- improve_calc_locs()[2:1, ]
    locs$lon <- locs$lon + 0.005
    locs <- sf::st_transform(sf::st_as_sf(locs, coords = c("lon", "lat"),
                                         crs = 4326), 3857)
    for (geom in c("sf", "terra")) {
      out <- calculate_improve(from, locs, "id", radius = 1000, geom = geom)
      if (geom == "terra") {
        testthat::expect_s4_class(out, "SpatVector")
        out <- sf::st_as_sf(out)
      } else {
        testthat::expect_s3_class(out, "sf")
      }
      testthat::expect_equal(out$id, rep(c("001", "009"), each = 6))
      testthat::expect_equal(out$SiteCode, rep(c("ACAD1", "BIBE1"), each = 6))
      testthat::expect_equal(sf::st_crs(out)$epsg, 4326L)
      testthat::expect_equal(as.character(sf::st_geometry_type(out)),
                             rep("POLYGON", 12))
    }
  }
)

testthat::test_that(
  "calculate_improve(locs=<overlapping polygons>): retains all matches",
  {
    from <- process_improve(improve_calc_path, return_format = "sf")
    locs <- fixture_aoi()
    locs <- rbind(locs, locs)
    locs$id <- c("z", "a")
    out <- calculate_improve(from, locs, "id")
    testthat::expect_equal(out$id, rep(c("z", "a"), each = 12))
    testthat::expect_equal(out$FactValue, rep(from$FactValue, 2))
  }
)

testthat::test_that(
  "calculate_improve(locs=<no matches>): returns typed empty results",
  {
    from <- process_improve(improve_calc_path)
    for (geom in list(FALSE, "sf", "terra")) {
      out <- calculate_improve(from, improve_calc_locs()[3, ], "id",
                               geom = geom)
      testthat::expect_equal(nrow(out), 0L)
      testthat::expect_contains(names(out), c("id", "FactDate", "FactValue"))
    }
  }
)

testthat::test_that(
  "calculate_improve(from=<zero and NA>): preserves values and source input",
  {
    from <- process_improve(improve_calc_path, return_format = "data.table")
    from$FactValue[1:2] <- c(0, NA_real_)
    before <- data.table::copy(from)
    out <- calculate_improve(from, improve_calc_locs()[2, ], "id")
    testthat::expect_equal(out$FactValue[1:2], c(0, NA_real_))
    testthat::expect_equal(from, before)
  }
)

testthat::test_that(
  "calculate_improve(inputs=<invalid>): reports unsupported contracts",
  {
    from <- process_improve(improve_calc_path)
    locs <- improve_calc_locs()
    testthat::expect_error(calculate_improve(from, locs, "missing"), "locs_id")
    testthat::expect_error(calculate_improve(from, locs[c(1, 1), ], "id"),
                           "unique")
    testthat::expect_error(calculate_improve(from, locs, "id", radius = -1),
                           "radius")
    testthat::expect_error(calculate_improve(from, locs, "id", geom = TRUE),
                           "geom")
    testthat::expect_error(calculate_covariates("improve", from, locs, "id",
                                               .by_time = "month"),
                           "requires")
    testthat::expect_error(calculate_covariates("improve", from, locs, "id",
                                               weights = 1), "requires")
    locs$SiteCode <- locs$id
    testthat::expect_error(calculate_improve(from, locs, "SiteCode"),
                           "conflicts")
    tab <- as.data.frame(from)
    testthat::expect_error(calculate_improve(tab, locs, "id"), "Longitude")
  }
)

testthat::test_that(
  "download_data(dataset_name='improve'): hands files through calculation",
  {
    testthat::skip_if(Sys.which("zip") == "", "zip executable unavailable")
    local_download_mocks(download_run_method = function(urls, destfiles, ...) {
      for (i in seq_along(destfiles)) {
        fixture <- file.path(improve_calc_path,
                             sub("\\.zip$", "", basename(destfiles[i])))
        utils::zip(destfiles[i], fixture, flags = "-jq")
      }
      list(success = length(destfiles), failed = 0)
    })
    path <- withr::local_tempdir()
    file.copy(file.path(improve_calc_path, "improve_sites.txt"), path)
    for (product in c("raw", "rhr2", "rhr3")) {
      download_data("improve", directory_to_save = path,
                    acknowledgement = TRUE, year = 2022, product = product)
      from <- process_covariates("improve", path = path, product = product)
      out <- calculate_covariates("improve", from, improve_calc_locs(), "id")
      testthat::expect_equal(unique(out$id), c("009", "001"))
      testthat::expect_equal(nrow(out), if (product == "raw") 12L else 4L)
      testthat::expect_equal(out$FactValue[1],
        switch(product, raw = 0.0012, rhr2 = 8.9, rhr3 = 0.98))
    }
  }
)

testthat::test_that(
  "calculate_improve(radius=1000): excludes distant stations",
  {
    from <- process_improve(improve_calc_path)
    locs <- improve_calc_locs()[2, ]
    locs$lon <- locs$lon + 0.02
    out <- calculate_improve(from, locs, "id", radius = 1000)
    testthat::expect_equal(nrow(out), 0L)
  }
)

testthat::test_that(
  "calculate_improve(locs=<polygon boundary>): includes boundary records",
  {
    from <- sf::st_as_sf(data.frame(
      SiteCode = "test", FactDate = as.Date("2022-01-02"),
      ParamCode = "FPM", FactValue = 7, Units = "ug/m^3", x = 0, y = 0
    ), coords = c("x", "y"), crs = 3857)
    locs <- terra::vect(terra::ext(0, 100, -100, 100), crs = "EPSG:3857")
    locs$id <- "001"
    out <- calculate_improve(from, locs, "id", geom = "sf")
    testthat::expect_equal(out$FactValue, 7)
    testthat::expect_equal(out$id, "001")
    testthat::expect_equal(sf::st_crs(out)$epsg, 3857L)
    testthat::expect_equal(
      unname(sf::st_equals(out, sf::st_as_sf(locs), sparse = FALSE)),
      matrix(TRUE, 1, 1)
    )
  }
)
