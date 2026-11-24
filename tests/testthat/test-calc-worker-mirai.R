# The tiny GRIB fixtures retain NOAA HRRR's original band metadata. They were
# cropped from the 2018-01-01 00Z wrfsfcf00/f01 files using GDAL Translate:
# -of GRIB -srcwin 1229 694 3 3
# f00: -b 54 -b 56 -b 57 -b 58 (TMP, SPFH, DPT, RH)
# f01: -b 54 -b 64 (TMP, APCP)
# Source: https://noaa-hrrr-bdp-pds.s3.amazonaws.com/hrrr.20180101/conus/
# Classic and multidimensional readers expose these variables in different
# orders. The complete fixtures total less than 2 KB and require no downloads.

hrrr_worker_layers <- function() {
  ids <- c("TMP_2MAGL", "RH_2MAGL", "DPT_2MAGL", "SPFH_2MAGL", "APCP_SFC")
  lapply(ids, function(id) {
    precipitation <- id == "APCP_SFC"
    accumulation <- if (precipitation) 1L else NULL
    info <- metadata_hrrr(id, accumulation = accumulation)
    process_hrrr(
      date = "2018-01-01",
      variable = info$layer_name,
      forecast_hour = as.integer(precipitation),
      accumulation = accumulation,
      path = testthat::test_path("..", "testdata", "hrrr")
    )
  })
}

################################################################################
testthat::test_that(
  "calc_unwrap(x = GRIB): preserves selected bands after serialization",
  {
    for (layer in hrrr_worker_layers()) {
      packed <- calc_wrap(layer)
      restored <- calc_unwrap(unserialize(serialize(packed, NULL)))
      testthat::expect_equal(terra::values(restored), terra::values(layer))
      testthat::expect_identical(names(restored), names(layer))
      testthat::expect_identical(terra::time(restored), terra::time(layer))
      testthat::expect_identical(terra::units(restored), terra::units(layer))
    }
  }
)

################################################################################
testthat::test_that(
  "calc_unwrap(x = windowed GRIB): preserves cells and revised time",
  {
    layer <- hrrr_worker_layers()[[1]]
    extent <- as.vector(terra::ext(layer))
    extent[1] <- extent[1] + terra::res(layer)[1]
    extent[4] <- extent[4] - terra::res(layer)[2]
    terra::window(layer) <- terra::ext(extent)
    terra::time(layer) <- as.POSIXct("2020-02-03 04:00:00", tz = "UTC")
    restored <- calc_unwrap(calc_wrap(layer))
    testthat::expect_equal(dim(restored), c(2L, 2L, 1L))
    testthat::expect_equal(as.vector(terra::ext(restored)), extent)
    testthat::expect_equal(terra::values(restored), terra::values(layer))
    testthat::expect_identical(terra::time(restored), terra::time(layer))
  }
)

################################################################################
testthat::test_that(
  "calc_unwrap(x = multidimensional GRIB): preserves array identity",
  {
    raster <- terra::rast(
      testthat::test_path(
        "..", "testdata", "hrrr", "hrrr.20180101.t00z.wrfsfcf00.grib2"
      ),
      md = TRUE
    )
    is_md <- tryCatch(raster@pntr$isMD, error = function(e) FALSE)
    testthat::skip_if(!all(is_md), "Multidimensional GRIB reading unavailable")
    for (i in c(4L, 2L)) {
      layer <- raster[[i]]
      names(layer) <- paste0("field_", i)
      restored <- calc_unwrap(calc_wrap(layer))
      testthat::expect_equal(terra::values(restored), terra::values(layer))
      testthat::expect_identical(names(restored), names(layer))
    }
  }
)

################################################################################
testthat::test_that(
  "calc_unwrap(x = in-memory raster or NULL): preserves values",
  {
    layer <- hrrr_worker_layers()[[1]]
    terra::values(layer) <- terra::values(layer) * 2
    restored <- calc_unwrap(calc_wrap(layer))
    testthat::expect_equal(terra::values(restored), terra::values(layer))
    testthat::expect_identical(names(restored), names(layer))
    testthat::expect_null(calc_unwrap(calc_wrap(NULL)))
  }
)

################################################################################
testthat::test_that(
  "calculate_hrrr(daemons = 2): matches sequential GRIB extraction",
  {
    testthat::skip_on_cran()
    testthat::skip_if(mirai::daemons_set(), "An existing daemon pool is active")
    layers <- hrrr_worker_layers()
    locs <- sf::st_as_sf(
      data.frame(
        site_id = seq_len(terra::ncell(layers[[1]])),
        terra::xyFromCell(layers[[1]], seq_len(terra::ncell(layers[[1]])))
      ),
      coords = c("x", "y"), crs = terra::crs(layers[[1]])
    )
    sequential <- lapply(layers, function(layer) {
      calculate_hrrr(layer, locs = locs, locs_id = "site_id")
    })
    columns <- c("tmp_0", "rh_0", "dpt_0", "spfh_0", "apcp_0")
    for (i in seq_along(layers)) {
      testthat::expect_equal(
        sequential[[i]][[columns[i]]], terra::values(layers[[i]])[, 1]
      )
    }

    polygon <- terra::as.polygons(terra::ext(layers[[1]]))
    terra::crs(polygon) <- terra::crs(layers[[1]])
    polygon$site_id <- "region"
    weighted <- calculate_hrrr(
      layers[[1]], locs = polygon, locs_id = "site_id", weights = layers[[2]]
    )

    mirai::daemons(2L)
    withr::defer(mirai::daemons(0L))
    setup <- mirai::everywhere(
      {
        if (file.exists(file.path(package_path, "R", "source-hrrr.R"))) {
          pkgload::load_all(package_path, quiet = TRUE, helpers = FALSE)
        } else {
          library(amadeus)
        }
        NULL
      },
      package_path = getNamespaceInfo(asNamespace("amadeus"), "path")
    )
    mirai::collect_mirai(setup, options = ".stop")
    parallel <- lapply(layers, function(layer) {
      calculate_hrrr(layer, locs = locs, locs_id = "site_id")
    })
    testthat::expect_equal(parallel, sequential)
    testthat::expect_equal(
      calculate_hrrr(
        layers[[1]], locs = polygon, locs_id = "site_id", weights = layers[[2]]
      ),
      weighted
    )
  }
)
