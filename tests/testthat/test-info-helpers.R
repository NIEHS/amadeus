################################################################################
##### helper lookup tests for GEOS, MERRA2, and MODIS

testthat::test_that(
  "get_geos_info(path=<geos fixtures>): returns collection-variable table",
  {
    geos_path <- testthat::test_path("..", "testdata", "geos")
    geos_info <- get_geos_info(path = geos_path)

    testthat::expect_s3_class(geos_info, "data.frame")
    testthat::expect_true(all(c("collection", "variable") %in% names(geos_info)))
    testthat::expect_true(any(geos_info$collection == "aqc_tavg_1hr_g1440x721_v1"))
    testthat::expect_true(any(geos_info$collection == "chm_inst_1hr_g1440x721_p23"))
    testthat::expect_true(any(geos_info$variable == "O3"))

    geos_info_file <- get_geos_info(path = geos_path, include_file = TRUE)
    testthat::expect_true(all(c("collection", "variable", "file") %in% names(geos_info_file)))
    testthat::expect_true(all(grepl("GEOS-CF\\.v01\\.rpl.*\\.nc4$", geos_info_file$file)))
  }
)

testthat::test_that(
  "get_merra2_info(path=<merra fixtures>): returns collection-variable table",
  {
    merra_path <- testthat::test_path("..", "testdata", "merra2")
    merra_info <- get_merra2_info(path = merra_path)

    testthat::expect_s3_class(merra_info, "data.frame")
    testthat::expect_true(all(c("collection", "variable") %in% names(merra_info)))
    testthat::expect_true(any(merra_info$collection == "inst1_2d_int_Nx"))
    testthat::expect_true(any(merra_info$collection == "inst3_2d_gas_Nx"))
    testthat::expect_true(any(merra_info$variable == "CPT"))
    testthat::expect_true(any(merra_info$variable == "AODANA"))
    testthat::expect_true(any(merra_info$variable == "AIRDENS"))

    merra_info_file <- get_merra2_info(path = merra_path, include_file = TRUE)
    testthat::expect_true(all(c("collection", "variable", "file") %in% names(merra_info_file)))
    testthat::expect_true(all(grepl("(MERRA2_400\\..*\\.nc4$|FWI\\..*\\.nc$)", merra_info_file$file)))
  }
)

testthat::test_that(
  "get_modis_info(path=<modis fixtures>): returns product-subdataset table",
  {
    modis_path <- testthat::test_path("..", "testdata", "modis")
    modis_info <- get_modis_info(path = modis_path)

    testthat::expect_s3_class(modis_info, "data.frame")
    testthat::expect_true(all(c("product", "subdataset") %in% names(modis_info)))
    testthat::expect_true(any(modis_info$product == "MOD11A1"))
    testthat::expect_true(any(modis_info$product == "MOD09GA"))
    testthat::expect_true(any(modis_info$product == "MCD19A2"))
    testthat::expect_true(any(modis_info$product == "MOD06_L2"))
    testthat::expect_true(any(modis_info$subdataset == "Optical_Depth_047"))
    testthat::expect_true(any(modis_info$subdataset == "LST_Day_1km"))

    modis_info_file <- get_modis_info(path = modis_path, include_file = TRUE)
    testthat::expect_true(all(c("product", "subdataset", "file") %in% names(modis_info_file)))
    testthat::expect_true(all(grepl("\\.(hdf|h5)$", modis_info_file$file)))
  }
)

testthat::test_that(
  "get_*_info(path=<no files>): returns informative errors",
  {
    withr::with_tempdir({
      testthat::expect_error(
        get_geos_info(path = "."),
        regexp = "No GEOS-CF \\.nc4 files"
      )
      testthat::expect_error(
        get_merra2_info(path = "."),
        regexp = "No MERRA2 netCDF files"
      )
      testthat::expect_error(
        get_modis_info(path = "."),
        regexp = "No MODIS HDF/H5 files"
      )
    })
  }
)
