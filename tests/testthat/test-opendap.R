################################################################################
##### Unit and integration tests for OPeNDAP auxiliary functions and the
##### OPeNDAP code paths in download_merra2(), download_geos(), download_modis()
# nolint start

################################################################################
# Skip helpers for live tests
skip_if_not_live_opendap <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_offline()
  if (Sys.getenv("NASA_EARTHDATA_TOKEN") == "") {
    testthat::skip("NASA_EARTHDATA_TOKEN not set")
  }
}

################################################################################
# extent_to_merra2_indices
################################################################################

testthat::test_that("extent_to_merra2_indices: CONUS extent", {
  result <- extent_to_merra2_indices(c(-125, 22, -64, 50))
  testthat::expect_type(result, "list")
  testthat::expect_named(result, c("lat", "lon"))
  testthat::expect_length(result$lat, 2)
  testthat::expect_length(result$lon, 2)
  # lat: floor((22+90)/0.5)=224, ceil((50+90)/0.5)=280
  testthat::expect_equal(result$lat[1], 224L)
  testthat::expect_equal(result$lat[2], 280L)
  # lon: floor((-125+180)/0.625)=88, ceil((-64+180)/0.625)=186
  testthat::expect_equal(result$lon[1], 88L)
  testthat::expect_equal(result$lon[2], 186L)
})

testthat::test_that("extent_to_merra2_indices: full globe returns full index range", {
  result <- extent_to_merra2_indices(c(-179.9, -89.9, 179.9, 89.9))
  testthat::expect_equal(result$lat[1], 0L)
  testthat::expect_equal(result$lat[2], 360L)
  testthat::expect_equal(result$lon[1], 0L)
  testthat::expect_equal(result$lon[2], 575L)
})

testthat::test_that("extent_to_merra2_indices: indices clamped at boundaries", {
  result <- extent_to_merra2_indices(c(-180, -90, 180, 90))
  testthat::expect_equal(result$lat[1], 0L)
  testthat::expect_equal(result$lat[2], 360L)
  testthat::expect_equal(result$lon[1], 0L)
  testthat::expect_equal(result$lon[2], 575L)
})

testthat::test_that("extent_to_merra2_indices: small extent single tile region", {
  result <- extent_to_merra2_indices(c(-80, 35, -75, 40))
  testthat::expect_true(result$lat[2] > result$lat[1])
  testthat::expect_true(result$lon[2] > result$lon[1])
  testthat::expect_true(result$lat[1] >= 0L && result$lat[2] <= 360L)
  testthat::expect_true(result$lon[1] >= 0L && result$lon[2] <= 575L)
})

testthat::test_that("extent_to_merra2_indices: different areas map to different index windows", {
  west_us <- extent_to_merra2_indices(c(-125, 32, -114, 42))
  europe <- extent_to_merra2_indices(c(5, 45, 15, 55))
  testthat::expect_false(identical(west_us$lat, europe$lat))
  testthat::expect_false(identical(west_us$lon, europe$lon))
  testthat::expect_true(all(west_us$lat >= 0L & west_us$lat <= 360L))
  testthat::expect_true(all(europe$lon >= 0L & europe$lon <= 575L))
})

testthat::test_that("extent_to_merra2_indices: input validation", {
  testthat::expect_error(extent_to_merra2_indices(c(-80, 35, -75)))
  testthat::expect_error(extent_to_merra2_indices(c(-80, 35, -75, "40")))
  testthat::expect_error(extent_to_merra2_indices(c(-181, 35, -75, 40)))
  testthat::expect_error(extent_to_merra2_indices(c(-80, -91, -75, 40)))
  testthat::expect_error(extent_to_merra2_indices(c(-80, 35, 181, 40)))
  testthat::expect_error(extent_to_merra2_indices(c(-80, 35, -75, 91)))
  testthat::expect_error(extent_to_merra2_indices(c(-75, 35, -80, 40)))  # xmin >= xmax
  testthat::expect_error(extent_to_merra2_indices(c(-80, 40, -75, 35)))  # ymin >= ymax
})

################################################################################
# extent_to_geos_indices
################################################################################

testthat::test_that("extent_to_geos_indices: CONUS extent", {
  result <- extent_to_geos_indices(c(-125, 22, -64, 50))
  testthat::expect_type(result, "list")
  testthat::expect_named(result, c("lat", "lon"))
  # lat: floor((22+90)/0.25)=448, ceil((50+90)/0.25)=560
  testthat::expect_equal(result$lat[1], 448L)
  testthat::expect_equal(result$lat[2], 560L)
  # lon: floor((-125+180)/0.25)=220, ceil((-64+180)/0.25)=464
  testthat::expect_equal(result$lon[1], 220L)
  testthat::expect_equal(result$lon[2], 464L)
})

testthat::test_that("extent_to_geos_indices: full globe returns full index range", {
  result <- extent_to_geos_indices(c(-180, -90, 180, 90))
  testthat::expect_equal(result$lat[1], 0L)
  testthat::expect_equal(result$lat[2], 720L)
  testthat::expect_equal(result$lon[1], 0L)
  testthat::expect_equal(result$lon[2], 1439L)
})

testthat::test_that("extent_to_geos_indices: finer resolution than MERRA2", {
  r_merra <- extent_to_merra2_indices(c(-80, 35, -75, 40))
  r_geos  <- extent_to_geos_indices(c(-80, 35, -75, 40))
  # GEOS-CF at 0.25 deg should cover more index points than MERRA2 at 0.5/0.625
  testthat::expect_gt(
    diff(r_geos$lat),
    diff(r_merra$lat)
  )
})

testthat::test_that("extent_to_geos_indices: different areas map to different index windows", {
  east_us <- extent_to_geos_indices(c(-80, 35, -75, 40))
  south_america <- extent_to_geos_indices(c(-70, -35, -60, -25))
  testthat::expect_false(identical(east_us$lat, south_america$lat))
  testthat::expect_false(identical(east_us$lon, south_america$lon))
  testthat::expect_true(all(east_us$lat >= 0L & east_us$lat <= 720L))
  testthat::expect_true(all(south_america$lon >= 0L & south_america$lon <= 1439L))
})

testthat::test_that("extent_to_geos_indices: input validation", {
  testthat::expect_error(extent_to_geos_indices(c(-80, 35, -75)))
  testthat::expect_error(extent_to_geos_indices(c(-181, 35, -75, 40)))
  testthat::expect_error(extent_to_geos_indices(c(-80, 35, -80, 40)))  # xmin == xmax
})

################################################################################
# extent_to_modis_tiles
################################################################################

testthat::test_that("extent_to_modis_tiles: CONUS returns expected tiles", {
  tiles <- extent_to_modis_tiles(c(-125, 22, -64, 50))
  testthat::expect_type(tiles, "character")
  testthat::expect_true(length(tiles) > 0)
  # All tiles must match hXXvYY pattern
  testthat::expect_true(all(grepl("^h[0-9]{2}v[0-9]{2}$", tiles)))
  # Known tiles within CONUS
  testthat::expect_true("h08v05" %in% tiles)  # California
  testthat::expect_true("h12v04" %in% tiles)  # Northeast US
})

testthat::test_that("extent_to_modis_tiles: small single-tile extent", {
  tiles <- extent_to_modis_tiles(c(-80, 35, -75, 40))
  testthat::expect_type(tiles, "character")
  testthat::expect_true(length(tiles) >= 1)
  testthat::expect_true(all(grepl("^h[0-9]{2}v[0-9]{2}$", tiles)))
})

testthat::test_that("extent_to_modis_tiles: full globe returns all non-fill tiles", {
  tiles <- extent_to_modis_tiles(c(-180, -90, 180, 90))
  # 460 non-fill tiles as documented in sn_bound_10deg.txt (nnonfill = 460)
  testthat::expect_equal(length(tiles), 460L)
})

testthat::test_that("extent_to_modis_tiles: equatorial extent", {
  tiles <- extent_to_modis_tiles(c(0, -5, 10, 5))
  testthat::expect_true(all(grepl("^h[0-9]{2}v[0-9]{2}$", tiles)))
  # v=8 is the equatorial row (v = floor((90-5)/10) = 8 to floor((90+5)/10) = 9)
  testthat::expect_true(any(grepl("v08|v09", tiles)))
})

testthat::test_that("extent_to_modis_tiles: input validation", {
  testthat::expect_error(extent_to_modis_tiles(c(-80, 35, -75)))
  testthat::expect_error(extent_to_modis_tiles(c(-181, 35, -75, 40)))
  testthat::expect_error(extent_to_modis_tiles(c(-80, -91, -75, 40)))
  testthat::expect_error(extent_to_modis_tiles(c(-75, 35, -80, 40)))  # xmin > xmax
})

################################################################################
# merra2_collection_ntimes
################################################################################

testthat::test_that("merra2_collection_ntimes: 1-hourly collections return 24", {
  testthat::expect_equal(merra2_collection_ntimes("inst1_2d_int_Nx"), 24L)
  testthat::expect_equal(merra2_collection_ntimes("inst1_2d_asm_Nx"), 24L)
  testthat::expect_equal(merra2_collection_ntimes("tavg1_2d_slv_Nx"), 24L)
  testthat::expect_equal(merra2_collection_ntimes("tavg1_2d_aer_Nx"), 24L)
})

testthat::test_that("merra2_collection_ntimes: 3-hourly collections return 8", {
  testthat::expect_equal(merra2_collection_ntimes("inst3_2d_gas_Nx"), 8L)
  testthat::expect_equal(merra2_collection_ntimes("inst3_3d_asm_Np"), 8L)
  testthat::expect_equal(merra2_collection_ntimes("tavg3_3d_tro_Nv"), 8L)
})

testthat::test_that("merra2_collection_ntimes: daily statistics collections return 1", {
  testthat::expect_equal(merra2_collection_ntimes("statD_2d_slv_Nx"), 1L)
})

testthat::test_that("merra2_collection_ntimes: 6-hourly collections return 4", {
  testthat::expect_equal(merra2_collection_ntimes("inst6_3d_ana_Np"), 4L)
  testthat::expect_equal(merra2_collection_ntimes("tavg6_3d_dyn_Np"), 4L)
})

testthat::test_that("merra2_collection_ntimes: unknown prefix defaults to 24", {
  testthat::expect_equal(merra2_collection_ntimes("monthly_2d_xyz"), 24L)
})

testthat::test_that("merra2_collection_ntimes: input validation", {
  testthat::expect_error(merra2_collection_ntimes(123))
  testthat::expect_error(merra2_collection_ntimes(c("a", "b")))
})

################################################################################
# build_opendap_constraint
################################################################################

testthat::test_that("build_opendap_constraint: NULL variables returns empty string", {
  testthat::expect_equal(
    build_opendap_constraint(variables = NULL),
    ""
  )
})

testthat::test_that("build_opendap_constraint: single variable no dims", {
  result <- build_opendap_constraint(variables = "T2M")
  testthat::expect_equal(result, "T2M")
})

testthat::test_that("build_opendap_constraint: multiple variables no dims", {
  result <- build_opendap_constraint(variables = c("T2M", "U10M"))
  testthat::expect_equal(result, "T2M,U10M")
})

testthat::test_that("build_opendap_constraint: all dimensions specified", {
  result <- build_opendap_constraint(
    variables = c("T2M", "U10M"),
    time_idx  = c(0L, 23L),
    lat_idx   = c(64L, 120L),
    lon_idx   = c(88L, 185L)
  )
  testthat::expect_equal(result, "T2M[0:23][64:120][88:185],U10M[0:23][64:120][88:185]")
})

testthat::test_that("build_opendap_constraint: only lat/lon dims", {
  result <- build_opendap_constraint(
    variables = "T2M",
    lat_idx   = c(64L, 120L),
    lon_idx   = c(88L, 185L)
  )
  testthat::expect_equal(result, "T2M[64:120][88:185]")
})

testthat::test_that("build_opendap_constraint: only time dim", {
  result <- build_opendap_constraint(
    variables = "T2M",
    time_idx  = c(0L, 23L)
  )
  testthat::expect_equal(result, "T2M[0:23]")
})

testthat::test_that("build_opendap_constraint: single time step (start == end)", {
  result <- build_opendap_constraint(
    variables = "T2M",
    time_idx  = c(5L, 5L),
    lat_idx   = c(100L, 100L)
  )
  testthat::expect_equal(result, "T2M[5:5][100:100]")
})

testthat::test_that("build_opendap_constraint: numeric (non-integer) indices coerced", {
  result <- build_opendap_constraint(
    variables = "T2M",
    lat_idx   = c(64.0, 120.0)
  )
  testthat::expect_equal(result, "T2M[64:120]")
})

testthat::test_that("build_opendap_constraint: input validation", {
  testthat::expect_error(
    build_opendap_constraint(variables = character(0))
  )
  testthat::expect_error(
    build_opendap_constraint(variables = 42)
  )
  testthat::expect_error(
    build_opendap_constraint(variables = "T2M", time_idx = c(5L, 3L))
  )
  testthat::expect_error(
    build_opendap_constraint(variables = "T2M", lat_idx = c(100L, 90L))
  )
  testthat::expect_error(
    build_opendap_constraint(variables = "T2M", lon_idx = c(200L, 100L))
  )
})

################################################################################
# build_opendap_url
################################################################################

testthat::test_that("build_opendap_url: no constraint returns plain URL", {
  url <- build_opendap_url(
    base     = "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2T1NXSLV.5.12.4/2024/01/",
    filename = "MERRA2_400.tavg1_2d_slv_Nx.20240101.nc4"
  )
  testthat::expect_equal(
    url,
    "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2T1NXSLV.5.12.4/2024/01/MERRA2_400.tavg1_2d_slv_Nx.20240101.nc4"
  )
})

testthat::test_that("build_opendap_url: empty string constraint returns plain URL", {
  url <- build_opendap_url(
    base       = "https://example.com/opendap/",
    filename   = "data.nc4",
    constraint = ""
  )
  testthat::expect_equal(url, "https://example.com/opendap/data.nc4")
})

testthat::test_that("build_opendap_url: with constraint appends ? correctly", {
  url <- build_opendap_url(
    base       = "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2T1NXSLV.5.12.4/2024/01/",
    filename   = "MERRA2_400.tavg1_2d_slv_Nx.20240101.nc4.nc4",
    constraint = "T2M[0:23][64:120][88:185]"
  )
  testthat::expect_true(grepl("\\?T2M\\[0:23\\]", url))
  testthat::expect_true(grepl("^https://", url))
})

testthat::test_that("build_opendap_url: NULL constraint returns plain URL", {
  url <- build_opendap_url(
    base       = "https://example.com/opendap/",
    filename   = "data.nc4",
    constraint = NULL
  )
  testthat::expect_equal(url, "https://example.com/opendap/data.nc4")
})

testthat::test_that("build_opendap_url: input validation", {
  testthat::expect_error(build_opendap_url(base = 123, filename = "a.nc4"))
  testthat::expect_error(build_opendap_url(base = "https://x.com/", filename = 123))
  testthat::expect_error(
    build_opendap_url(
      base     = c("https://a.com/", "https://b.com/"),
      filename = "a.nc4"
    )
  )
})

################################################################################
# Composing auxiliary functions (integration of pure helpers)
################################################################################

testthat::test_that("MERRA2 extent + constraint + URL pipeline produces valid URL", {
  idx <- extent_to_merra2_indices(c(-80, 35, -75, 40))
  n_times <- merra2_collection_ntimes("tavg1_2d_slv_Nx")
  constraint <- build_opendap_constraint(
    variables = c("T2M", "U10M"),
    time_idx  = c(0L, n_times - 1L),
    lat_idx   = idx$lat,
    lon_idx   = idx$lon
  )
  url <- build_opendap_url(
    base       = "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2T1NXSLV.5.12.4/2024/01/",
    filename   = "MERRA2_400.tavg1_2d_slv_Nx.20240101.nc4.nc4",
    constraint = constraint
  )
  testthat::expect_true(grepl("^https://goldsmr4", url))
  testthat::expect_true(grepl("\\?T2M", url))
  testthat::expect_true(grepl("U10M", url))
  testthat::expect_true(grepl("\\[0:23\\]", url))
  testthat::expect_true(grepl("\\[", url))
})

testthat::test_that("GEOS-CF extent + constraint + URL pipeline produces valid URL", {
  idx <- extent_to_geos_indices(c(-80, 35, -75, 40))
  constraint <- build_opendap_constraint(
    variables = "CO",
    time_idx  = c(0L, 0L),
    lat_idx   = idx$lat,
    lon_idx   = idx$lon
  )
  url <- build_opendap_url(
    base       = "https://portal.nccs.nasa.gov/datashare/gmao/geos-cf/v1/ana/Y2024/M01/D01/",
    filename   = "GEOS-CF.v01.rpl.aqc_tavg_1hr_g1440x721_v1.20240101_00z.nc4",
    constraint = constraint
  )
  testthat::expect_true(grepl("\\?CO", url))
  testthat::expect_true(grepl("\\[0:0\\]", url))
  testthat::expect_true(grepl("\\[", url))
})

################################################################################
# OPeNDAP code paths in download_merra2 (mocked)
################################################################################

testthat::test_that("download_merra2 use_opendap=FALSE is backward compatible (mocked)", {
  # When use_opendap=FALSE, behavior must be identical to old code path
  testthat::local_mocked_bindings(
    download_permit         = function(...) invisible(NULL),
    download_setup_dir      = function(...) invisible(NULL),
    download_sanitize_path  = function(x) paste0(x, "/"),
    get_token               = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    check_url_status        = function(...) TRUE,
    check_destfile          = function(...) TRUE,
    download_run_method     = function(urls, destfiles, ...) {
      list(success = length(urls), failed = 0L, skipped = 0L)
    },
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    request      = function(url) structure(list(url = url), class = "httr2_request"),
    req_perform  = function(req) structure(list(body = charToRaw(
      '<a href="MERRA2_400.tavg1_2d_slv_Nx.20180101.nc4"></a>'
    )), class = "httr2_response"),
    resp_body_string = function(resp) rawToChar(resp$body),
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- download_merra2(
      collection      = "tavg1_2d_slv_Nx",
      date            = "2018-01-01",
      directory_to_save = ".",
      acknowledgement = TRUE,
      use_opendap     = FALSE
    )
    testthat::expect_type(result, "list")
  })
})

testthat::test_that("download_merra2 use_opendap=TRUE builds OPeNDAP URLs (mocked)", {
  captured_urls <- character(0)

  testthat::local_mocked_bindings(
    download_permit           = function(...) invisible(NULL),
    download_setup_dir        = function(...) invisible(NULL),
    download_sanitize_path    = function(x) paste0(x, "/"),
    get_token                 = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    check_url_status          = function(...) TRUE,
    check_destfile            = function(...) TRUE,
    download_run_method       = function(urls, destfiles, ...) {
      captured_urls <<- urls
      list(success = length(urls), failed = 0L, skipped = 0L)
    },
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    request      = function(url) structure(list(url = url), class = "httr2_request"),
    req_perform  = function(req) structure(list(body = charToRaw(
      '<a href="MERRA2_400.tavg1_2d_slv_Nx.20180101.nc4"></a>'
    )), class = "httr2_response"),
    resp_body_string = function(resp) rawToChar(resp$body),
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- download_merra2(
      collection        = "tavg1_2d_slv_Nx",
      date              = "2018-01-01",
      extent            = c(-80, 35, -75, 40),
      directory_to_save = ".",
      acknowledgement   = TRUE,
      use_opendap       = TRUE,
      variables         = c("T2M", "U10M")
    )
    testthat::expect_type(result, "list")
    if (length(captured_urls) > 0) {
      testthat::expect_true(all(grepl("opendap", captured_urls)))
      testthat::expect_true(all(grepl("\\?T2M", captured_urls)))
      # time dimension [0:23] must appear before lat/lon brackets
      testthat::expect_true(all(grepl("\\[0:23\\]", captured_urls)))
    }
  })
})

testthat::test_that("download_merra2 use_opendap=TRUE download=FALSE returns discovered list", {
  testthat::local_mocked_bindings(
    download_permit = function(...) invisible(NULL),
    download_setup_dir = function(...) invisible(NULL),
    download_sanitize_path = function(x) paste0(x, "/"),
    get_token = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    check_url_status = function(...) TRUE,
    check_destfile = function(...) TRUE,
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    request = function(url) structure(list(url = url), class = "httr2_request"),
    req_perform = function(req) structure(list(body = charToRaw(
      paste0(
        '<a href="MERRA2_400.tavg1_2d_slv_Nx.20180101.nc4"></a>',
        '<a href="MERRA2_400.tavg1_2d_slv_Nx.20180102.nc4"></a>'
      )
    )), class = "httr2_response"),
    resp_body_string = function(resp) rawToChar(resp$body),
    .package = "httr2"
  )

  withr::with_tempdir({
    result <- suppressWarnings(download_merra2(
      collection = "tavg1_2d_slv_Nx",
      date = c("2018-01-01", "2018-01-02"),
      extent = c(-80, 35, -75, 40),
      directory_to_save = ".",
      acknowledgement = TRUE,
      use_opendap = TRUE,
      variables = c("T2M"),
      download = FALSE
    ))
    testthat::expect_type(result, "list")
    testthat::expect_true(all(c("urls", "destfiles", "n_files") %in% names(result)))
    testthat::expect_true(result$n_files >= 0L)
    if (length(result$urls) > 0) {
      testthat::expect_true(all(grepl("opendap", result$urls)))
    }
  })
})

testthat::test_that("download_merra2 use_opendap=TRUE extent=NULL warns about no benefit (mocked)", {
  testthat::local_mocked_bindings(
    download_permit           = function(...) invisible(NULL),
    download_setup_dir        = function(...) invisible(NULL),
    download_sanitize_path    = function(x) paste0(x, "/"),
    get_token                 = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    check_url_status          = function(...) TRUE,
    check_destfile            = function(...) TRUE,
    download_run_method       = function(...) {
      list(success = 0L, failed = 0L, skipped = 0L)
    },
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    request      = function(url) structure(list(url = url), class = "httr2_request"),
    req_perform  = function(req) structure(list(body = charToRaw(
      '<a href="MERRA2_400.tavg1_2d_slv_Nx.20180101.nc4"></a>'
    )), class = "httr2_response"),
    resp_body_string = function(resp) rawToChar(resp$body),
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_message(
      download_merra2(
        collection        = "tavg1_2d_slv_Nx",
        date              = "2018-01-01",
        extent            = NULL,
        directory_to_save = ".",
        acknowledgement   = TRUE,
        use_opendap       = TRUE
      ),
      "no subsetting benefit"
    )
  })
})

testthat::test_that("download_merra2 use_opendap=TRUE with invalid extent errors", {
  testthat::local_mocked_bindings(
    download_permit           = function(...) invisible(NULL),
    download_setup_dir        = function(...) invisible(NULL),
    download_sanitize_path    = function(x) paste0(x, "/"),
    get_token                 = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_error(
      download_merra2(
        collection        = "tavg1_2d_slv_Nx",
        date              = "2018-01-01",
        extent            = c(-80, 35, -75),
        directory_to_save = ".",
        acknowledgement   = TRUE,
        use_opendap       = TRUE
      )
    )
  })
})

################################################################################
# OPeNDAP code paths in download_geos (mocked)
################################################################################

testthat::test_that("download_geos use_opendap=FALSE is backward compatible (mocked)", {
  testthat::local_mocked_bindings(
    download_permit           = function(...) invisible(NULL),
    download_setup_dir        = function(...) invisible(NULL),
    download_sanitize_path    = function(x) paste0(x, "/"),
    get_token                 = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    check_url_status          = function(...) TRUE,
    check_destfile            = function(...) TRUE,
    generate_date_sequence    = function(...) "20190909",
    generate_time_sequence    = function(...) "0000",
    download_run_method       = function(...) {
      list(success = 1L, failed = 0L, skipped = 0L)
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- download_geos(
      collection        = "aqc_tavg_1hr_g1440x721_v1",
      date              = "2019-09-09",
      directory_to_save = ".",
      acknowledgement   = TRUE,
      use_opendap       = FALSE
    )
    testthat::expect_type(result, "list")
  })
})

testthat::test_that("download_geos use_opendap=TRUE builds constraint URLs (mocked)", {
  captured_urls <- character(0)

  testthat::local_mocked_bindings(
    download_permit           = function(...) invisible(NULL),
    download_setup_dir        = function(...) invisible(NULL),
    download_sanitize_path    = function(x) paste0(x, "/"),
    get_token                 = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    check_url_status          = function(...) TRUE,
    check_destfile            = function(...) TRUE,
    generate_date_sequence    = function(...) "20190909",
    generate_time_sequence    = function(...) "0000",
    download_run_method       = function(urls, destfiles, ...) {
      captured_urls <<- urls
      list(success = length(urls), failed = 0L, skipped = 0L)
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- download_geos(
      collection        = "aqc_tavg_1hr_g1440x721_v1",
      date              = "2019-09-09",
      extent            = c(-80, 35, -75, 40),
      directory_to_save = ".",
      acknowledgement   = TRUE,
      use_opendap       = TRUE,
      variables         = c("CO", "NO2")
    )
    testthat::expect_type(result, "list")
    if (length(captured_urls) > 0) {
      testthat::expect_true(all(grepl("\\?CO", captured_urls)))
    }
  })
})

testthat::test_that("download_geos use_opendap=TRUE extent=NULL warns about no benefit (mocked)", {
  testthat::local_mocked_bindings(
    download_permit           = function(...) invisible(NULL),
    download_setup_dir        = function(...) invisible(NULL),
    download_sanitize_path    = function(x) paste0(x, "/"),
    get_token                 = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    check_url_status          = function(...) TRUE,
    check_destfile            = function(...) TRUE,
    generate_date_sequence    = function(...) "20190909",
    generate_time_sequence    = function(...) "0000",
    download_run_method       = function(...) {
      list(success = 0L, failed = 0L, skipped = 0L)
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_message(
      download_geos(
        collection        = "aqc_tavg_1hr_g1440x721_v1",
        date              = "2019-09-09",
        directory_to_save = ".",
        acknowledgement   = TRUE,
        use_opendap       = TRUE
      ),
      "no subsetting benefit"
    )
  })
})

################################################################################
# OPeNDAP code paths in download_modis (mocked)
################################################################################

testthat::test_that("download_modis use_opendap=FALSE is backward compatible (mocked)", {
  granule_url <- paste0(
    "https://e4ftl01.cr.usgs.gov/MOLT/MOD09GA.061/2024.01.01/",
    "MOD09GA.A2024001.h08v04.061.2024003055504.hdf"
  )
  testthat::local_mocked_bindings(
    download_permit           = function(...) invisible(NULL),
    download_setup_dir        = function(...) invisible(NULL),
    download_sanitize_path    = function(x) paste0(x, "/"),
    get_token                 = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    check_destfile            = function(...) TRUE,
    download_run_method       = function(...) {
      list(success = 1L, failed = 0L, skipped = 0L)
    },
    .package = "amadeus"
  )
  fake_resp <- list(
    feed = list(
      entry = list(
        list(links = list(
          list(
            rel  = "http://esipfed.org/ns/fedsearch/1.1/data#",
            href = granule_url
          )
        ))
      )
    )
  )
  testthat::local_mocked_bindings(
    request      = function(url) structure(list(url = url), class = "httr2_request"),
    req_url_query   = function(req, ...) req,
    req_options     = function(req, ...) req,
    req_retry       = function(req, ...) req,
    req_timeout     = function(req, ...) req,
    req_perform     = function(req) structure(list(), class = "httr2_response"),
    resp_body_json  = function(resp) fake_resp,
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- download_modis(
      product           = "MOD09GA",
      date              = "2024-01-01",
      extent            = c(-125, 22, -64, 50),
      directory_to_save = ".",
      acknowledgement   = TRUE,
      use_opendap       = FALSE
    )
    testthat::expect_type(result, "list")
  })
})

testthat::test_that("download_modis use_opendap=TRUE converts to LP DAAC OPeNDAP URLs (mocked)", {
  captured_urls <- character(0)
  granule_url <- paste0(
    "https://e4ftl01.cr.usgs.gov/MOLT/MOD09GA.061/2024.01.01/",
    "MOD09GA.A2024001.h08v04.061.2024003055504.hdf"
  )
  testthat::local_mocked_bindings(
    download_permit           = function(...) invisible(NULL),
    download_setup_dir        = function(...) invisible(NULL),
    download_sanitize_path    = function(x) paste0(x, "/"),
    get_token                 = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    check_destfile            = function(...) TRUE,
    download_run_method       = function(urls, destfiles, ...) {
      captured_urls <<- urls
      list(success = length(urls), failed = 0L, skipped = 0L)
    },
    .package = "amadeus"
  )
  fake_resp <- list(
    feed = list(
      entry = list(
        list(links = list(
          list(
            rel  = "http://esipfed.org/ns/fedsearch/1.1/data#",
            href = granule_url
          )
        ))
      )
    )
  )
  testthat::local_mocked_bindings(
    request      = function(url) structure(list(url = url), class = "httr2_request"),
    req_url_query   = function(req, ...) req,
    req_options     = function(req, ...) req,
    req_retry       = function(req, ...) req,
    req_timeout     = function(req, ...) req,
    req_perform     = function(req) structure(list(), class = "httr2_response"),
    resp_body_json  = function(resp) fake_resp,
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- download_modis(
      product           = "MOD09GA",
      date              = "2024-01-01",
      extent            = c(-125, 22, -64, 50),
      directory_to_save = ".",
      acknowledgement   = TRUE,
      use_opendap       = TRUE,
      variables         = c("sur_refl_b01_1", "sur_refl_b02_1")
    )
    testthat::expect_type(result, "list")
    if (length(captured_urls) > 0) {
      testthat::expect_true(all(grepl("opendap.earthdata.nasa.gov", captured_urls)))
      testthat::expect_true(all(grepl("LPDAAC_ECS", captured_urls)))
      testthat::expect_true(all(grepl("sur_refl_b01_1", captured_urls)))
    }
  })
})

testthat::test_that("download_modis use_opendap=TRUE handles VNP46A2 with NULL version", {
  captured_urls <- character(0)
  granule_url <- paste0(
    "https://example.com/",
    "VNP46A2.A2023001.h08v05.001.hdf"
  )
  testthat::local_mocked_bindings(
    download_permit           = function(...) invisible(NULL),
    download_setup_dir        = function(...) invisible(NULL),
    download_sanitize_path    = function(x) paste0(x, "/"),
    get_token                 = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    check_destfile            = function(...) TRUE,
    download_run_method       = function(urls, destfiles, ...) {
      captured_urls <<- urls
      list(success = length(urls), failed = 0L, skipped = 0L)
    },
    .package = "amadeus"
  )
  fake_resp <- list(
    feed = list(
      entry = list(
        list(links = list(
          list(
            rel  = "http://esipfed.org/ns/fedsearch/1.1/data#",
            href = granule_url
          )
        ))
      )
    )
  )
  testthat::local_mocked_bindings(
    request        = function(url) structure(list(url = url), class = "httr2_request"),
    req_url_query  = function(req, ...) req,
    req_options    = function(req, ...) req,
    req_retry      = function(req, ...) req,
    req_timeout    = function(req, ...) req,
    req_perform    = function(req) structure(list(), class = "httr2_response"),
    resp_body_json = function(resp) fake_resp,
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- download_modis(
      product           = "VNP46A2",
      date              = "2023-01-01",
      extent            = c(-125, 22, -64, 50),
      directory_to_save = ".",
      acknowledgement   = TRUE,
      use_opendap       = TRUE,
      variables         = c("DNB_BRDF-Corrected_NTL")
    )
    testthat::expect_type(result, "list")
    testthat::expect_length(captured_urls, 1L)
    testthat::expect_match(
      captured_urls,
      "opendap\\.earthdata\\.nasa\\.gov/providers/LPDAAC_ECS/collections/VNP46A2_V/granules/"
    )
    testthat::expect_match(captured_urls, "DNB_BRDF-Corrected_NTL")
  })
})

testthat::test_that("download_modis use_opendap=TRUE without variables warns (mocked)", {
  granule_url <- paste0(
    "https://e4ftl01.cr.usgs.gov/MOLT/MOD09GA.061/2024.01.01/",
    "MOD09GA.A2024001.h08v04.061.2024003055504.hdf"
  )
  testthat::local_mocked_bindings(
    download_permit           = function(...) invisible(NULL),
    download_setup_dir        = function(...) invisible(NULL),
    download_sanitize_path    = function(x) paste0(x, "/"),
    get_token                 = function(...) "fake_token",
    check_for_null_parameters = function(...) invisible(NULL),
    check_destfile            = function(...) TRUE,
    download_run_method       = function(...) {
      list(success = 1L, failed = 0L, skipped = 0L)
    },
    .package = "amadeus"
  )
  fake_resp <- list(
    feed = list(
      entry = list(
        list(links = list(
          list(
            rel  = "http://esipfed.org/ns/fedsearch/1.1/data#",
            href = granule_url
          )
        ))
      )
    )
  )
  testthat::local_mocked_bindings(
    request      = function(url) structure(list(url = url), class = "httr2_request"),
    req_url_query   = function(req, ...) req,
    req_options     = function(req, ...) req,
    req_retry       = function(req, ...) req,
    req_timeout     = function(req, ...) req,
    req_perform     = function(req) structure(list(), class = "httr2_response"),
    resp_body_json  = function(resp) fake_resp,
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_message(
      download_modis(
        product           = "MOD09GA",
        date              = "2024-01-01",
        extent            = c(-125, 22, -64, 50),
        directory_to_save = ".",
        acknowledgement   = TRUE,
        use_opendap       = TRUE,
        variables         = NULL
      ),
      "variables"
    )
  })
})

################################################################################
# Live integration tests (skipped on CI, require NASA_EARTHDATA_TOKEN)
################################################################################

testthat::test_that("download_merra2 OPeNDAP live download (small extent, 1 day)", {
  skip_if_not_live_opendap()
  withr::with_tempdir({
    result <- download_merra2(
      collection        = "tavg1_2d_slv_Nx",
      date              = "2020-06-15",
      extent            = c(-80, 35, -75, 40),
      directory_to_save = ".",
      acknowledgement   = TRUE,
      use_opendap       = TRUE,
      variables         = c("T2M", "U10M", "V10M")
    )
    testthat::expect_type(result, "list")
    testthat::expect_true(
      all(c("success", "failed", "skipped") %in% names(result))
    )
    nc4_files <- list.files(".", pattern = "\\.nc4$", recursive = TRUE)
    testthat::expect_gt(length(nc4_files), 0)
    # Verify file size is smaller than full global file (~180MB)
    # A subset for ~5x5 degrees should be well under 5MB
    if (length(nc4_files) > 0) {
      file_size_mb <- file.info(nc4_files[1])$size / 1e6
      testthat::expect_lt(file_size_mb, 50)
    }
  })
})

testthat::test_that("download_merra2 OPeNDAP live download works for two different areas", {
  skip_if_not_live_opendap()
  extents <- list(
    c(-80, 35, -75, 40), # Eastern US
    c(5, 45, 15, 55) # Europe
  )

  for (i in seq_along(extents)) {
    withr::with_tempdir({
      result <- download_merra2(
        collection        = "tavg1_2d_slv_Nx",
        date              = "2020-06-15",
        extent            = extents[[i]],
        directory_to_save = ".",
        acknowledgement   = TRUE,
        use_opendap       = TRUE,
        variables         = c("T2M")
      )
      testthat::expect_type(result, "list")
      testthat::expect_true(result$success >= 1L)
      nc4_files <- list.files(".", pattern = "\\.nc4$", recursive = TRUE)
      testthat::expect_gt(length(nc4_files), 0)
    })
  }
})

testthat::test_that("download_merra2 OPeNDAP live download supports extent=NULL", {
  skip_if_not_live_opendap()
  withr::with_tempdir({
    result <- download_merra2(
      collection        = "tavg1_2d_slv_Nx",
      date              = "2020-06-15",
      extent            = NULL,
      directory_to_save = ".",
      acknowledgement   = TRUE,
      use_opendap       = TRUE,
      variables         = c("T2M")
    )
    testthat::expect_type(result, "list")
    testthat::expect_true(result$success >= 1L)
    nc4_files <- list.files(".", pattern = "\\.nc4$", recursive = TRUE)
    testthat::expect_gt(length(nc4_files), 0)
  })
})

testthat::test_that("download_geos OPeNDAP live download (small extent, 1 date)", {
  skip_if_not_live_opendap()
  withr::with_tempdir({
    result <- download_geos(
      collection        = "aqc_tavg_1hr_g1440x721_v1",
      date              = "2019-09-09",
      extent            = c(-80, 35, -75, 40),
      directory_to_save = ".",
      acknowledgement   = TRUE,
      use_opendap       = TRUE,
      variables         = c("CO", "NO2")
    )
    testthat::expect_type(result, "list")
    testthat::expect_true(
      all(c("success", "failed", "skipped") %in% names(result))
    )
    nc4_files <- list.files(".", pattern = "\\.nc4$", recursive = TRUE)
    testthat::expect_gt(length(nc4_files), 0)
  })
})

testthat::test_that("download_geos OPeNDAP live download works for two different areas", {
  skip_if_not_live_opendap()
  extents <- list(
    c(-80, 35, -75, 40), # Eastern US
    c(120, -10, 130, 0) # Maritime Southeast Asia
  )

  for (i in seq_along(extents)) {
    withr::with_tempdir({
      result <- download_geos(
        collection        = "aqc_tavg_1hr_g1440x721_v1",
        date              = "2019-09-09",
        extent            = extents[[i]],
        directory_to_save = ".",
        acknowledgement   = TRUE,
        use_opendap       = TRUE,
        variables         = c("CO")
      )
      testthat::expect_type(result, "list")
      testthat::expect_true(result$success >= 1L)
      nc4_files <- list.files(".", pattern = "\\.nc4$", recursive = TRUE)
      testthat::expect_gt(length(nc4_files), 0)
    })
  }
})

testthat::test_that("download_geos OPeNDAP live download supports extent=NULL", {
  skip_if_not_live_opendap()
  withr::with_tempdir({
    result <- download_geos(
      collection        = "aqc_tavg_1hr_g1440x721_v1",
      date              = "2019-09-09",
      extent            = NULL,
      directory_to_save = ".",
      acknowledgement   = TRUE,
      use_opendap       = TRUE,
      variables         = c("CO")
    )
    testthat::expect_type(result, "list")
    testthat::expect_true(result$success >= 1L)
    nc4_files <- list.files(".", pattern = "\\.nc4$", recursive = TRUE)
    testthat::expect_gt(length(nc4_files), 0)
  })
})

testthat::test_that("download_modis OPeNDAP live download (small extent, 1 day)", {
  skip_if_not_live_opendap()
  withr::with_tempdir({
    result <- download_modis(
      product           = "MOD09GA",
      version           = "061",
      date              = "2020-06-15",
      extent            = c(-80, 35, -75, 40),
      directory_to_save = ".",
      acknowledgement   = TRUE,
      use_opendap       = TRUE,
      variables         = c("sur_refl_b01_1", "sur_refl_b02_1")
    )
    testthat::expect_type(result, "list")
    testthat::expect_true(
      all(c("success", "failed", "skipped") %in% names(result))
    )
  })
})

# nolint end
