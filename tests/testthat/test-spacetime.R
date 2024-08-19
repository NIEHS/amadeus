################################################################################
##### unit and integration tests for spacetime manipulation functions

################################################################################
##### check_mysftime
testthat::test_that("check_mysftime works as expected", {
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_package("sftime")
  withr::local_options(list(sf_use_s2 = FALSE))

  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  mysft <- sftime::st_as_sftime(stdata,
    coords = c("lon", "lat"),
    crs = 4326,
    time_column_name = "time"
  )

  # should work
  testthat::expect_no_error(check_mysftime(x = mysft))

  # check that error messages work well
  testthat::expect_error(check_mysftime(stdata), "x is not a sftime")
  mysft <- sftime::st_as_sftime(as.data.frame(stdata),
    coords = c("lon", "lat"),
    crs = 4326,
    time_column_name = "time"
  )
  testthat::expect_error(
    check_mysftime(x = mysft),
    "x is not inherited from a data.table"
  )
  mysft <- stdata |>
    dplyr::rename("date" = time) |>
    sftime::st_as_sftime(
      coords = c("lon", "lat"),
      crs = 4326,
      time_column_name = "date"
    )
  testthat::expect_error(
    check_mysftime(mysft), "time column should be called time"
  )
  mysft <- stdata |>
    sftime::st_as_sftime(
      coords = c("lon", "lat"),
      crs = 4326,
      time_column_name = "time"
    ) |>
    dplyr::rename("geom" = "geometry")
  testthat::expect_error(
    check_mysftime(mysft),
    "geometry column should be called geometry"
  )
  mysft <- sftime::st_as_sftime(stdata,
    coords = c("lon", "lat"),
    crs = 4326,
    time_column_name = "time"
  )
  pol <- cbind(
    c(39.35, 39.36, 39.36, 39.35, 39.35),
    c(-81.43, -81.43, -81.42, -81.42, -81.43)
  ) |>
    list() |>
    st_polygon()
  for (i in 1:27) {
    mysft$geometry[i] <- pol
  }
  testthat::expect_error(
    check_mysftime(mysft),
    "geometry is not a sfc_POINT"
  )
})

################################################################################
##### check_mysf
testthat::test_that("check_mysf", {
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  mysf <- sf::st_as_sf(stdata,
    coords = c("lon", "lat"),
    crs = 4326
  )

  # should work
  testthat::expect_no_error(check_mysf(x = mysf))

  # check that error messages work well
  testthat::expect_error(check_mysf(stdata), "x is not a sf")
  mysf <- sf::st_as_sf(as.data.frame(stdata),
    coords = c("lon", "lat"),
    crs = 4326
  )
  testthat::expect_error(
    check_mysf(x = mysf),
    "x is not inherited from a data.table"
  )
  mysf <- stdata |>
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326
    ) |>
    dplyr::rename("geom" = "geometry")
  testthat::expect_error(
    check_mysf(mysf),
    "geometry column should be called geometry"
  )
  mysf <- sf::st_as_sf(stdata,
    coords = c("lon", "lat"),
    crs = 4326
  )
  pol <- cbind(
    c(39.35, 39.36, 39.36, 39.35, 39.35),
    c(-81.43, -81.43, -81.42, -81.42, -81.43)
  ) |>
    list() |>
    st_polygon()
  for (i in 1:27) {
    mysf$geometry[i] <- pol
  }
  testthat::expect_error(
    check_mysf(mysf),
    "geometry is not a sfc_POINT"
  )
})

################################################################################
##### rename_time
testthat::test_that("rename_time", {
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_package("sftime")
  withr::local_options(list(sf_use_s2 = FALSE))
  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  mysft <- sftime::st_as_sftime(stdata,
    coords = c("lon", "lat"),
    crs = 4326,
    time_column_name = "time"
  )
  testthat::expect_no_error(rename_time(mysft, "date"))
  testthat::expect_equal(
    attributes(rename_time(mysft, "date"))$time_column,
    "date"
  )
  testthat::expect_error(
    rename_time(stdata, "date"),
    "x is not a sftime"
  )
})

################################################################################
##### dt_as_mysftime
testthat::test_that("dt_as_mysftime", {
  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  # should work
  testthat::expect_no_error(dt_as_mysftime(
    x = stdata,
    lonname = "lon",
    latname = "lat",
    timename = "time",
    crs = 4326
  ))
  testthat::expect_no_error(check_mysftime(dt_as_mysftime(
    x = stdata,
    lonname = "lon",
    latname = "lat",
    timename = "time",
    crs = 4326
  )))
  testthat::expect_error(
    dt_as_mysftime(
      x = stdata,
      lonname = "longitude",
      latname = "lat",
      timename = "time",
      crs = 4326
    ),
    "Some of lon, lat, time columns missing or mispelled"
  )
  expect_error(
    dt_as_mysftime(
      x = stdata[, lat := NULL],
      lonname = "lon",
      latname = "lat",
      timename = "time",
      crs = 4326
    ),
    "Some of lon, lat, time columns missing or mispelled"
  )
})

################################################################################
##### as_mysftime
testthat::test_that("as_mysftime", {
  withr::local_package("terra")
  withr::local_package("data.table")
  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  # with data.table
  testthat::expect_no_error(as_mysftime(
    x = stdata,
    lonname = "lon",
    latname = "lat",
    timename = "time",
    crs = 4326
  ))
  testthat::expect_no_error(check_mysftime(as_mysftime(
    x = stdata,
    lonname = "lon",
    latname = "lat",
    timename = "time",
    crs = 4326
  )))
  testthat::expect_error(
    as_mysftime(x = stdata),
    "argument \"lonname\" is missing, with no default"
  )
  # with data.frame
  testthat::expect_no_error(as_mysftime(
    x = as.data.frame(stdata),
    lonname = "lon",
    latname = "lat",
    timename = "time",
    crs = 4326
  ))
  testthat::expect_no_error(check_mysftime(as_mysftime(
    x = as.data.frame(stdata),
    lonname = "lon",
    latname = "lat",
    timename = "time",
    crs = 4326
  )))
  # with sf
  mysf <- sf::st_as_sf(stdata,
    coords = c("lon", "lat"),
    crs = 4326
  )
  testthat::expect_no_error(as_mysftime(mysf, "time"))
  b <- mysf |>
    dplyr::rename("date" = "time")
  testthat::expect_no_error(as_mysftime(b, "date"))
  testthat::expect_no_error(check_mysftime(as_mysftime(b, "date")))
  # with sftime
  mysft <- sftime::st_as_sftime(stdata,
    coords = c("lon", "lat"),
    crs = 4326,
    time_column_name = "time"
  )
  testthat::expect_no_error(as_mysftime(mysft, "time"))
  testthat::expect_no_error(check_mysftime(as_mysftime(mysft, "time")))
  # with SpatRaster
  myrast <-
    terra::rast(
      extent = c(-112, -101, 33.5, 40.9),
      ncol = 5,
      nrow = 5,
      crs = "EPSG:4326"
    )
  terra::values(myrast) <- seq(-5, 19)
  terra::add(myrast) <- c(myrast**2, myrast**3)
  names(myrast) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  testthat::expect_no_error(as_mysftime(x = myrast, varname = "altitude"))
  testthat::expect_no_error(
    check_mysftime(as_mysftime(x = myrast, varname = "altitude"))
  )
  # with SpatVector
  myvect <- terra::vect(
    stdata,
    geom = c("lon", "lat"),
    crs = "EPSG:4326",
    keepgeom = FALSE
  )
  testthat::expect_no_error(as_mysftime(x = myvect))
  testthat::expect_no_error(check_mysftime(as_mysftime(x = myvect)))
  myvect <- stdata |>
    dplyr::rename("time2" = time) |>
    terra::vect(
      geom = c("lon", "lat"),
      crs = "EPSG:4326",
      keepgeom = FALSE
    )
  testthat::expect_error(
    as_mysftime(x = myvect),
    "timename column missing or mispelled"
  )
  # with SpatRasterDataset created from 2 SpatRast (i.e. 2 variables)
  # with 3 layers (i.e. 3 timestamps)
  var1 <- terra::rast(
    extent = c(-112, -101, 33.5, 40.9),
    ncol = 5,
    nrow = 5,
    crs = "EPSG:4326"
  )
  terra::values(var1) <- seq(-5, 19)
  terra::add(var1) <- c(var1**2, var1**3)
  var1 <- rast(
    extent = c(-112, -101, 33.5, 40.9),
    ncol = 5,
    nrow = 5,
    crs = "EPSG:4326"
  )
  terra::values(var1) <- seq(-5, 19)
  add(var1) <- c(var1**2, var1**3)
  names(var1) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  var2 <- terra::rast(
    extent = c(-112, -101, 33.5, 40.9),
    ncol = 5,
    nrow = 5,
    crs = "EPSG:4326"
  )
  terra::values(var2) <- seq(-15, 9)
  add(var2) <- c(var2**2, var2**3)
  names(var2) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  myrds <- terra::sds(var1, var2)
  names(myrds) <- c("var1", "var2")
  testthat::expect_no_error(as_mysftime(myrds))
  testthat::expect_error(
    as_mysftime(x = "roquefort"),
    "x class not accepted"
  )
})

################################################################################
##### sftime_as_spatvector
testthat::test_that("sftime_as_spatvector", {
  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  mysftime <- sftime::st_as_sftime(stdata,
    coords = c("lon", "lat"),
    time_column_name = "time",
    crs = 4326
  )
  testthat::expect_no_error(sftime_as_spatvector(mysftime))
  # with a different time column name:
  attributes(mysftime)$time_column <- "date"
  mysftime <- dplyr::rename(mysftime, "date" = "time")
  testthat::expect_no_error(sftime_as_spatvector(mysftime))
  # doesn't work with other classes:
  testthat::expect_error(sftime_as_spatvector(stdata))
})

################################################################################
##### sf_as_mysftime
testthat::test_that("sf_as_mysftime", {
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_package("sftime")
  withr::local_options(list(sf_use_s2 = FALSE))

  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  mysf <- sf::st_as_sf(stdata, coords = c("lon", "lat"), crs = 4326)
  testthat::expect_no_error(sf_as_mysftime(mysf, "time"))
  testthat::expect_no_error(check_mysftime(sf_as_mysftime(mysf, "time")))
  b <- mysf |>
    dplyr::rename("date" = "time")
  testthat::expect_no_error(check_mysftime(sf_as_mysftime(b, "date")))
  testthat::expect_error(
    sf_as_mysftime(b, "time"),
    "time column missing or mispelled"
  )
})

################################################################################
##### sftime_as_mysftime
testthat::test_that("sftime_as_mysftime", {
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_package("sftime")
  withr::local_options(list(sf_use_s2 = FALSE))

  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  mysft <- sftime::st_as_sftime(stdata,
    coords = c("lon", "lat"),
    time_column_name = "time",
    crs = 4326
  )
  testthat::expect_no_error(sftime_as_mysftime(mysft, "time"))
  testthat::expect_no_error(
    check_mysftime(sftime_as_mysftime(mysft, "time"))
  )
  testthat::expect_error(sftime_as_mysftime(mysft, "date"))
  attributes(mysft)$time_column <- "date"
  mysft <- dplyr::rename(mysft, "date" = "time")
  testthat::expect_no_error(check_mysftime(sf_as_mysftime(mysft, "date")))
  testthat::expect_error(
    sf_as_mysftime(mysft, "time"),
    "time column missing or mispelled"
  )
})

################################################################################
##### spatraster_as_sftime
testthat::test_that("spatraster_as_sftime", {
  withr::local_package("terra")
  withr::local_package("sftime")
  withr::local_options(list(sf_use_s2 = FALSE))
  myrast <-
    terra::rast(
      extent = c(-112, -101, 33.5, 40.9),
      ncol = 5,
      nrow = 5,
      crs = "EPSG:4326"
    )
  terra::values(myrast) <- seq(-5, 19)
  terra::add(myrast) <- c(myrast**2, myrast**3)
  names(myrast) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  # conversion should work
  testthat::expect_no_error(spatraster_as_sftime(myrast, "myvar"))
  testthat::expect_no_error(spatraster_as_sftime(myrast, "myvar", "date"))
  mysft <- spatraster_as_sftime(myrast, "myvar", "date")
  testthat::expect_equal(attributes(mysft)$time, "date")
  # conversion does not work because raster's names are not dates
  names(myrast) <- c("roquefort", "comte", "camembert")
  testthat::expect_error(
    spatraster_as_sftime(myrast, "myvar"),
    "x layers might not be time"
  )
})

################################################################################
##### spatrds_as_sftime
testthat::test_that("spatrds_as_sftime", {
  withr::local_package("terra")
  withr::local_package("sftime")
  withr::local_options(list(sf_use_s2 = FALSE))
  var1 <-
    terra::rast(
      extent = c(-112, -101, 33.5, 40.9),
      ncol = 5,
      nrow = 5,
      crs = "EPSG:4326"
    )
  terra::values(var1) <- seq(-5, 19)
  terra::add(var1) <- c(var1**2, var1**3)
  var1 <- rast(
    extent = c(-112, -101, 33.5, 40.9),
    ncol = 5,
    nrow = 5,
    crs = "EPSG:4326"
  )
  terra::values(var1) <- seq(-5, 19)
  terra::add(var1) <- c(var1**2, var1**3)
  names(var1) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  var2 <- rast(
    extent = c(-112, -101, 33.5, 40.9),
    ncol = 5,
    nrow = 5,
    crs = "EPSG:4326"
  )
  terra::values(var2) <- seq(-15, 9)
  terra::add(var2) <- c(var2**2, var2**3)
  names(var2) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  myrds <- terra::sds(var1, var2)
  names(myrds) <- c("var1", "var2")
  # conversion should work
  testthat::expect_no_error(spatrds_as_sftime(myrds, "time"))
  mysft <- spatrds_as_sftime(myrds, "date")
  testthat::expect_equal(attributes(mysft)$time, "date")
})

################################################################################
##### sftime_as_sf
testthat::test_that("sftime_as_sf", {
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_package("sftime")
  withr::local_options(list(sf_use_s2 = FALSE))


  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  mysftime <- sftime::st_as_sftime(stdata,
    coords = c("lon", "lat"),
    time_column_name = "time",
    crs = 4326
  )
  testthat::expect_no_error(sftime_as_sf(mysftime))
  testthat::expect_no_error(sftime_as_sf(mysftime, keeptime = FALSE))
  testthat::expect_equal(class(sftime_as_sf(mysftime))[1], "sf")
  testthat::expect_equal(
    class(sftime_as_sf(mysftime, keeptime = FALSE))[1], "sf"
  )
  testthat::expect_true(
    "time" %in% colnames(sftime_as_sf(mysftime, keeptime = TRUE))
  )
  testthat::expect_false(
    "time" %in% colnames(sftime_as_sf(mysftime, keeptime = FALSE))
  )
})

################################################################################
##### sftime_as_sf
testthat::test_that("sftime_as_sf", {
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_package("sftime")
  withr::local_options(list(sf_use_s2 = FALSE))

  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  mysftime <- sftime::st_as_sftime(stdata,
    coords = c("lon", "lat"),
    time_column_name = "time",
    crs = 4326
  )
  testthat::expect_no_error(sftime_as_sf(mysftime))
  testthat::expect_no_error(sftime_as_sf(mysftime, keeptime = FALSE))
  testthat::expect_equal(class(sftime_as_sf(mysftime))[1], "sf")
  testthat::expect_equal(
    class(sftime_as_sf(mysftime, keeptime = FALSE))[1], "sf"
  )
  testthat::expect_true(
    "time" %in% colnames(sftime_as_sf(mysftime, keeptime = TRUE))
  )
  testthat::expect_false(
    "time" %in% colnames(sftime_as_sf(mysftime, keeptime = FALSE))
  )
})

################################################################################
##### sftime_as_spatraster
testthat::test_that("sftime_as_spatraster", {
  withr::local_package("terra")
  withr::local_package("sftime")
  withr::local_options(list(sf_use_s2 = FALSE))
  myrast <-
    terra::rast(
      extent = c(-112, -101, 33.5, 40.9),
      ncol = 5,
      nrow = 5,
      crs = "EPSG:4326"
    )
  terra::values(myrast) <- seq(-5, 19)
  terra::add(myrast) <- c(myrast**2, myrast**3)
  names(myrast) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  mysftime <- as_mysftime(myrast, varname = "roquefort")
  testthat::expect_no_error(sftime_as_spatraster(mysftime, "roquefort"))
  testthat::expect_error(
    sftime_as_spatraster(mysftime, "cheddar"),
    "varname missing or mispelled"
  )
})

################################################################################
##### sftime_as_spatrds
testthat::test_that("sftime_as_spatrds", {
  withr::local_package("terra")
  withr::local_package("sftime")
  withr::local_options(list(sf_use_s2 = FALSE))
  var1 <-
    terra::rast(
      extent = c(-112, -101, 33.5, 40.9),
      ncol = 5,
      nrow = 5,
      crs = "EPSG:4326"
    )
  terra::values(var1) <- seq(-5, 19)
  terra::add(var1) <- c(var1**2, var1**3)
  var1 <- rast(
    extent = c(-112, -101, 33.5, 40.9),
    ncol = 5,
    nrow = 5,
    crs = "EPSG:4326"
  )
  terra::values(var1) <- seq(-5, 19)
  terra::add(var1) <- c(var1**2, var1**3)
  names(var1) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  var2 <- rast(
    extent = c(-112, -101, 33.5, 40.9),
    ncol = 5,
    nrow = 5,
    crs = "EPSG:4326"
  )
  terra::values(var2) <- seq(-15, 9)
  terra::add(var2) <- c(var2**2, var2**3)
  names(var2) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  myrds <- terra::sds(var1, var2)
  names(myrds) <- c("var1", "var2")
  # create a structured sftime
  mysft <- spatrds_as_sftime(myrds, "time")
  # conversion should work
  testthat::expect_no_error(sftime_as_spatrds(mysft))
  testthat::expect_error(sftime_as_spatrds("hello"), "x is not a sftime")
  rename_time(mysft, "date")
  testthat::expect_no_error(sftime_as_spatrds(mysft))
})

testthat::test_that("as_mysftime with time-varying SpatRasterDataset.", {
  m1 <- terra::rast(matrix(1:100, nrow = 10))
  terra::time(m1) <- as.Date("2024-01-01")
  m2 <- terra::rast(matrix(1:100, nrow = 10))
  terra::time(m2) <- as.Date("2024-01-02")
  m3 <- terra::sds(c(m1, m2))

  testthat::expect_error(
    as_mysftime(m3)
  )
})
