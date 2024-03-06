
test_that("check_mysftime works as expected", {
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
  expect_no_error(check_mysftime(x = mysft))

  # check that error messages work well
  expect_error(check_mysftime(stdata), "x is not a sftime")
  mysft <- sftime::st_as_sftime(as.data.frame(stdata),
    coords = c("lon", "lat"),
    crs = 4326,
    time_column_name = "time"
  )
  expect_error(
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
  expect_error(check_mysftime(mysft), "time column should be called time")
  mysft <- stdata |>
    sftime::st_as_sftime(
      coords = c("lon", "lat"),
      crs = 4326,
      time_column_name = "time"
    ) |>
    dplyr::rename("geom" = "geometry")
  expect_error(
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
  expect_error(
    check_mysftime(mysft),
    "geometry is not a sfc_POINT"
  )
})


test_that("check_mysf works as expected", {
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
  expect_no_error(check_mysf(x = mysf))

  # check that error messages work well
  expect_error(check_mysf(stdata), "x is not a sf")
  mysf <- sf::st_as_sf(as.data.frame(stdata),
    coords = c("lon", "lat"),
    crs = 4326
  )
  expect_error(
    check_mysf(x = mysf),
    "x is not inherited from a data.table"
  )
  mysf <- stdata |>
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326
    ) |>
    dplyr::rename("geom" = "geometry")
  expect_error(
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
  expect_error(
    check_mysf(mysf),
    "geometry is not a sfc_POINT"
  )
})

test_that("rename_time works as expected", {
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
  expect_no_error(rename_time(mysft, "date"))
  expect_equal(
    attributes(rename_time(mysft, "date"))$time_column,
    "date"
  )
  expect_error(
    rename_time(stdata, "date"),
    "x is not a sftime"
  )
})

test_that("dt_as_mysftime works as expected", {
  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  # should work
  expect_no_error(dt_as_mysftime(
    x = stdata,
    lonname = "lon",
    latname = "lat",
    timename = "time",
    crs = 4326
  ))
  expect_no_error(check_mysftime(dt_as_mysftime(
    x = stdata,
    lonname = "lon",
    latname = "lat",
    timename = "time",
    crs = 4326
  )))
  expect_error(
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

test_that("as_mysftime works as expected", {
  withr::local_package("terra")
  withr::local_package("data.table")
  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  # with data.table
  expect_no_error(as_mysftime(
    x = stdata,
    lonname = "lon",
    latname = "lat",
    timename = "time",
    crs = 4326
  ))
  expect_no_error(check_mysftime(as_mysftime(
    x = stdata,
    lonname = "lon",
    latname = "lat",
    timename = "time",
    crs = 4326
  )))
  expect_error(
    as_mysftime(x = stdata),
    "argument \"lonname\" is missing, with no default"
  )
  # with data.frame
  expect_no_error(as_mysftime(
    x = as.data.frame(stdata),
    lonname = "lon",
    latname = "lat",
    timename = "time",
    crs = 4326
  ))
  expect_no_error(check_mysftime(as_mysftime(
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
  expect_no_error(as_mysftime(mysf, "time"))
  b <- mysf |>
    dplyr::rename("date" = "time")
  expect_no_error(as_mysftime(b, "date"))
  expect_no_error(check_mysftime(as_mysftime(b, "date")))
  # with sftime
  mysft <- sftime::st_as_sftime(stdata,
    coords = c("lon", "lat"),
    crs = 4326,
    time_column_name = "time"
  )
  expect_no_error(as_mysftime(mysft, "time"))
  expect_no_error(check_mysftime(as_mysftime(mysft, "time")))
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
  expect_no_error(as_mysftime(x = myrast, varname = "altitude"))
  expect_no_error(check_mysftime(as_mysftime(x = myrast, varname = "altitude")))
  # with SpatVector
  myvect <- terra::vect(
    stdata,
    geom = c("lon", "lat"),
    crs = "EPSG:4326",
    keepgeom = FALSE
  )
  expect_no_error(as_mysftime(x = myvect))
  expect_no_error(check_mysftime(as_mysftime(x = myvect)))
  myvect <- stdata |>
    dplyr::rename("time2" = time) |>
    terra::vect(
      geom = c("lon", "lat"),
      crs = "EPSG:4326",
      keepgeom = FALSE
    )
  expect_error(
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
  expect_no_error(as_mysftime(myrds))
  expect_error(
    as_mysftime(x = "roquefort"),
    "x class not accepted"
  )
})


test_that("sftime_as_spatvector as expected", {
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
  expect_no_error(sftime_as_spatvector(mysftime))
  # with a different time column name:
  attributes(mysftime)$time_column <- "date"
  mysftime <- dplyr::rename(mysftime, "date" = "time")
  expect_no_error(sftime_as_spatvector(mysftime))
  # doesn't work with other classes:
  expect_error(sftime_as_spatvector(stdata))
})

test_that("sf_as_mysftime works as expected", {
  # open testing data
  stdata <- data.table::fread(paste0(
    testthat::test_path("..", "testdata/", ""),
    "spacetime_table.csv"
  ))
  mysf <- sf::st_as_sf(stdata, coords = c("lon", "lat"), crs = 4326)
  expect_no_error(sf_as_mysftime(mysf, "time"))
  expect_no_error(check_mysftime(sf_as_mysftime(mysf, "time")))
  b <- mysf |>
    dplyr::rename("date" = "time")
  expect_no_error(check_mysftime(sf_as_mysftime(b, "date")))
  expect_error(sf_as_mysftime(b, "time"),
               "time column missing or mispelled")
})

test_that("sftime_as_mysftime works as expected", {
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
  expect_no_error(sftime_as_mysftime(mysft, "time"))
  expect_no_error(check_mysftime(sftime_as_mysftime(mysft, "time")))
  attributes(mysft)$time_column <- "date"
  mysft <- dplyr::rename(mysft, "date" = "time")
  expect_no_error(check_mysftime(sf_as_mysftime(mysft, "date")))
  expect_error(sf_as_mysftime(mysft, "time"),
               "time column missing or mispelled")
})


test_that("spatraster_as_sftime works as expected", {
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
  expect_no_error(spatraster_as_sftime(myrast, "myvar"))
  expect_no_error(spatraster_as_sftime(myrast, "myvar", "date"))
  mysft <- spatraster_as_sftime(myrast, "myvar", "date")
  expect_equal(attributes(mysft)$time, "date")
  # conversion does not work because raster's names are not dates
  names(myrast) <- c("roquefort", "comte", "camembert")
  expect_error(
    spatraster_as_sftime(myrast, "myvar"),
    "x layers might not be time"
  )
})


test_that("spatrds_as_sftime works as expected", {
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
  expect_no_error(spatrds_as_sftime(myrds, "time"))
  mysft <- spatrds_as_sftime(myrds, "date")
  expect_equal(attributes(mysft)$time, "date")
})


test_that("sftime_as_sf works as expected", {
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
  expect_no_error(sftime_as_sf(mysftime))
  expect_no_error(sftime_as_sf(mysftime, keeptime = FALSE))
  expect_equal(class(sftime_as_sf(mysftime))[1], "sf")
  expect_equal(class(sftime_as_sf(mysftime, keeptime = FALSE))[1], "sf")
  expect_true("time" %in% colnames(sftime_as_sf(mysftime, keeptime = TRUE)))
  expect_false("time" %in% colnames(sftime_as_sf(mysftime, keeptime = FALSE)))
})

test_that("sftime_as_sf works as expected", {
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
  expect_no_error(sftime_as_sf(mysftime))
  expect_no_error(sftime_as_sf(mysftime, keeptime = FALSE))
  expect_equal(class(sftime_as_sf(mysftime))[1], "sf")
  expect_equal(class(sftime_as_sf(mysftime, keeptime = FALSE))[1], "sf")
  expect_true("time" %in% colnames(sftime_as_sf(mysftime, keeptime = TRUE)))
  expect_false("time" %in% colnames(sftime_as_sf(mysftime, keeptime = FALSE)))
})


test_that("sftime_as_spatraster works as expected", {
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
  expect_no_error(sftime_as_spatraster(mysftime, "roquefort"))
  expect_error(
    sftime_as_spatraster(mysftime, "cheddar"),
    "varname missing or mispelled"
  )
})


test_that("sftime_as_spatrds works as expected", {
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
  expect_no_error(sftime_as_spatrds(mysft))
  expect_error(sftime_as_spatrds("hello"), "x is not a sftime")
  rename_time(mysft, "date")
  expect_no_error(sftime_as_spatrds(mysft))
})