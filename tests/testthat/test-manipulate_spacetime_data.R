test_that("convert_stobj_to_stdt works well", {
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <- seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  df <- rbind(df, df)
  df$time <- c(rep("2023-11-02", 25), rep("2023-11-03", 25))
  df$var1 <- 1:50
  df$var2 <- 51:100

  # test that is fails if stobj is not from an accepted st class
  expect_error(
    convert_stobj_to_stdt(stobj = "I love cheese")
  )

  # test with dataframe / datatable objects
  # 1) it should work
  expect_no_error(convert_stobj_to_stdt(df))
  expect_no_error(convert_stobj_to_stdt(data.table::as.data.table(df)))
  expect_true(is.na(convert_stobj_to_stdt(df)$crs_stdt))
  expect_equal(class(convert_stobj_to_stdt(df)$stdt)[[1]], "data.table")
  expect_false(any(!(c("lon", "lat", "time") %in%
                       colnames(convert_stobj_to_stdt(df)$stdt))))
  # 2) it should fail because time column is missing
  df$time <- NULL
  expect_error(
    convert_stobj_to_stdt(df)
  )

  # test with sf / sftime objects
  # 1) it should work
  df$time <- c(rep("2023-11-02", 25), rep("2023-11-03", 25))
  stobj <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = "EPSG:4326")
  expect_no_error(convert_stobj_to_stdt(stobj))
  stdt <- convert_stobj_to_stdt(stobj)$stdt
  crsdt <- convert_stobj_to_stdt(stobj)$crs_stdt
  expect_equal(class(stdt)[[1]], "data.table")
  expect_equal(class(crsdt), "character")
  expect_true(terra::same.crs(crsdt, "EPSG:4326"))
  expect_false(any(!(c("lon", "lat", "time") %in% colnames(stdt))))
  expect_equal(
    stdt[lon == -112 & lat == 35.35 & time == "2023-11-02", var1],
    6
  )
  expect_equal(
    stdt[lon == -112 & lat == 35.35 & time == "2023-11-02", var2],
    56
  )
  df$time <- as.Date(df$time)
  stobj <- sftime::st_as_sftime(df,
    coords = c("lon", "lat"),
    time_column_name = "time",
    crs = "EPSG:4326"
  )
  expect_no_error(convert_stobj_to_stdt(stobj))
  expect_equal(class(convert_stobj_to_stdt(stobj)$crs_stdt), "character")
  # 2) it should fail because time columns is misspelled
  stobj$time <- stobj$time
  stobj$time <- NULL
  expect_error(
    convert_stobj_to_stdt(stobj),
    "Error: stobj does not contain geometry and time columns"
  )

  # test with SpatVector objects
  # 1) it should work
  stobj <-
    terra::vect(
      df,
      geom = c("lon", "lat"),
      crs = "EPSG:4326",
      keepgeom = FALSE
    )
  expect_no_error(convert_stobj_to_stdt(stobj))
  stdt <- convert_stobj_to_stdt(stobj)$stdt
  expect_equal(class(stdt)[[1]], "data.table")
  expect_equal(class(convert_stobj_to_stdt(stobj)$crs_stdt), "character")
  expect_true({
    terra::same.crs(
      convert_stobj_to_stdt(stobj)$crs_stdt,
      "EPSG:4326"
    )
  })
  expect_false(any(!(c("lon", "lat", "time") %in% colnames(stdt))))
  expect_equal(
    stdt[lon == -112 & lat == 35.35 & time == "2023-11-02", var1],
    6
  )
  expect_equal(
    stdt[lon == -112 & lat == 35.35 & time == "2023-11-02", var2],
    56
  )

  # test with SpatRastDataset created from 2 SpatRast (i.e. 2 variables)
  # with 3 layers (i.e. 3 timestamps)
  # 1) it should work
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
  values(var1) <- seq(-5, 19)
  add(var1) <- c(var1**2, var1**3)
  names(var1) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  var2 <- rast(
    extent = c(-112, -101, 33.5, 40.9),
    ncol = 5,
    nrow = 5,
    crs = "EPSG:4326"
  )
  values(var2) <- seq(-15, 9)
  add(var2) <- c(var2**2, var2**3)
  names(var2) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  stobj <- terra::sds(var1, var2)
  names(stobj) <- c("var1", "var2")
  expect_no_error(stdt_converted <- convert_stobj_to_stdt(stobj))
  expect_equal(class(stdt_converted$stdt)[[1]], "data.table")
  expect_equal(class(stdt_converted$crs_stdt), "character")
  expect_true(terra::same.crs(stdt_converted$crs_stdt, "EPSG:4326"))

  expect_false({
    any(!(c("lon", "lat", "time") %in%
            colnames(stdt_converted$stdt)))
  })
  expect_equal(
    {
      stdt_converted$stdt[
        lon == -106.5 &
          lat == stdt_converted$stdt$lat[37] &
          time == "2023-11-02", var1
      ]
    },
    49
  )
  expect_equal(
    {
      stdt_converted$stdt[
        lon == -106.5 &
          lat == stdt_converted$stdt$lat[37] &
          time == "2023-11-02", var2
      ]
    },
    9
  )

  var1sds <- terra::sds(var1)
  expect_error(convert_stobj_to_stdt(var1sds))

  # convert stdt to spatrastdataset test
  expect_no_error(sds_from_stdt <- convert_stdt_spatrastdataset(stdt_converted))
  expect_s4_class(sds_from_stdt, "SpatRasterDataset")
})



test_that("is_stdt works as expected", {
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <- seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  df <- rbind(df, df)
  df$time <- c(rep("2023-11-02", 25), rep("2023-11-03", 25))
  df$var1 <- 1:50
  df$var2 <- 51:100

  errstdt1 <- list(1L, 2L)
  expect_equal(is_stdt(errstdt1), FALSE)
  names(errstdt1) <- c("stdt", "crs_stdt")
  expect_equal(is_stdt(errstdt1), FALSE)
  class(errstdt1) <- c("list", "stdt")
  expect_equal(is_stdt(errstdt1), FALSE)
  errstdt2 <- errstdt1
  errstdt2$stdt <- data.table(A = 1, B = 2, C = 3)
  expect_equal(is_stdt(errstdt2), FALSE)
  names(errstdt2$stdt) <- c("lon", "lat", "time")
  expect_equal(is_stdt(errstdt2), FALSE)
  expect_error(convert_stdt(errstdt2))
  expect_error(convert_stdt_spatvect(errstdt2))
  expect_error(convert_stdt_sftime(errstdt2))
  expect_error(convert_stdt_spatrastdataset(errstdt2))
})

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

test_that("dt_as_sf works as expected", {
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <- seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  df <- rbind(df, df)
  df$time <- c(rep("2023-11-02", 25), rep("2023-11-03", 25))
  df$var1 <- 1:50
  df$var2 <- 51:100

  expect_error(dt_as_sf(df, 3L))
  expect_error(dt_as_sf(as.data.table(df), 3L))

  dfe <- as.data.table(df)
  names(dfe)[1] <- "xcoord"
  expect_error(dt_as_sf(dfe, "EPSG:4326"))
  dfe <- as.data.table(df)
  names(dfe)[2] <- "ycoord"
  expect_error(dt_as_sf(dfe, "EPSG:4326"))

  dfdt <- as.data.table(df)
  expect_no_error(dt_as_sf(dfdt, "EPSG:4326"))
  dfsf <- dt_as_sf(dfdt, "EPSG:4326")
  expect_s3_class(dfsf, "sf")
})


test_that("dt_as_sftime works as expected", {
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <- seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  df <- rbind(df, df)
  df$time <- as.Date(c(rep("2023-11-02", 25), rep("2023-11-03", 25)))
  df$var1 <- 1:50
  df$var2 <- 51:100

  expect_error(dt_as_sftime(df, 3L))
  expect_error(dt_as_sftime(as.data.table(df), 3L))

  dfe <- as.data.table(df)
  names(dfe)[1] <- "xcoord"
  expect_error(dt_as_sftime(dfe, "EPSG:4326"))
  dfe <- as.data.table(df)
  names(dfe)[2] <- "ycoord"
  expect_error(dt_as_sftime(dfe, "EPSG:4326"))

  dfdt <- as.data.table(df)
  expect_no_error(dt_as_sftime(dfdt, "EPSG:4326"))
  dfsf <- dt_as_sftime(dfdt, "EPSG:4326")
  expect_s3_class(dfsf, "sftime")

  df_nonstandard <- df
  colnames(df_nonstandard)[3] <- "yeardate"
  expect_error(dt_as_sftime(df_nonstandard, "EPSG:4326"))
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


test_that("project_dt works as expected", {
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <- seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  df <- rbind(df, df)
  df$time <- c(rep("2023-11-02", 25), rep("2023-11-03", 25))
  df$var1 <- 1:50
  df$var2 <- 51:100
  dfdt <- as.data.table(df)

  expect_error(project_dt(dfdt, 3L, 2L))
  expect_error(project_dt(dfdt, "EPSG:4326", 2L))
  expect_error(project_dt(as.matrix(dfdt), 3L, 2L))

  dfdte <- dfdt
  names(dfdte)[1] <- "xcoord"
  expect_error(project_dt(dfdte, "EPSG:4326", "EPSG:5070"))
  dfdte <- as.data.table(df)
  names(dfdte)[2] <- "ycoord"
  expect_error(project_dt(dfdte, "EPSG:4326", "EPSG:5070"))

  expect_no_error(project_dt(dfdt, "EPSG:4326", "EPSG:5070"))
  dfdtp <- project_dt(dfdt, "EPSG:4326", "EPSG:5070")

  expect_s3_class(dfdtp, "data.table")
})
