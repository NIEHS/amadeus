test_that("convert_stobj_to_stdt works well", {
  withr::local_package("sf")
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
                keepgeom = FALSE)
  expect_no_error(convert_stobj_to_stdt(stobj))
  stdt <- convert_stobj_to_stdt(stobj)$stdt
  expect_equal(class(stdt)[[1]], "data.table")
  expect_equal(class(convert_stobj_to_stdt(stobj)$crs_stdt), "character")
  expect_true({
               terra::same.crs(convert_stobj_to_stdt(stobj)$crs_stdt,
                               "EPSG:4326")})
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
                crs = "EPSG:4326")
  terra::values(var1) <- seq(-5, 19)
  terra::add(var1) <- c(var1 ** 2, var1 ** 3)
  var1 <- rast(
    extent = c(-112, -101, 33.5, 40.9),
    ncol = 5,
    nrow = 5,
    crs = "EPSG:4326"
  )
  values(var1) <- seq(-5, 19)
  add(var1) <- c(var1 ** 2, var1 ** 3)
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
                        colnames(stdt_converted$stdt)))})
  expect_equal({
                stdt_converted$stdt[
                                    lon == -106.5 &
                                      lat == stdt_converted$stdt$lat[37] &
                                      time == "2023-11-02", var1]},
  49)
  expect_equal({
                stdt_converted$stdt[
                                    lon == -106.5 &
                                      lat == stdt_converted$stdt$lat[37] &
                                      time == "2023-11-02", var2]},
  9)

  var1sds <- terra::sds(var1)
  expect_error(convert_stobj_to_stdt(var1sds))

  # convert stdt to spatrastdataset test
  expect_no_error(sds_from_stdt <- convert_stdt_spatrastdataset(stdt_converted))
  expect_s4_class(sds_from_stdt, "SpatRasterDataset")

})



test_that("is_stdt works as expected", {
  withr::local_package("sf")
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


test_that("dt_to_sf works as expected", {
  withr::local_package("sf")
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

  expect_error(dt_to_sf(df, 3L))
  expect_error(dt_to_sf(as.data.table(df), 3L))

  dfe <- as.data.table(df)
  names(dfe)[1] <- "xcoord"
  expect_error(dt_to_sf(dfe, "EPSG:4326"))
  dfe <- as.data.table(df)
  names(dfe)[2] <- "ycoord"
  expect_error(dt_to_sf(dfe, "EPSG:4326"))

  dfdt <- as.data.table(df)
  expect_no_error(dt_to_sf(dfdt, "EPSG:4326"))
  dfsf <- dt_to_sf(dfdt, "EPSG:4326")
  expect_s3_class(dfsf, "sf")
})


test_that("dt_to_sftime works as expected", {
  withr::local_package("sf")
  withr::local_package("sftime")
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

  expect_error(dt_to_sftime(df, 3L))
  expect_error(dt_to_sftime(as.data.table(df), 3L))

  dfe <- as.data.table(df)
  names(dfe)[1] <- "xcoord"
  expect_error(dt_to_sftime(dfe, "EPSG:4326"))
  dfe <- as.data.table(df)
  names(dfe)[2] <- "ycoord"
  expect_error(dt_to_sftime(dfe, "EPSG:4326"))

  dfdt <- as.data.table(df)
  expect_no_error(dt_to_sftime(dfdt, "EPSG:4326"))
  dfsf <- dt_to_sftime(dfdt, "EPSG:4326")
  expect_s3_class(dfsf, "sftime")

  df_nonstandard <- df
  colnames(df_nonstandard)[3] <- "yeardate"
  expect_error(dt_to_sftime(df_nonstandard, "EPSG:4326"))
})



test_that("project_dt works as expected", {
  withr::local_package("sf")
  withr::local_package("sftime")
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
