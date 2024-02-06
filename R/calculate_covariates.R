#' Calculate covariates

# nocov start
# nolint start
#' @param covariate character(1). Covariate type.
#' @param path character. Single or multiple path strings.
#' @param sites sf/SpatVector. Unique sites. Should include
#'  a unique identifier field named \code{id_col}
#' @param id_col character(1). Name of unique identifier.
#'  Default is \code{"site_id"}.
#' @param ... Arguments passed to each covariate calculation
#'  function.
#' @seealso
#' - \link{calc_modis}: `"modis"`, `"MODIS"`
#' - \link{calc_koppen_geiger}: `"koppen-geiger"`, `"koeppen-geiger"`, `"koppen"`,
#' - \link{calc_ecoregion}: `"ecoregion"`, `"ecoregions"`
#' - \link{calc_temporal_dummies}: `"dummies"`
#' @returns Calculated covariates. Mainly data.frame object.
#' @author Insang Song
#' @export
# nolint end
calc_covariates <-
  function(
      covariate = c("modis", "koppen-geiger",
                    "koeppen-geiger", "koppen", "koeppen",
                    "geos", "dummies", "gmted", "roads",
                    "sedac_groads", "nlcd", "tri", "ncep", "aadt",
                    "ecoregions", "ecoregion"),
      path,
      sites,
      id_col = "site_id",
      ...) {

    covariate <- tolower(covariate)
    covariate <- match.arg(covariate)
    if (startsWith(covariate, "ko")) {
      covariate <- "koppen"
    }

    # select function to run
    what_to_run <- switch(covariate,
      modis = calc_modis,
      ecoregion = calc_ecoregion,
      ecoregions = calc_ecoregion,
      koppen = calc_koppen_geiger,
      # narr_monolevel = calc_narr_monolevel,
      # monolevel = calc_narr_monolevel,
      # narr_p_levels = calc_narr_p_levels,
      # p_levels = calc_narr_p_levels,
      # plevels = calc_narr_p_levels,
      nlcd = calc_nlcd_ratio,
      # noaa = calc_noaa_hms,
      # smoke = calc_noaa_hms,
      # hms = calc_noaa_hms,
      # sedac_groads = calc_sedac_groads,
      # roads = calc_sedac_groads,
      # sedac_population = calc_sedac_population,
      # population = calc_sedac_population,
      # aadt = calc_aadt,
      # tri = calc_tri,
      # ncep = calc_ncep,
      # geos = calc_geos,
      # gmted = calc_gmted,
      dummies = calc_temporal_dummies
    )

    res_covariate <-
      tryCatch({
        what_to_run(
          path = path,
          sites = sites,
          id_col = id_col,
          ...
        )
      }, error = function(e) {
        print(e)
        print(args(what_to_run))
        message(paste0("Please refer to the argument list and
                        the error message above to rectify the error.\n"))
        return(NULL)
      })

    return(res_covariate)
  }
# nocov end

#' Calculate Koeppen-Geiger climate zone binary variables
#' @param path character(1). Path to Koppen-Geiger
#'  climate zone raster file
#' @param sites sf/SpatVector. Unique sites. Should include
#'  a unique identifier field named \code{id_col}
#' @param id_col character(1). Name of unique identifier.
#' @returns a data.frame object
#' @author Insang Song
#' @importFrom terra vect
#' @importFrom terra rast
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom terra extract
#' @importFrom terra coltab
#' @importFrom terra merge
#' @importFrom methods is
#' @export
calc_koppen_geiger <-
  function(
      path = "./input/koppen_geiger/raw/Beck_KG_V1_present_0p0083.tif",
      sites,
      id_col = "site_id") {
    ## You will get "sites" in memory after sourcing the file above
    kg_rast <- terra::rast(path)
    sites_tr <- sites

    if (!methods::is(sites, "SpatVector")) {
      sites_tr <- terra::vect(sites)
    }
    sites_kg <- terra::project(sites_tr, terra::crs(kg_rast))
    sites_kg_extract <- terra::extract(kg_rast, sites_kg)

    # The starting value is NA as the color table has 0 value in it
    kg_class <-
      c(
        NA, "Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", "Csa", "Csb",
        "Csc", "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc",
        "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
        "Dfa", "Dfb", "Dfc", "Dfd", "ET", "EF"
      )
    kg_coltab <- terra::coltab(kg_rast)
    kg_coltab <- kg_coltab[[1]][seq(1, 31), ]
    kg_colclass <- data.frame(
      value = kg_coltab$value,
      class_kg = kg_class
    )

    sites_kg_extract[[id_col]] <- unlist(sites_kg[[id_col]])
    colnames(sites_kg_extract)[2] <- "value"
    sites_kg_extract_e <- merge(sites_kg_extract, kg_colclass, by = "value")

    # "Dfa": 25
    # "BSh": 6
    # "Dfb": 26
    id_search <- unlist(sites_kg_extract_e[[id_col]])
    # errorfix: how to generalize and auto-fix it?
    sites_kg_extract_e[
      which(id_search == "44009000788101"),
      "class_kg"
    ] <- "Dfa"
    sites_kg_extract_e[
      which(id_search == "48061200488101"),
      "class_kg"
    ] <- "BSh"
    sites_kg_extract_e[
      which(id_search == "33015001488101"),
      "class_kg"
    ] <- "Dfb"

    sites_kg_extract_e$class_kg <-
      as.factor(substr(sites_kg_extract_e$class_kg, 1, 1))
    # currently there are no "E" region in sites.
    # however, E is filled with all zeros at the moment.
    aelabels <- LETTERS[1:5]
    df_ae_separated <-
      split(aelabels, aelabels) |>
      lapply(function(x) {
        as.integer(sites_kg_extract_e$class_kg == x)
      }) |>
      Reduce(f = cbind, x = _) |>
      as.data.frame()
    colnames(df_ae_separated) <- sprintf("DUM_CLRG%s_0_00000", aelabels)

    kg_extracted <-
      cbind(
        site_id = unlist(sites_kg_extract_e[[id_col]]),
        df_ae_separated
      )
    return(kg_extracted)
  }


#' Compute land cover classes ratio in circle buffers around points
#'
#' @param path character giving nlcd data path
#' @param sites terra::SpatVector of points geometry
#' @param radius numeric (non-negative) giving the
#' radius of buffer around points
#' @param year numeric giving the year of NLCD data used
#' @importFrom utils read.csv
#' @importFrom utils data
#' @importFrom terra rast
#' @importFrom terra project
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom terra same.crs
#' @importFrom terra buffer
#' @importFrom sf st_union
#' @importFrom sf st_geometry
#' @importFrom terra intersect
#' @importFrom exactextractr exact_extract
#' @import spData
#' @export
calc_nlcd_ratio <- function(path,
                            sites,
                            radius = 1000,
                            year = 2021) {
  # check inputs
  if (!is.numeric(radius)) {
    stop("radius is not a numeric.")
  }
  if (radius <= 0) {
    stop("radius has not a likely value.")
  }
  if (!is.numeric(year)) {
    stop("year is not a numeric.")
  }
  if (class(sites)[1] != "SpatVector") {
    stop("sites is not a terra::SpatVector.")
  }
  if (!is.character(path)) {
    stop("path is not a character.")
  }
  if (!file.exists(path)) {
    stop("path does not exist.")
  }
  # open nlcd file corresponding to the year
  nlcd_file <- list.files(path,
                          pattern = paste0("nlcd_", year, "_.*.tif$"),
                          full.names = TRUE)
  if (length(nlcd_file) == 0) {
    stop("NLCD data not available for this year.")
  }
  nlcd <- terra::rast(nlcd_file)
  # select points within mainland US and reproject on nlcd crs if necessary
  # need spData library
  utils::data("us_states", package = "spData")
  us_main <-
    terra::ext(c(xmin = -127, xmax = -65, ymin = 24, ymax = 51)) |>
    terra::vect() |>
    terra::set.crs("EPSG:4326") |>
    terra::project(y = terra::crs(sites))
  data_vect_b <- sites |>
    terra::intersect(x = us_main)
  if (!terra::same.crs(data_vect_b, nlcd)) {
    data_vect_b <- terra::project(data_vect_b, terra::crs(nlcd))
  }
  # create circle buffers with buf_radius
  bufs_pol <- terra::buffer(data_vect_b, width = radius) |>
    sf::st_as_sf()
  # ratio of each nlcd class per buffer
  nlcd_at_bufs <- exactextractr::exact_extract(nlcd,
                                               sf::st_geometry(bufs_pol),
                                               fun = "frac",
                                               stack_apply = TRUE,
                                               progress = FALSE)
  # select only the columns of interest
  nlcd_at_bufs <- nlcd_at_bufs[names(nlcd_at_bufs)[grepl("frac_",
                                                         names(nlcd_at_bufs))]]
  # change column names
  fpath <- system.file("extdata", "nlcd_classes.csv", package = "amadeus")
  nlcd_classes <- utils::read.csv(fpath)
  nlcd_names <- names(nlcd_at_bufs)
  nlcd_names <- sub(pattern = "frac_", replacement = "", x = nlcd_names)
  nlcd_names <- as.numeric(nlcd_names)
  nlcd_names <- nlcd_classes[nlcd_classes$value %in% nlcd_names, c("class")]
  new_names <- sapply(
    nlcd_names,
    function(x) {
      sprintf("LDU_%s_0_%05d_%04d", x, radius, year)
    }
  )
  names(nlcd_at_bufs) <- new_names
  # merge data_vect with nlcd class fractions (and reproject)
  new_data_vect <- cbind(data_vect_b, nlcd_at_bufs)
  new_data_vect <- terra::project(new_data_vect, terra::crs(sites))
  return(new_data_vect)
}


#' Calculate EPA Ecoregions level 2/3 binary variables
#' @param path character(1). Path to Ecoregion Shapefiles
#' @param sites sf/SpatVector. Unique sites. Should include
#'  a unique identifier field named \code{id_col}
#' @param id_col character(1). Name of unique identifier.
#' @returns a data.frame object with dummy variables and attributes of:
#'   - \code{attr(., "ecoregion2_code")}: Ecoregion lv.2 code and key
#'   - \code{attr(., "ecoregion3_code")}: Ecoregion lv.3 code and key
#' @author Insang Song
#' @importFrom methods is
#' @importFrom terra vect
#' @importFrom terra project
#' @importFrom terra intersect
#' @importFrom terra snap
#' @importFrom terra extract
#' @importFrom terra crs
#' @export
calc_ecoregion <-
  function(
    path = "./input/data/ecoregions/raw/us_eco_l3_state_boundaries.shp",
    sites,
    id_col = "site_id"
  ) {

    if (!methods::is(sites, "SpatVector")) {
      sites <- terra::vect(sites)
    }
    ecoreg <- terra::vect(path)
    ecoreg <- ecoreg[, grepl("^(L2_KEY|L3_KEY)", names(ecoreg))]

    sites <- terra::project(sites, terra::crs(ecoreg))

    sites_in <- terra::intersect(sites, ecoreg)
    sites_out <-
      sites[!unlist(sites[[id_col]]) %in% unlist(sites_in[[id_col]]), ]

    sites_snapped <- terra::snap(sites_out, ecoreg, tolerance = 50)
    sites_fixed <- rbind(sites_in, sites_snapped)
    extracted <- terra::extract(ecoreg, sites_fixed)

    # Generate field names from extracted ecoregion keys
    # TODO: if we keep all-zero fields, the initial reference
    # should be the ecoregion polygon, not the extracted data
    key2_sorted <- unlist(extracted[, 3])
    key2_num <-
      regmatches(key2_sorted, regexpr("\\d{1,2}\\.[1-9]", key2_sorted))
    key2_num <- as.integer(10 * as.numeric(key2_num))
    key2_num <- sprintf("DUM_E2%03d_0_00000", key2_num)
    key2_num_unique <- sort(unique(key2_num))

    key3_sorted <- unlist(extracted[, 2])
    key3_num <-
      regmatches(key3_sorted, regexpr("\\d{1,3}", key3_sorted))
    key3_num <- as.integer(as.numeric(key3_num))
    key3_num <- sprintf("DUM_E3%03d_0_00000", key3_num)
    key3_num_unique <- sort(unique(key3_num))


    df_lv2 <-
      split(key2_num_unique, key2_num_unique) |>
      lapply(function(x) {
        as.integer(key2_num == x)
      }) |>
      Reduce(f = cbind, x = _) |>
      as.data.frame()
    colnames(df_lv2) <- key2_num_unique
    df_lv3 <-
      split(key3_num_unique, key3_num_unique) |>
      lapply(function(x) {
        as.integer(key3_num == x)
      }) |>
      Reduce(f = cbind, x = _) |>
      as.data.frame()
    colnames(df_lv3) <- key3_num_unique

    sites_ecoreg <- cbind(sites[[id_col]], df_lv2, df_lv3)
    attr(sites_ecoreg, "ecoregion2_code") <- sort(unique(ecoreg$L2_KEY))
    attr(sites_ecoreg, "ecoregion3_code") <- sort(unique(ecoreg$L3_KEY))
    return(sites_ecoreg)
  }


#' A single-date MODIS worker for parallelization
#' @description modis_worker operates at six MODIS/VIIRS products
#' (MOD11A1, MOD13A2, MOD06_L2, VNP46A2, MOD09GA, and MCD19A2)
#' on a daily basis. Given that the raw hdf files are downloaded from
#' NASA, standard file names include a data retrieval date flag starting
#' with A. Leveraging that piece of information, the function will select
#' files of scope on the date of interest. Please note that this function
#' does not provide a function to filter swaths or tiles, so it is strongly
#' recommended to check and pre-filter the file names at users' discretion.
#' @param raster SpatRaster.
#' @param date Date(1). date to query.
#' @param sites_in SpatVector/sf/sftime object. AQS sites.
#' @param name_extracted character. Names of calculated covariates.
#' @param product character(1). Product code of MODIS. Should be one of
#' \code{c('MOD11A1', 'MOD13A2', 'MOD06_L2', 'VNP46A2', 'MOD09GA', 'MCD19A2')}
#' @param fun_summary_raster function. Summary function for
#' multilayer rasters. Passed to \code{foo}. See also
#' \code{\link[exactextractr]{exact_extract}}
#' @param id_col character(1). Field name where unique site identifiers
#' are stored. Default is `"site_id"`
#' @param radius numeric. Radius to buffer.
#' @author Insang Song
#' @returns A data.frame object.
#' @importFrom terra extract
#' @importFrom terra project
#' @importFrom terra vect
#' @importFrom terra nlyr
#' @importFrom terra describe
#' @importFrom methods is
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @export
modis_worker <- function(
  raster,
  date,
  sites_in = NULL,
  name_extracted = NULL,
  product = c("MOD11A1", "MOD13A2", "MOD06_L2",
              "VNP46A2", "MOD09GA", "MCD19A2"),
  fun_summary_raster = "mean",
  id_col = "site_id",
  radius = 0L
) {
  if (!any(methods::is(sites_in, "SpatVector"),
           methods::is(sites_in, "sf"),
           methods::is(sites_in, "sftime"),
           is_stdt(sites_in))) {
    stop("sites_in should be one of sf, sftime, stdt, or SpatVector.\n")
  }
  if (is_stdt(sites_in)) {
    sites_in <- convert_stdt_spatvect(sites_in)
  }
  if (!methods::is(sites_in, "SpatVector")) {
    sites_in <- terra::vect(sites_in)
  }
  if (!id_col %in% names(sites_in)) {
    stop(sprintf("sites should include columns named %s.\n",
                 id_col)
    )
  }
  if (!"time" %in% names(sites_in)) {
    sites_in$time <- date
  }

  extract_with_buffer <- function(
    points,
    surf,
    radius,
    id,
    time = "time",
    func = "mean"
  ) {
    # generate buffers
    bufs <- terra::buffer(points, width = radius, quadsegs = 180L)
    bufs <- terra::project(bufs, terra::crs(surf))
    # crop raster (deprecated)

    # extract raster values
    surf_at_bufs <-
      exactextractr::exact_extract(
        x = surf,
        y = sf::st_as_sf(bufs),
        fun = func,
        force_df = TRUE,
        append_cols = c(id, time),
        progress = FALSE,
        max_cells_in_memory = 1e7
      )
    return(surf_at_bufs)
  }

  product <- match.arg(product)

  ## internal NaN values 65535 to NaN
  if (product == "VNP46A2") {
    raster[raster == 65535L] <- NaN
  }
  ## NaN to zero
  raster[is.nan(raster)] <- 0L

  # raster used to be vrt_today
  if (any(grepl("00000", name_extracted))) {
    sites_tr <- terra::project(sites_in, terra::crs(raster))
    extracted <- terra::extract(x = raster, y = sites_tr, ID = FALSE)
    sites_blank <- as.data.frame(sites_in)
    extracted <- cbind(sites_blank, extracted)
  } else {
    extracted <-
      extract_with_buffer(
        points = sites_in,
        surf = raster,
        id = id_col,
        radius = radius,
        func = fun_summary_raster
      )
  }

  # cleaning names
  # assuming that extracted is a data.frame
  #extracted$time <- date
  name_offset <- terra::nlyr(raster)
  # multiple columns will get proper names
  name_range <- seq(ncol(extracted) - name_offset + 1, ncol(extracted), 1)
  colnames(extracted)[name_range] <- name_extracted
  return(extracted)
}



#' Calculate MODIS product covariates in multiple CPU threads
#' @description calc_modis essentially runs \code{modis_worker} function
#' in each thread (subprocess). Based on daily resolution, each day's workload
#' will be distributed to each thread. With \code{product} argument,
#' the files are processed by a customized function where the unique structure
#' and/or characteristics of the products are considered. \code{nthreads}
#' argument should be carefully selected in consideration of the machine's
#' CPU and memory capacities as products have their own memory pressure.
#' Overall, this function and dependent routines assume that the file system
#' can handle concurrent access to the (network) disk by multiple processes.
#' File system characteristics, package versions, and hardware settings/
#' specification can affect the processing efficiency.
#' @param path character. List of HDF files.
#' @param product character(1). MODIS product. Should be one of
#' \code{c("MOD11A1", "MOD13A2", "MOD06_L2", "VNP46A2", "MOD09GA", "MCD19A2")}
#' @param sites sf object. Unique sites where covariates
#' will be calculated.
#' @param id_col character(1). Site identifier. Default is `"site_id"`
#' @param name_covariates character. Name header of covariates.
#' e.g., `"MOD_NDVIF_0_"`.
#' The calculated covariate names will have a form of
#' '{name_covariates}{zero-padded buffer radius in meters}',
#' e.g., 'MOD_NDVIF_0_50000' where 50 km radius circular buffer
#' was used to calculate mean NDVI value.
#' @param radius numeric. Radii to calculate covariates.
#' Default is `c(0, 1000, 10000, 50000)`.
#' @param subdataset Index or search pattern of subdataset.
#'  Pattern is not accepted when product is \code{"VNP46A2"} or
#'  \code{"MOD06_L2"}
#' @param fun_summary character or function. Function to summarize
#'  extracted raster values.
#' @param nthreads integer(1). Number of threads to be used
#'  to calculate covariates.
#' @param package_list_add character. A vector with package names to load
#'  these in each thread. Note that \code{sf}, \code{terra},
#'  \code{exactextractr}, \code{doParallel}, \code{parallelly},
#'  and \code{dplyr}
#'  are the default packages to be loaded.
#' @param export_list_add character. A vector with object names to export
#'  to each thread. It should be minimized to spare memory.
#' @description sites should be sf object as it is exportable to
#' parallel workers.
#' @note See details for setting parallelization
#' \code{\link[foreach]{foreach}},
#' \code{\link[parallelly]{makeClusterPSOCK}},
#' \code{\link[parallelly]{availableCores}},
#' \code{\link[doParallel]{registerDoParallel}}
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom methods is
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @importFrom terra nlyr
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom future plan
#' @importFrom future cluster
#' @importFrom parallelly availableWorkers
#' @importFrom doParallel registerDoParallel
#' @export
calc_modis <-
  function(
    path,
    product = c("MOD11A1", "MOD13A2", "MOD06_L2",
                "VNP46A2", "MOD09GA", "MCD19A2"),
    sites,
    id_col = "site_id",
    name_covariates,
    radius = c(0L, 1e3L, 1e4L, 5e4L),
    subdataset = NULL,
    fun_summary = "mean",
    nthreads = floor(length(parallelly::availableWorkers()) / 2),
    package_list_add = NULL,
    export_list_add = NULL
  ) {
    product <- match.arg(product)
    dates_available <-
      regmatches(path, regexpr("A20\\d{2,2}[0-3]\\d{2,2}", path))
    dates_available <- unique(dates_available)
    dates_available <- sub("A", "", dates_available)

    sites_input <- try(sf::st_as_sf(sites), silent = TRUE)
    if (inherits(sites_input, "try-error")) {
      stop("sites cannot be convertible to sf.
      Please convert sites into a sf object to proceed.\n")
    }

    export_list <- c()
    package_list <-
      c("sf", "terra", "exactextractr", "foreach", "data.table", "stars",
        "dplyr", "parallelly", "doParallel")
    if (!is.null(export_list_add)) {
      export_list <- append(export_list, export_list_add)
    }
    if (!is.null(package_list_add)) {
      package_list <- append(package_list, package_list_add)
    }

    # make clusters
    doParallel::registerDoParallel(cores = nthreads)
    future::future(future::cluster, workers = nthreads)

    datei <- NULL
    calc_results <-
      foreach::foreach(
        datei = seq_along(dates_available),
        .packages = package_list,
        .export = export_list,
        .combine = dplyr::bind_rows,
        .errorhandling = "pass",
        .verbose = TRUE
      ) %dopar% {
        options(sf_use_s2 = FALSE)
        # nolint start
        day_to_pick <- dates_available[datei]
        # nolint end
        day_to_pick <- as.Date(day_to_pick, format = "%Y%j")

        radiusindex <- seq_along(radius)
        radiuslist <- split(radiusindex, radiusindex)

        # VNP46 corner assignment
        if (product == "VNP46A2") {
          vrt_today <-
            modis_preprocess_vnp46(
              paths = path,
              date_in = day_to_pick,
              # upper level subdataset is ignored
              subdataset = 3L,
              crs_ref = "EPSG:4326"
            )
        } else if (product == "MOD06_L2") {
          vrt_today <-
            modis_mosaic_mod06(
                               paths = path,
                               date_in = day_to_pick)
        } else {
          vrt_today <-
            modis_get_vrt(
                          paths = path,
                          regex_sds = subdataset,
                          product = product,
                          date_in = day_to_pick)
        }
        if (terra::nlyr(vrt_today) != length(name_covariates)) {
          warning("The number of layers in the input raster do not match
                  the length of name_covariates.\n")
        }

        res0 <-
          lapply(radiuslist,
            function(k) {
              name_radius <-
                sprintf("%s%05d",
                        name_covariates,
                        radius[k])

              tryCatch({
                extracted <-
                  modis_worker(
                    raster = vrt_today,
                    date = as.character(day_to_pick),
                    sites_in = sites_input,
                    product = product,
                    fun_summary_raster = fun_summary,
                    name_extracted = name_radius,
                    id_col = id_col,
                    radius = radius[k]
                  )
                return(extracted)
              }, error = function(e) {
                name_radius <-
                  sprintf("%s%05d",
                          name_covariates,
                          radius[k])
                error_df <- sf::st_drop_geometry(sites_input)
                if (!"time" %in% names(error_df)) {
                  error_df$time <- day_to_pick
                }
                # coerce to avoid errors
                error_df <- as.data.frame(error_df)
                error_df <- error_df[, c(id_col, "time")]
                error_df[, name_radius] <- -99999
                return(error_df)
              }
              )
            }
          )
        res <-
          Reduce(\(x, y) {
            dplyr::left_join(x, y,
              by = c("site_id", "time")
            )
          },
          res0)
        return(res)
      }
    Sys.sleep(1L)
    return(calc_results)
  }


#' Calculate temporal dummy variables
#' @param sites data.frame with a temporal field named `"time"`
#'  see \code{\link{convert_stobj_to_stdt}}
#' @param id_col character(1). Unique site identifier column name.
#'  Default is `"site_id"`.
#' @param domain_year integer. Year domain to dummify.
#'  Default is \code{seq(2018L, 2022L)}
#' @returns data.frame with year, month, and weekday indicators.
#' @author Insang Song
#' @importFrom methods is
#' @importFrom data.table year
#' @importFrom data.table month
#' @importFrom data.table as.data.table
#' @export
calc_temporal_dummies <-
  function(
    sites,
    id_col = "site_id",
    domain_year = seq(2018L, 2022L)
  ) {
    if (!methods::is(sites, "data.frame")) {
      stop("Argument sites is not a data.frame.\n")
    }
    if (!"time" %in% names(sites)) {
      stop("A mandatory field 'time' does not exist in sites.\n")
    }
    id_col <- id_col
    dummify <- function(vec, domain) {
      vec_unique <- domain
      vec_split <- split(vec_unique, vec_unique)
      vec_assigned <-
        lapply(vec_split,
               function(x) {
                 as.integer(vec == x)
               })
      dt_dum <- Reduce(cbind, vec_assigned)
      dt_dum <- data.table::as.data.table(dt_dum)
      return(dt_dum)
    }

    # year
    vec_year <- data.table::year(sites$time)
    dt_year_dum <- dummify(vec_year, domain_year)
    # should the last year be the present year or 2022?
    colnames(dt_year_dum) <-
      sprintf("DUM_Y%d_0_00000", domain_year)

    # month
    vec_month <- data.table::month(sites$time)
    dt_month_dum <- dummify(vec_month, seq(1L, 12L))
    shortmn <-
      c("JANUA", "FEBRU", "MARCH", "APRIL",
        "MAYMA", "JUNEJ", "JULYJ", "AUGUS",
        "SEPTE", "OCTOB", "NOVEM", "DECEM")
    colnames(dt_month_dum) <-
      sprintf("DUM_%s_0_00000", shortmn)

    # weekday (starts from 1-Monday)
    vec_wday <- as.POSIXlt(sites$time)$wday
    dt_wday_dum <- dummify(vec_wday, seq(1L, 7L))
    colnames(dt_wday_dum) <-
      sprintf("DUM_WKDY%d_0_00000", seq(1L, 7L))

    # column binding
    sites_dums <-
      cbind(
        sites,
        dt_year_dum,
        dt_month_dum,
        dt_wday_dum
      )

    return(sites_dums)
  }

# nocov start
#' Calculate TRI covariates
#' @param path character(1). Path to the directory with TRI CSV files
#' @param sites stdt/sf/SpatVector/data.frame. Unique sites
#' see \code{\link{convert_stobj_to_stdt}}
#' @param id_col character(1). Unique site identifier column name.
#'  Default is `"site_id"`.
#' @param domain_year integer. Year domain to dummify.
#'  Default is \code{seq(2018L, 2022L)}
#' @param radius Circular buffer radius.
#' Default is \code{c(1000, 10000, 50000)} (meters)
#' @param sites_epsg character(1). Coordinate system of sites.
#' @author Insang Song
#' @returns A data.frame object.
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom methods is
#' @importFrom data.table fread
#' @importFrom data.table rbindlist
#' @export
calc_tri <- function(
  path = "./input/tri/",
  sites,
  id_col = "site_id",
  domain_year = seq(2018L, 2022L),
  radius = c(1e3L, 1e4L, 5e4L),
  sites_epsg = "EPSG:4326"
) {
  if (is_stdt(sites)) {
    sites <- sites$stdt
    sites_epsg <- sites$crs_dt
  } else {
    if (!all(c("lon", "lat", "time") %in% colnames(sites))) {
      stop("sites should be stdt or 
      have 'lon', 'lat', and 'time' fields.\n")
    }
    sites_epsg <- terra::crs(sites)
    if (!methods::is(sites, "SpatVector")) {
      if (methods::is(sites, "sf")) {
        sites <- terra::vect(sites)
      }
      if (is.data.frame(sites)) {
        sites <-
          terra::vect(sites,
                      geom = c("lon", "lat"),
                      crs = sites_epsg)
      }
    }
  }
  if (!is.numeric(radius)) {
    stop("radius should be numeric.\n")
  }

  csvs_tri <- list.files(path = path, pattern = "*.csv$", full.names = TRUE)
  col_sel <- c(1, 13, 12, 34, 41, 42, 43, 45, 46, 48, 47, 104)
  csvs_tri <- lapply(csvs_tri, read.csv)
  csvs_tri <- lapply(csvs_tri, function(x) x[, col_sel])
  csvs_tri <- data.table::rbindlist(csvs_tri)
  # column name readjustment

  # depending on the way the chemicals are summarized
  # ... csvs are aggregated...
  csvs_tri_x <-
    data.table::dcast(csvs_tri, YEAR + LONGITUDE + LATITUDE ~ .)
  spvect_tri <-
    terra::vect(csvs_tri_x,
                geom = c("LONGITUDE", "LATITUDE"),
                crs = "EPSG:4326",
                keepgeom = TRUE)
  sites_re <- terra::project(sites, terra::crs(spvect_tri))

  list_buffer <- split(radius, radius)
  list_buffer <-
    lapply(list_buffer,
           function(x) {
             xx <- terra::nearby(sites_re, spvect_tri, distance = x)
             xx$buffer <- x
             xx[, lapply(.SD, sum, na.rm = TRUE),
                by = c("YEAR", "LONGITUDE", "LATITUDE", "buffer")]
           })
  df_tri <- data.table::rbindlist(list_buffer)

  return(df_tri)
}
# nocov end


#' Calculate National Emission Inventory (NEI) covariates
#' @description NEI data comprises multiple csv files where emissions of
#' 50+ pollutants are recorded at county level. With raw data files,
#' this function will join a combined table of NEI data and county
#' boundary, then perform a spatial join to the sites.
#' @param path character(1). Path to the directory with NEI CSV files
#' @param sites stdt/sf/SpatVector/data.frame. Unique sites.
#' See [`convert_stobj_to_stdt`] for details of `stdt`
#' @param id_col character(1). Unique site identifier column name.
#'  Default is `"site_id"`. It is no more than a placeholder
#' in this function
#' @param year integer(1). Data year.
#'  Currently only accepts `c(2017, 2020)`
#' @param county_shp character(1). Path to county boundary file.
#' @param sites_epsg character(1). Coordinate system of sites.
#' @author Insang Song, Ranadeep Daw
#' @returns A data.frame object.
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom methods is
#' @importFrom data.table .SD
#' @importFrom data.table fread
#' @importFrom data.table rbindlist
#' @export
calc_nei <- function(
  path = "./input/nei/",
  sites,
  id_col = "site_id",
  year = 2017,
  county_shp = NULL,
  sites_epsg = "EPSG:4326"
) {
  if (is_stdt(sites)) {
    # sites <- sites$stdt
    # sites_epsg <- sites$crs_stdt
    sites <- convert_stdt_spatvect(sites)
  } else {
    if (!all(c("lon", "lat", "time") %in% colnames(sites))) {
      stop("sites should be stdt or 
      have 'lon', 'lat', and 'time' fields.\n")
    }
    if (!methods::is(sites, "SpatVector")) {
      if (methods::is(sites, "sf")) {
        sites <- terra::vect(sites)
      }
      if (is.data.frame(sites)) {
        sites <-
          terra::vect(sites,
            geom = c("lon", "lat"),
            keepgeom = TRUE,
            crs = sites_epsg
          )
      }
    }
  }
  if (is.null(county_shp)) {
    stop("county_shp should be provided. Put the right path to the
    county boundary file.")
  }
  if (!year %in% c(2017, 2020)) {
    stop("year should be one of 2017 or 2020.\n")
  }

  # Concatenate NEI csv files
  csvs_nei <- list.files(path = path, pattern = "*.csv$", full.names = TRUE)
  csvs_nei <- lapply(csvs_nei, data.table::fread)
  csvs_nei <- data.table::rbindlist(csvs_nei)

  # column name readjustment
  target_nm <- c("fips code", "total emissions", "emissions uom")
  # not grep-ping at once for flexibility
  target_cns <- sapply(target_nm, function(x) grep(x, colnames(csvs_nei)))
  colnames(csvs_nei)[target_cns] <-
    c("geoid", "emissions_total", "unit_measurement")

  # unify unit of measurement
  # TON here is short tonne, which is 2000 lbs.
  csvs_nei$emissions_total_ton <-
    ifelse(
      csvs_nei$unit_measurement == "TON",
      csvs_nei$emissions_total,
      csvs_nei$emissions_total / 2000
    )
  emissions_total_ton <- NULL
  geoid <- NULL
  yearabbr <- substr(year, 3, 4)
  csvs_nei$geoid <- sprintf("%05d", as.integer(csvs_nei$geoid))
  csvs_nei <-
    csvs_nei[, list(
      TRF_NEINP_0_00000 = sum(emissions_total_ton, na.rm = TRUE)
    ),
    by = geoid]
  csvs_nei$Year <- year

  # read county vector
  cnty_vect <- county_shp
  if (is.character(cnty_vect)) {
    cnty_vect <- terra::vect(cnty_vect)
  }
  cnty_geoid_guess <- grep("GEOID", names(cnty_vect))
  names(cnty_vect)[cnty_geoid_guess] <- "geoid"
  cnty_vect$geoid <- sprintf("%05d", as.integer(cnty_vect$geoid))
  cnty_vect <- merge(cnty_vect, csvs_nei, by = "geoid")
  cnty_vect <- cnty_vect[, c("geoid", "Year", "TRF_NEINP_0_00000")]
  names(cnty_vect)[3] <- sub("NP", yearabbr, names(cnty_vect)[3])

  # spatial join
  sites_re <- terra::project(sites, terra::crs(cnty_vect))
  sites_re <- terra::intersect(sites_re, cnty_vect)

  return(sites_re)
}
