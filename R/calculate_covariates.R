#' Calculate covariates

# nocov start
# nolint start
#' @param covariate character(1). Covariate type.
#' @param from character. Single or multiple from strings.
#' @param locs sf/SpatVector. Unique locs. Should include
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
      from,
      locs,
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
          from = from,
          locs = locs,
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
#' @param from character(1). from to Koppen-Geiger
#'  climate zone raster file
#' @param locs sf/SpatVector. Unique locs. Should include
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
# locs (locs), from (from), locs_id (id_col), variables
calc_koppen_geiger <-
  function(
      locs = NULL,
      from = NULL,
      locs_id = "site_id",
      variables = NULL) {
    ## You will get "locs" in memory after sourcing the file above
    locs_tr <- locs

    if (!methods::is(locs, "SpatVector")) {
      locs_tr <- terra::vect(locs)
    }
    locs_kg <- terra::project(locs_tr, terra::crs(from))
    locs_kg_extract <- terra::extract(from, locs_kg)

    # The starting value is NA as the color table has 0 value in it
    kg_class <-
      c(
        NA, "Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", "Csa", "Csb",
        "Csc", "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc",
        "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
        "Dfa", "Dfb", "Dfc", "Dfd", "ET", "EF"
      )
    kg_coltab <- terra::coltab(from)
    kg_coltab <- kg_coltab[[1]][seq(1, 31), ]
    kg_colclass <- data.frame(
      value = kg_coltab$value,
      class_kg = kg_class
    )

    locs_kg_extract[[locs_id]] <- unlist(locs_kg[[locs_id]])
    colnames(locs_kg_extract)[2] <- "value"
    locs_kg_extract_e <- merge(locs_kg_extract, kg_colclass, by = "value")

    # "Dfa": 25
    # "BSh": 6
    # "Dfb": 26
    id_search <- unlist(locs_kg_extract_e[[locs_id]])
    # errorfix: how to generalize and auto-fix it?
    locs_kg_extract_e[
      which(id_search == "44009000788101"),
      "class_kg"
    ] <- "Dfa"
    locs_kg_extract_e[
      which(id_search == "48061200488101"),
      "class_kg"
    ] <- "BSh"
    locs_kg_extract_e[
      which(id_search == "33015001488101"),
      "class_kg"
    ] <- "Dfb"

    locs_kg_extract_e$class_kg <-
      as.factor(substr(locs_kg_extract_e$class_kg, 1, 1))
    # currently there are no "E" region in locs.
    # however, E is filled with all zeros at the moment.
    aelabels <- LETTERS[1:5]
    df_ae_separated <-
      split(aelabels, aelabels) |>
      lapply(function(x) {
        as.integer(locs_kg_extract_e$class_kg == x)
      }) |>
      Reduce(f = cbind, x = _) |>
      as.data.frame()
    colnames(df_ae_separated) <- sprintf("DUM_CLRG%s_0_00000", aelabels)

    kg_extracted <-
      cbind(
        locs_id = unlist(locs_kg_extract_e[[locs_id]]),
        df_ae_separated
      )
    return(kg_extracted)
  }


#' Compute land cover classes ratio in circle buffers around points
#'
#' @param locs terra::SpatVector of points geometry
#' @param from SpatRaster of NLCD
#' @param locs_id character(1). Unique identifier of locations
#' @param radius numeric (non-negative) giving the
#' radius of buffer around points
#' @param year numeric giving the year of NLCD data used
#' @param variables character. Variable names to extract. Ignored.
#' @importFrom utils read.csv
#' @importFrom methods is
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
#' @export
calc_nlcd_ratio <- function(locs,
                            from,
                            locs_id,
                            radius = 1000,
                            variables = NULL) {
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
  if (!methods::is(locs, "SpatVector")) {
    stop("locs is not a terra::SpatVector.")
  }
  if (!methods::is(from, "SpatRaster")) {
    stop("from is not a SpatRaster.")
  }

  # select points within mainland US and reproject on nlcd crs if necessary
  us_main <-
    terra::ext(c(xmin = -127, xmax = -65, ymin = 24, ymax = 51)) |>
    terra::vect() |>
    terra::set.crs("EPSG:4326") |>
    terra::project(y = terra::crs(locs))
  data_vect_b <- locs |>
    terra::intersect(x = us_main)
  if (!terra::same.crs(data_vect_b, from)) {
    data_vect_b <- terra::project(data_vect_b, terra::crs(from))
  }
  # create circle buffers with buf_radius
  bufs_pol <- terra::buffer(data_vect_b, width = radius) |>
    sf::st_as_sf()
  # ratio of each nlcd class per buffer
  nlcd_at_bufs <- exactextractr::exact_extract(from,
                                               sf::st_geometry(bufs_pol),
                                               fun = "frac",
                                               stack_apply = TRUE,
                                               progress = FALSE)
  # select only the columns of interest
  nlcd_at_bufs <- nlcd_at_bufs[names(nlcd_at_bufs)[grepl("frac_",
                                                         names(nlcd_at_bufs))]]
  # change column names
  cfpath <- system.file("extdata", "nlcd_classes.csv", package = "amadeus")
  nlcd_classes <- utils::read.csv(cfpath)
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
  new_data_vect <- terra::project(new_data_vect, terra::crs(locs))
  return(new_data_vect)
}


#' Calculate EPA Ecoregions level 2/3 binary variables
#' @param locs sf/SpatVector. Unique locs. Should include
#'  a unique identifier field named \code{locs_id}
#' @param from SpatVector. Ecoregion polygons
#' @param locs_id character(1). Name of unique identifier.
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
    locs,
    from = NULL, 
    locs_id = "site_id"
  ) {

    if (!methods::is(locs, "SpatVector")) {
      locs <- terra::vect(locs)
    }

    locs <- terra::project(locs, terra::crs(from))

    locs_in <- terra::intersect(locs, from)
    locs_out <-
      locs[!unlist(locs[[locs_id]]) %in% unlist(locs_in[[locs_id]]), ]

    locs_snapped <- terra::snap(locs_out, from, tolerance = 50)
    locs_fixed <- rbind(locs_in, locs_snapped)
    extracted <- terra::extract(from, locs_fixed)

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

    locs_ecoreg <- cbind(locs[[locs_id]], df_lv2, df_lv3)
    attr(locs_ecoreg, "ecoregion2_code") <- sort(unique(from$L2_KEY))
    attr(locs_ecoreg, "ecoregion3_code") <- sort(unique(from$L3_KEY))
    return(locs_ecoreg)
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
#' @param locs SpatVector/sf/sftime object. Locations where MODIS values
#' are summarized..
#' @param from SpatRaster. Preprocessed objects.
#' @param date Date(1). date to query.
#' @param name_extracted character. Names of calculated covariates.
#' @param fun_summary function. Summary function for
#' multilayer rasters. Passed to \code{foo}. See also
#' \code{\link[exactextractr]{exact_extract}}
#' @param locs_id character(1). Field name where unique site identifiers
#' are stored. Default is `"site_id"`
#' @param radius numeric. Radius to generate circular buffers.
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
  locs = NULL,
  from = NULL,
  date = NULL,
  name_extracted = NULL,
  fun_summary = "mean",
  locs_id = "site_id",
  radius = 0L
) {
  if (!any(methods::is(locs_in, "SpatVector"),
           methods::is(locs_in, "sf"),
           methods::is(locs_in, "sftime"),
           is_stdt(locs_in))) {
    stop("locs_in should be one of sf, sftime, stdt, or SpatVector.\n")
  }
  if (is_stdt(locs_in)) {
    locs_in <- convert_stdt_spatvect(locs_in)
  }
  if (!methods::is(locs_in, "SpatVector")) {
    locs_in <- terra::vect(locs_in)
  }
  if (!locs_id %in% names(locs_in)) {
    stop(sprintf("locs should include columns named %s.\n",
                 locs_id)
    )
  }
  if (!"time" %in% names(locs_in)) {
    locs_in$time <- date
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

  ## NaN to zero
  from[is.nan(from)] <- 0L

  # raster used to be vrt_today
  if (any(grepl("00000", name_extracted))) {
    locs_tr <- terra::project(locs_in, terra::crs(from))
    extracted <- terra::extract(x = from, y = locs_tr, ID = FALSE)
    locs_blank <- as.data.frame(locs_in)
    extracted <- cbind(locs_blank, extracted)
  } else {
    extracted <-
      extract_with_buffer(
        points = locs,
        surf = from,
        id = locs_id,
        radius = radius,
        func = fun_summary
      )
  }
  # cleaning names
  # assuming that extracted is a data.frame
  #extracted$time <- date
  name_offset <- terra::nlyr(from)
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
#' @param from character. List of HDF files.
#' @param product character(1). MODIS product. Should be one of
#' \code{c("MOD11A1", "MOD13A2", "MOD06_L2", "VNP46A2", "MOD09GA", "MCD19A2")}
#' @param locs sf object. Unique locs where covariates
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
#' @description locs should be sf object as it is exportable to
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
    from,
    product = c("MOD11A1", "MOD13A2", "MOD06_L2",
                "VNP46A2", "MOD09GA", "MCD19A2"),
    locs,
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
      regmatches(from, regexpr("A20\\d{2,2}[0-3]\\d{2,2}", from))
    dates_available <- unique(dates_available)
    dates_available <- sub("A", "", dates_available)

    locs_input <- try(sf::st_as_sf(locs), silent = TRUE)
    if (inherits(locs_input, "try-error")) {
      stop("locs cannot be convertible to sf.
      Please convert locs into a sf object to proceed.\n")
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
              froms = from,
              date_in = day_to_pick,
              # upper level subdataset is ignored
              subdataset = 3L,
              crs_ref = "EPSG:4326"
            )
        } else if (product == "MOD06_L2") {
          vrt_today <-
            modis_mosaic_mod06(
                               froms = from,
                               date_in = day_to_pick)
        } else {
          vrt_today <-
            modis_get_vrt(
                          froms = from,
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
                    locs_in = locs_input,
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
                error_df <- sf::st_drop_geometry(locs_input)
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
#' @param locs data.frame with a temporal field named `"time"`
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
    locs,
    id_col = "site_id",
    domain_year = seq(2018L, 2022L)
  ) {
    if (!methods::is(locs, "data.frame")) {
      stop("Argument locs is not a data.frame.\n")
    }
    if (!"time" %in% names(locs)) {
      stop("A mandatory field 'time' does not exist in locs.\n")
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
    vec_year <- data.table::year(locs$time)
    dt_year_dum <- dummify(vec_year, domain_year)
    # should the last year be the present year or 2022?
    colnames(dt_year_dum) <-
      sprintf("DUM_Y%d_0_00000", domain_year)

    # month
    vec_month <- data.table::month(locs$time)
    dt_month_dum <- dummify(vec_month, seq(1L, 12L))
    shortmn <-
      c("JANUA", "FEBRU", "MARCH", "APRIL",
        "MAYMA", "JUNEJ", "JULYJ", "AUGUS",
        "SEPTE", "OCTOB", "NOVEM", "DECEM")
    colnames(dt_month_dum) <-
      sprintf("DUM_%s_0_00000", shortmn)

    # weekday (starts from 1-Monday)
    vec_wday <- as.POSIXlt(locs$time)$wday
    dt_wday_dum <- dummify(vec_wday, seq(1L, 7L))
    colnames(dt_wday_dum) <-
      sprintf("DUM_WKDY%d_0_00000", seq(1L, 7L))

    # column binding
    locs_dums <-
      cbind(
        locs,
        dt_year_dum,
        dt_month_dum,
        dt_wday_dum
      )

    return(locs_dums)
  }


#' Calculate TRI covariates
#' @param from character(1). from to the directory with TRI CSV files
#' @param locs stdt/sf/SpatVector/data.frame. Unique locs
#' see \code{\link{convert_stobj_to_stdt}}
#' @param id_col character(1). Unique site identifier column name.
#'  Default is `"site_id"`.
#' @param domain_year integer. Year domain to dummify.
#'  Default is \code{seq(2018L, 2022L)}
#' @param radius Circular buffer radius.
#' Default is \code{c(1000, 10000, 50000)} (meters)
#' @param locs_epsg character(1). Coordinate system of locs.
#' @author Insang Song, Mariana Kassien
#' @returns A data.frame object.
#' @note U.S. context.
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom terra nearby
#' @importFrom methods is
#' @importFrom data.table .SD
#' @importFrom utils read.csv
#' @importFrom data.table rbindlist
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr ends_with
#' @importFrom dplyr all_of
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarize
#' @importFrom tidyr pivot_wider
#' @export
calc_tri <- function(
  from = "./input/tri/",
  locs,
  id_col = "site_id",
  domain_year = seq(2018L, 2022L),
  radius = c(1e3L, 1e4L, 5e4L),
  locs_epsg = "EPSG:4326"
) {
  if (is_stdt(locs)) {
    locs <- locs$stdt
    locs_epsg <- locs$crs_dt
  } else {
    if (!all(c("lon", "lat", "time") %in% names(locs))) {
      stop("locs should be stdt or 
      have 'lon', 'lat', and 'time' fields.\n")
    }
    locs_epsg <- terra::crs(locs)
    if (!methods::is(locs, "SpatVector")) {
      if (methods::is(locs, "sf")) {
        locs <- terra::vect(locs)
      }
      if (is.data.frame(locs)) {
        locs <-
          terra::vect(locs,
                      geom = c("lon", "lat"),
                      keepgeom = TRUE,
                      crs = locs_epsg)
      }
    }
  }
  if (!is.numeric(radius)) {
    stop("radius should be numeric.\n")
  }

  csvs_tri_from <-
    list.files(from = from, pattern = "*.csv$", full.names = TRUE)
  csvs_tri <- lapply(csvs_tri_from, read.csv)
  col_sel <- c(1, 13, 12, 14, 20, 34, 36, 47, 48, 49)
  csvs_tri <- data.table::rbindlist(csvs_tri)
  dt_tri <- csvs_tri[, col_sel, with = FALSE]

  # column name readjustment
  tri_cns <- colnames(dt_tri)
  tri_cns <- sub(".*?\\.\\.", "", tri_cns)
  tri_cns <- sub("^[^A-Za-z]*", "", tri_cns)
  tri_cns <- gsub("\\.", "_", tri_cns)
  dt_tri <- setNames(dt_tri, tri_cns)

  # depending on the way the chemicals are summarized
  # Unit is kilogram
  # nolint start
  dt_tri_x <-
    dt_tri |>
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_AIR"),
        ~ifelse(UNIT_OF_MEASURE == "Pounds", . / 453.592 / 1e3, . / 1e3)
      )
    ) |>
    dplyr::group_by(YEAR, LONGITUDE, LATITUDE, TRI_CHEMICAL_COMPOUND_ID) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::ends_with("_AIR"),
        ~sum(., na.rm = TRUE)
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      values_from = c("FUGITIVE_AIR", "STACK_AIR"),
      names_from = "TRI_CHEMICAL_COMPOUND_ID",
      names_sep = "_"
    ) |>
    dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE))
    #  |>
    # dplyr::filter(LONGITUDE > -130 & LONGITUDE < -60) |>
    # dplyr::filter(LATITUDE > 20 & LATITUDE < 52)

  spvect_tri <-
    terra::vect(dt_tri_x,
                geom = c("LONGITUDE", "LATITUDE"),
                crs = "EPSG:4269", # all are NAD83
                keepgeom = TRUE)
  # spvect_tri$to_id <- seq(1, nrow(spvect_tri))
  locs_re <- terra::project(locs, terra::crs(spvect_tri))

  YEAR <- NULL
  from_id <- NULL
  buffer <- NULL

  # strategy: year-buffer
  # split by year: locs and tri locations
  list_locs <- split(locs_re, unlist(locs_re[["time"]]))
  list_tri <- split(spvect_tri, unlist(spvect_tri[["YEAR"]]))

  # mapply and inner lapply
  list_locs_tri <-
    mapply(
      function(sts, tri) {
        list_buffer <- split(radius, radius)
        list_buffer <-
          lapply(list_buffer,
                function(x) {
                  tri_df <- as.data.frame(tri)
                  locs_tri_near <-
                    terra::nearby(sts, tri, distance = x)
                  locs_tri_near <- as.data.frame(locs_tri_near)
                  locs_tri_near$from_id <-
                    unlist(sts[[id_col]])[locs_tri_near$from_id]
                  locs_tri_near_e <-
                    cbind(locs_tri_near, tri_df[locs_tri_near$to_id, ])
                  locs_tri_near_e$buffer <- x
                  print(names(locs_tri_near_e))
                  locs_tri_s <- locs_tri_near_e |>
                    as.data.frame() |>
                    dplyr::group_by(YEAR, from_id, buffer) |>
                    dplyr::summarize(
                      dplyr::across(
                        dplyr::all_of(dplyr::contains("_AIR")),
                        ~sum(., na.rm = TRUE)
                      )
                    ) |>
                    dplyr::ungroup()
                  return(locs_tri_s)
                })
        df_buffer <- data.table::rbindlist(list_buffer)
        return(df_buffer)
      },
      list_locs, list_tri, SIMPLIFY = FALSE
    ) 

  df_tri <- data.table::rbindlist(list_locs_tri)

  return(df_tri)
}
# nolint end

#' Calculate National Emission Inventory (NEI) covariates
#' @description NEI data comprises multiple csv files where emissions of
#' 50+ pollutants are recorded at county level. With raw data files,
#' this function will join a combined table of NEI data and county
#' boundary, then perform a spatial join to the locs.
#' @param from character(1). from to the directory with NEI CSV files
#' @param locs stdt/sf/SpatVector/data.frame. Unique locs.
#' See [`convert_stobj_to_stdt`] for details of `stdt`
#' @param id_col character(1). Unique site identifier column name.
#'  Default is `"site_id"`. It is no more than a placeholder
#' in this function
#' @param year integer(1). Data year.
#'  Currently only accepts `c(2017, 2020)`
#' @param county_shp character(1). from to county boundary file.
#' @param locs_epsg character(1). Coordinate system of locs.
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
  from = "./input/nei/",
  locs,
  id_col = "site_id",
  year = 2017,
  county_shp = NULL,
  locs_epsg = "EPSG:4326"
) {
  if (is_stdt(locs)) {
    # locs <- locs$stdt
    # locs_epsg <- locs$crs_stdt
    locs <- convert_stdt_spatvect(locs)
  } else {
    if (!all(c("lon", "lat", "time") %in% colnames(locs))) {
      stop("locs should be stdt or 
      have 'lon', 'lat', and 'time' fields.\n")
    }
    if (!methods::is(locs, "SpatVector")) {
      if (methods::is(locs, "sf")) {
        locs <- terra::vect(locs)
      }
      if (is.data.frame(locs)) {
        locs <-
          terra::vect(locs,
            geom = c("lon", "lat"),
            keepgeom = TRUE,
            crs = locs_epsg
          )
      }
    }
  }
  if (is.null(county_shp)) {
    stop("county_shp should be provided. Put the right from to the
    county boundary file.")
  }
  if (!year %in% c(2017, 2020)) {
    stop("year should be one of 2017 or 2020.\n")
  }

  # Concatenate NEI csv files
  csvs_nei <- list.files(from = from, pattern = "*.csv$", full.names = TRUE)
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
  locs_re <- terra::project(locs, terra::crs(cnty_vect))
  locs_re <- terra::intersect(locs_re, cnty_vect)

  return(locs_re)
}
