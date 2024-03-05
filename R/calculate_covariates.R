# nocov start
# nolint start
#' Calculate covariates wrapper function
#' @description
#' The \code{calculate_covariates()} function extracts values at point
#' locations from a SpatRaster or SpatVector object returned from
#' \code{process_covariates()}. \code{calculate_covariates()} and the underlying
#' source-specific covariate functions have been designed to operate on the
#' processed objects. To avoid errors, \strong{do not edit the processed
#' SpatRaster or SpatVector objects before passing to
#' \code{calculate_covariates()}}.
#' @param covariate character(1). Covariate type.
#' @param from character. Single or multiple from strings.
#' @param locs sf/SpatVector. Unique locations. Should include
#'  a unique identifier field named `locs_id`
#' @param locs_id character(1). Name of unique identifier.
#'  Default is `"site_id"`.
#' @param ... Arguments passed to each covariate calculation
#'  function.
#' @seealso
#' - `calc_modis`: `"modis"`, `"MODIS"`
#' - `calc_koppen_geiger`: `"koppen-geiger"`, `"koeppen-geiger"`, `"koppen"`,
#' - `calc_ecoregion`: `"ecoregion"`, `"ecoregions"`
#' - `calc_temporal_dummies`: `"dummies"`
#' - `calc_hms`: `"hms"`, `"noaa"`, `"smoke"`
#' - `calc_gmted`: `"gmted"`
#' - `calc_narr`: `"narr"`, `"narr_monolevel"`, `"narr_p_levels`",
#' `"plevels"`, `"monolevel"`, `"p_levels"`
#' - `calc_geos`: `"geos"`, `"geos_cf"`
#' - `calc_sedac_population`: `"population"`, `"sedac_population"`
#' - `calc_sedac_groads`: `"roads"`
#' - `calc_nlcd`: `"nlcd"`
#' @returns Calculated covariates. Mainly data.frame object.
#' @author Insang Song
#' @export
# nolint end
calc_covariates <-
  function(
      covariate = c("modis", "koppen-geiger",
                    "koeppen-geiger", "koppen", "koeppen",
                    "geos", "dummies", "gmted", "roads",
                    "ecoregions", "ecoregion", "hms", "noaa", "smoke",
                    "gmted", "narr", "narr_monolevel", "narr_p_levels",
                    "plevels", "monolevel", "p_levels", "geos",
                    "sedac_population", "population", "nlcd"),
      from,
      locs,
      locs_id = "site_id",
      ...) {

    covariate <- tolower(covariate)
    covariate <- match.arg(covariate)
    if (startsWith(covariate, "ko")) {
      covariate <- "koppen"
    }

    # select function to run
    what_to_run <- switch(covariate,
      modis = calc_modis_par,
      ecoregion = calc_ecoregion,
      ecoregions = calc_ecoregion,
      koppen = calc_koppen_geiger,
      narr = calc_narr,
      narr_monolevel = calc_narr,
      monolevel = calc_narr,
      narr_p_levels = calc_narr,
      p_levels = calc_narr,
      plevels = calc_narr,
      nlcd = calc_nlcd,
      noaa = calc_hms,
      smoke = calc_hms,
      hms = calc_hms,
      sedac_groads = calc_sedac_groads,
      roads = calc_sedac_groads,
      sedac_population = calc_sedac_population,
      population = calc_sedac_population,
      nei = calc_nei,
      tri = calc_tri,
      geos = calc_geos,
      gmted = calc_gmted,
      dummies = calc_temporal_dummies
    )

    res_covariate <-
      tryCatch({
        what_to_run(
          from = from,
          locs = locs,
          locs_id = locs_id,
          ...
        )
      }, error = function(e) {
        print(e)
        print(args(what_to_run))
        message(
          paste0(
            "Please refer to the argument list and the error message above ",
            "to rectify the error.\n"
          )
        )
        return(NULL)
      })

    return(res_covariate)
  }
# nocov end

#' Calculate climate classification covariates
#' @description
#' Extract climate classification values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id} and
#' binary (0 = point not in climate region; 1 = point in climate region)
#' variables for each climate classification region.
#' @param from SpatVector(1). Output of \code{process_koppen_geiger()}.
#' @param locs sf/SpatVector. Unique locs. Should include
#'  a unique identifier field named `locs_id`
#' @param locs_id character(1). Name of unique identifier.
#' @param ... Placeholders.
#' @seealso [process_koppen_geiger()]
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
      from = NULL,
      locs = NULL,
      locs_id = "site_id",
      ...) {
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


#' Calculate land cover covariates
#' @description
#' Compute ratio of land cover class in circle buffers around points. Returns
#' a \code{data.frame} object containing \code{locs_id}, longitude, latitude,
#' time (year), and computed ratio for each land cover class.
#' @param from SpatRaster(1). Output of \code{process_nlcd()}.
#' @param locs terra::SpatVector of points geometry
#' @param locs_id character(1). Unique identifier of locations
#' @param radius numeric (non-negative) giving the
#' radius of buffer around points
#' @param ... Placeholders.
#' @seealso [process_nlcd()]
#' @returns a data.frame object
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
#' @importFrom terra metags
#' @importFrom exactextractr exact_extract
#' @export
calc_nlcd <- function(from,
                      locs,
                      locs_id = "site_id",
                      radius = 1000,
                      ...) {
  # check inputs
  if (!is.numeric(radius)) {
    stop("radius is not a numeric.")
  }
  if (radius <= 0) {
    stop("radius has not a likely value.")
  }
  if (!methods::is(locs, "SpatVector")) {
    stop("locs is not a terra::SpatVector.")
  }
  if (!methods::is(from, "SpatRaster")) {
    stop("from is not a SpatRaster.")
  }
  year <- try(as.integer(terra::metags(from, name = "year")))
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
  cfpath <- system.file("extdata", "nlcd_classes.csv", package = "amadeus")
  nlcd_classes <- utils::read.csv(cfpath)
  nlcd_at_bufs <-
    nlcd_at_bufs[
      sort(names(nlcd_at_bufs)[
        grepl(paste0("frac_(", paste(nlcd_classes$value, collapse = "|"), ")"),
              names(nlcd_at_bufs))
      ])
    ]
  # change column names
  nlcd_names <- names(nlcd_at_bufs)
  nlcd_names <- sub(pattern = "frac_", replacement = "", x = nlcd_names)
  nlcd_names <- as.numeric(nlcd_names)
  nlcd_names <- nlcd_classes[nlcd_classes$value %in% nlcd_names, c("class")]
  new_names <- sapply(
    nlcd_names,
    function(x) {
      sprintf("LDU_%s_0_%05d", x, radius)
    }
  )
  names(nlcd_at_bufs) <- new_names
  # merge data_vect with nlcd class fractions (and reproject)
  new_data_vect <- cbind(data_vect_b, nlcd_at_bufs)
  new_data_vect <- terra::project(new_data_vect, terra::crs(locs))
  new_data_vect$time <- as.integer(year)
  return(new_data_vect)
}


#' Calculate ecoregions covariates
#' @description
#' Extract ecoregions covariates (U.S. EPA Ecoregions Level 2/3) at point
#' locations. Returns a \code{data.frame} object containing \code{locs_id} and
#' binary (0 = point not in ecoregion; 1 = point in ecoregion) variables for
#' each ecoregion.
#' @param from SpatVector(1). Output of \code{process_ecoregion()}.
#' @param locs sf/SpatVector. Unique locs. Should include
#'  a unique identifier field named `locs_id`
#' @param locs_id character(1). Name of unique identifier.
#' @param ... Placeholders.
#' @seealso [process_ecoregion()]
#' @returns a data.frame object with dummy variables and attributes of:
#'   - `attr(., "ecoregion2_code")`: Ecoregion lv.2 code and key
#'   - `attr(., "ecoregion3_code")`: Ecoregion lv.3 code and key
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
    from = NULL,
    locs,
    locs_id = "site_id",
    ...
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
#' @param from SpatRaster. Preprocessed objects.
#' @param locs SpatVector/sf/sftime object. Locations where MODIS values
#' are summarized.
#' @param locs_id character(1). Field name where unique site identifiers
#' are stored. Default is `"site_id"`
#' @param radius numeric. Radius to generate circular buffers.
#' @param date Date(1). date to query.
#' @param name_extracted character. Names of calculated covariates.
#' @param fun_summary function. Summary function for
#' multilayer rasters. Passed to `foo`. See [exactextractr::exact_extract]
#' for details.
#' @param ... Placeholders.
#' @description modis_worker operates at six MODIS/VIIRS products
#' (MOD11A1, MOD13A2, MOD06_L2, VNP46A2, MOD09GA, and MCD19A2)
#' on a daily basis. Given that the raw hdf files are downloaded from
#' NASA, standard file names include a data retrieval date flag starting
#' with A. Leveraging that piece of information, the function will select
#' files of scope on the date of interest. Please note that this function
#' does not provide a function to filter swaths or tiles, so it is strongly
#' recommended to check and pre-filter the file names at users' discretion.
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
calc_modis_daily <- function(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  radius = 0L,
  date = NULL,
  name_extracted = NULL,
  fun_summary = "mean",
  ...
) {
  if (!any(methods::is(locs, "SpatVector"),
           methods::is(locs, "sf"),
           methods::is(locs, "sftime"))) {
    stop("locs should be one of sf, sftime, or SpatVector.\n")
  }
  if (!methods::is(locs, "SpatVector")) {
    locs <- terra::vect(locs)
  }
  if (!locs_id %in% names(locs)) {
    stop(sprintf("locs should include columns named %s.\n",
                 locs_id)
    )
  }
  if (!"time" %in% names(locs)) {
    locs$time <- date
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

  ## NaN to zero
  from[is.nan(from)] <- 0L

  # raster used to be vrt_today
  if (any(grepl("00000", name_extracted))) {
    locs_tr <- terra::project(locs, terra::crs(from))
    extracted <- terra::extract(x = from, y = locs_tr, ID = FALSE)
    locs_blank <- as.data.frame(locs)
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
  name_offset <- terra::nlyr(from)
  # multiple columns will get proper names
  name_range <- seq(ncol(extracted) - name_offset + 1, ncol(extracted), 1)
  colnames(extracted)[name_range] <- name_extracted
  return(extracted)
}


#' Calculate MODIS product covariates in multiple CPU threads
#' @param from character. List of paths to MODIS/VIIRS files.
#' @param locs sf/SpatVector object. Unique locs where covariates
#' will be calculated.
#' @param locs_id character(1). Site identifier. Default is `"site_id"`
#' @param radius numeric. Radii to calculate covariates.
#' Default is `c(0, 1000, 10000, 50000)`.
#' @param preprocess function. Function to handle HDF files.
#' @param name_covariates character. Name header of covariates.
#' e.g., `"MOD_NDVIF_0_"`.
#' The calculated covariate names will have a form of
#' '{name_covariates}{zero-padded buffer radius in meters}',
#' e.g., 'MOD_NDVIF_0_50000' where 50 km radius circular buffer
#' was used to calculate mean NDVI value.
#' @param subdataset Index or search pattern of subdataset.
#' @param fun_summary character or function. Function to summarize
#'  extracted raster values.
#' @param nthreads integer(1). Number of threads to be used
#'  to calculate covariates.
#' @param package_list_add character. A vector with package names to load
#'  these in each thread. Note that `sf`, `terra`, `exactextractr`,
#' `doParallel`, `parallelly` and `dplyr` are the default packages to be
#' loaded.
#' @param export_list_add character. A vector with object names to export
#'  to each thread. It should be minimized to spare memory.
#' @param ... Arguments passed to `preprocess`.
#' @description `calc_modis_par` essentially runs [`calc_modis_daily`] function
#' in each thread (subprocess). Based on daily resolution, each day's workload
#' will be distributed to each thread. With `product` argument,
#' the files are processed by a customized function where the unique structure
#' and/or characteristics of the products are considered. `nthreads`
#' argument should be carefully selected in consideration of the machine's
#' CPU and memory capacities as products have their own memory pressure.
#' `locs` should be `sf` object as it is exportable to parallel workers.
#' @note Overall, this function and dependent routines assume that the file
#' system can handle concurrent access to the (network) disk by multiple
#' processes. File system characteristics, package versions, and hardware
#' settings and specification can affect the processing efficiency.
#' `locs` is expected to be convertible to `sf` object. `sf`, `SpatVector`, and
#' other class objects that could be converted to `sf` can be used.
#' Common arguments in `preprocess` functions such as `date` and `path` are
#' automatically detected and passed to the function. Please note that
#' `locs` here and `path` in `preprocess` functions are assumed to have a
#' standard naming convention of raw files from NASA.
#' @seealso See details for setting parallelization:
#' * [foreach::foreach]
#' * [parallelly::makeClusterPSOCK]
#' * [parallelly::availableCores]
#' * [doParallel::registerDoParallel]
#' Also, for `preprocess`, see:
#' * [process_covariates]
#' * [process_modis_merge]
#' * [process_modis_swath]
#' * [process_bluemarble]
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom methods is
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @importFrom terra nlyr
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom rlang inject
#' @importFrom future plan
#' @importFrom future cluster
#' @importFrom parallelly availableWorkers
#' @importFrom doParallel registerDoParallel
#' @export
calc_modis_par <-
  function(
    from = NULL,
    locs = NULL,
    locs_id = "site_id",
    radius = c(0L, 1e3L, 1e4L, 5e4L),
    preprocess = process_modis_merge,
    name_covariates = NULL,
    subdataset = NULL,
    fun_summary = "mean",
    nthreads = floor(length(parallelly::availableWorkers()) / 2),
    package_list_add = NULL,
    export_list_add = NULL,
    ...
  ) {
    if (!is.function(preprocess)) {
      stop("preprocess should be one of process_modis_merge,
process_modis_swath, or process_bluemarble.")
    }
    # read all arguments
    hdf_args <- c(as.list(environment()), list(...))

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
        "dplyr", "parallelly", "doParallel", "rlang")
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

        hdf_args <- append(hdf_args, values = list(date = day_to_pick))
        hdf_args <- append(hdf_args, values = list(path = hdf_args$from))
        # unified interface with rlang::inject
        vrt_today <-
          rlang::inject(preprocess(!!!hdf_args))

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
                  calc_modis_daily(
                    locs = locs_input,
                    from = vrt_today,
                    locs_id = locs_id,
                    date = as.character(day_to_pick),
                    fun_summary = fun_summary,
                    name_extracted = name_radius,
                    radius = radius[k]
                  )
                return(extracted)
              }, error = function(e) {
                name_radius <-
                  sprintf("%s%05d",
                          name_covariates,
                          radius[k])
                error_df <- sf::st_drop_geometry(locs_input)
                # coerce to avoid errors
                error_df <- as.data.frame(error_df)
                error_df <- error_df[, c(locs_id, "time")]
                error_df[[name_radius]] <- -99999
                attr(error_df, "error_message") <- e
                return(error_df)
              }
              )
            }
          )
        res <-
          Reduce(function(x, y) {
            dplyr::left_join(x, y,
              by = c(locs_id, "time")
            )
          },
          res0)
        return(res)
      }
    Sys.sleep(1L)
    return(calc_results)
  }


#' Calculate temporal dummy covariates
#' @description
#' Calculate temporal dummy covariates at point locations. Returns a
#' \code{data.frame} object with \code{locs_id}, year binary variable for each
#' value in \code{year}, and month and day of week binary variables.
#' @param locs data.frame with a temporal field named `"time"`
#' @param locs_id character(1). Unique site identifier column name.
#'  Default is `"site_id"`.
#' @param year integer. Year domain to dummify.
#'  Default is \code{seq(2018L, 2022L)}
#' @param ... Placeholders.
#' @returns a data.frame object
#' @author Insang Song
#' @importFrom methods is
#' @importFrom data.table year
#' @importFrom data.table month
#' @importFrom data.table as.data.table
#' @export
calc_temporal_dummies <-
  function(
    locs,
    locs_id = "site_id",
    year = seq(2018L, 2022L),
    ...
  ) {
    if (!methods::is(locs, "data.frame")) {
      stop("Argument locs is not a data.frame.\n")
    }
    if (!"time" %in% names(locs)) {
      stop("A mandatory field 'time' does not exist in locs.\n")
    }
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
    dt_year_dum <- dummify(vec_year, year)
    # should the last year be the present year or 2022?
    colnames(dt_year_dum) <-
      sprintf("DUM_Y%d_0_00000", year)

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


# nolint start
#' Calculate Sum of Exponentially Decaying Contributions (SEDC) covariates
#' @param from `SpatVector` object. Locations where each SEDC is calculated. 
#' @param locs `SpatVector` object. Locations where
#'  the sum of SEDCs are calculated.
#' @param locs_id character(1). Name of the unique id field in `point_to`.
#' @param sedc_bandwidth numeric(1).
#' Distance at which the source concentration is reduced to
#'  `exp(-3)` (approximately -95 %)
#' @param target_fields character(varying). Field names in characters.
#' @returns a data.frame (tibble) object with input field names with
#'  a suffix \code{"_sedc"} where the sums of EDC are stored.
#'  Additional attributes are attached for the EDC information.
#'    - `attr(result, "sedc_bandwidth")``: the bandwidth where
#'  concentration reduces to approximately five percent
#'    - `attr(result, "sedc_threshold")``: the threshold distance
#'  at which emission source points are excluded beyond that
#' @note The function is originally from
#' [chopin](https://github.com/Spatiotemporal-Exposures-and-Toxicology/chopin)
#' Distance calculation is done with terra functions internally.
#'  Thus, the function internally converts sf objects in
#'  \code{point_*} arguments to terra.
#'  The threshold should be carefully chosen by users.
#' @author Insang Song
#' @references
#' * [Messier KP, Akita Y, & Serre ML. (2012). Integrating Address Geocoding, Land Use Regression, and Spatiotemporal Geostatistical Estimation for Groundwater Tetrachloroethylene. _Environmental Science & Technology_ 46(5), 2772-2780.](https://dx.doi.org/10.1021/es203152a)
#' * Wiesner C. (n.d.). [Euclidean Sum of Exponentially Decaying Contributions Tutorial](https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html)
#' @examples
#' library(terra)
#' library(sf)
#' set.seed(101)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#' nc <- terra::project(nc, "EPSG:5070")
#' pnt_locs <- terra::centroids(nc, inside = TRUE)
#' pnt_locs <- pnt_locs[, "NAME"]
#' pnt_from <- terra::spatSample(nc, 100L)
#' pnt_from$pid <- seq(1, 100)
#' pnt_from <- pnt_from[, "pid"]
#' pnt_from$val1 <- rgamma(100L, 1, 0.05)
#' pnt_from$val2 <- rgamma(100L, 2, 1)
#'
#' vals <- c("val1", "val2")
#' calc_sedc(pnt_locs, pnt_from, "NAME", 1e5, vals)
#' @importFrom dplyr as_tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr all_of
#' @importFrom dplyr across
#' @importFrom dplyr ungroup
#' @importFrom terra nearby
#' @importFrom terra distance
#' @importFrom terra buffer
#' @importFrom rlang sym
#' @export
# nolint end
calc_sedc <-
  function(
    from = NULL,
    locs = NULL,
    locs_id = NULL,
    sedc_bandwidth = NULL,
    target_fields = NULL
  ) {
    # define sources, set SEDC exponential decay range

    if (!methods::is(locs, "SpatVector")) {
      locs <- try(terra::vect(locs))
    }
    if (!methods::is(from, "SpatVector")) {
      from <- try(terra::vect(from))
    }

    cn_overlap <- intersect(names(locs), names(from))
    if (length(cn_overlap) > 0) {
      warning(
        sprintf(
          "There are %d fields with the same name.
The result may not be accurate.\n",
          length(cn_overlap)
        )
      )
    }
    len_point_locs <- seq_len(nrow(locs))

    locs$from_id <- len_point_locs
    locs_buf <-
      terra::buffer(
        locs,
        width = sedc_bandwidth * 2,
        quadsegs = 90
      )

    from_in <- from[locs_buf, ]
    len_point_from <- seq_len(nrow(from_in))

    # len point from? len point to?
    from_in$to_id <- len_point_from
    dist <- NULL

    # near features with distance argument: only returns integer indices
    # threshold is set to the twice of sedc_bandwidth
    # lines 895-900 may overlap with distance arg in 912-913
    res_nearby <-
      terra::nearby(locs, from_in, distance = sedc_bandwidth * 2)
    # attaching actual distance
    dist_nearby <- terra::distance(locs, from_in)
    dist_nearby_df <- as.vector(dist_nearby)
    # adding integer indices
    dist_nearby_tdf <-
      expand.grid(
        from_id = len_point_locs,
        to_id = len_point_from
      )
    dist_nearby_df <- cbind(dist_nearby_tdf, dist = dist_nearby_df)

    # summary
    res_sedc <- res_nearby |>
      dplyr::as_tibble() |>
      dplyr::left_join(data.frame(locs)) |>
      dplyr::left_join(data.frame(from_in)) |>
      dplyr::left_join(dist_nearby_df) |>
      # per the definition in
      # https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html
      # exp(-3) is about 0.05
      dplyr::mutate(w_sedc = exp((-3 * dist) / sedc_bandwidth)) |>
      dplyr::group_by(!!rlang::sym(locs_id)) |>
      dplyr::summarize(
        dplyr::across(
          dplyr::all_of(target_fields),
          ~sum(w_sedc * ., na.rm = TRUE)
        )
      ) |>
      dplyr::ungroup()
    idx_air <- grep("_AIR_", names(res_sedc))
    names(res_sedc)[idx_air] <-
      sprintf("%s_%05d", names(res_sedc)[idx_air], sedc_bandwidth)

    attr(res_sedc, "sedc_bandwidth") <- sedc_bandwidth
    attr(res_sedc, "sedc_threshold") <- sedc_bandwidth * 2

    return(res_sedc)
  }




#' Calculate toxic release covariates
#' @description
#' Extract toxic release values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id} and variables for each chemical in
#' \code{from}.
#' @param from SpatVector(1). Output of \code{process_tri()}.
#' @param locs sf/SpatVector. Locations where TRI variables are calculated.
#' @param locs_id character(1). Unique site identifier column name.
#'  Default is `"site_id"`.
#' @param radius Circular buffer radius.
#' Default is \code{c(1000, 10000, 50000)} (meters)
#' @param ... Placeholders.
#' @author Insang Song, Mariana Kassien
#' @returns a data.frame object
#' @note U.S. context.
#' @seealso [calc_sedc()], [process_tri()]
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
#' @export
calc_tri <- function(
  from = NULL,
  locs,
  locs_id = "site_id",
  radius = c(1e3L, 1e4L, 5e4L),
  ...
) {
  if (!methods::is(locs, "SpatVector")) {
    if (methods::is(locs, "sf")) {
      locs <- terra::vect(locs)
    }
  }
  if (!is.numeric(radius)) {
    stop("radius should be numeric.\n")
  }
  locs_re <- terra::project(locs, terra::crs(from))

  # split by year: locs and tri locations
  tri_cols <- grep("_AIR", names(from), value = TRUE)
  # error fix: no whitespace
  tri_cols <- sub(" ", "_", tri_cols)

  # inner lapply
  list_radius <- split(radius, radius)
  list_locs_tri <-
    lapply(
      list_radius,
      function(x) {
        locs_tri_s <-
          calc_sedc(
            locs = locs_re,
            from = from,
            locs_id = locs_id,
            sedc_bandwidth = x,
            target_fields = tri_cols
          )
        return(locs_tri_s)
      }
    )
  # bind element data.frames into one
  df_tri <- Reduce(function(x, y) dplyr::full_join(x, y), list_locs_tri)
  if (nrow(df_tri) != nrow(locs)) {
    df_tri <- dplyr::left_join(as.data.frame(locs), df_tri)
  }
  return(df_tri)
}




#' Calculate road emissions covariates
#' @param from SpatVector(1). Output of \code{process_nei()}.
#' @param locs sf/SpatVector. Locations at NEI values are joined.
#' @param locs_id character(1). Unique site identifier column name.
#' Unused but kept for compatibility.
#' @param ... Placeholders.
#' @author Insang Song, Ranadeep Daw
#' @seealso [process_nei()]
#' @returns a data.frame object
#' @importFrom terra vect
#' @importFrom methods is
#' @importFrom terra project
#' @importFrom terra intersect
#' @export
calc_nei <- function(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  ...
) {
  if (!methods::is(locs, "SpatVector")) {
    locs <- try(terra::vect(locs))
    if (inherits(locs, "try-error")) {
      stop("locs is unable to be converted to SpatVector.\n")
    }
  }
  # spatial join
  locs_re <- terra::project(locs, terra::crs(from))
  locs_re <- terra::intersect(locs_re, from)

  return(locs_re)
}



#' Calculate wildfire smoke covariates
#' @description
#' Extract wildfire smoke plume values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, date, and binary variable
#' for wildfire smoke plume density inherited from \code{from} (0 = point not
#' covered by wildfire smoke plume; 1 = point covered by wildfire smoke plume).
#' @param from SpatVector(1). Output of \code{process_hms()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @seealso [process_hms()]
#' @author Mitchell Manware
#' @return a data.frame object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @export
calc_hms <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0) {
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### from == character indicates no wildfire smoke polumes are present
  #### return 0 for all locs and dates
  if ("character" %in% class(from)) {
    cat(paste0(
      "Inherited list of dates due to absent smoke plume polygons.\n"
    ))
    skip_extraction <- NULL
    skip_variable <- from[1]
    skip_dates <- from[2:length(from)]
    skip_sites_id <- data.frame(data.frame(locs)[, locs_id])
    for (s in seq_along(skip_dates)) {
      skip_extraction_date <- cbind(
        skip_sites_id,
        as.Date(
          skip_dates[s],
          format = "%Y%m%d"
        ),
        as.integer(0)
      )
      colnames(skip_extraction_date) <- c(
        locs_id,
        "date",
        paste0(
          skip_variable,
          "_",
          radius
        )
      )
      skip_extraction <- rbind(
        skip_extraction,
        skip_extraction_date
      )
    }
    cat(paste0(
      "Returning ",
      tolower(skip_variable),
      " smoke plume covariates.\n"
    ))
    return(skip_extraction)
  }
  #### prepare sites
  sites_e <- process_locs_vector(
    locs,
    terra::crs(from),
    radius
  )
  #### site identifiers only
  sites_id <- subset(
    terra::as.data.frame(sites_e),
    select = locs_id
  )
  #### generate date sequence for missing polygon patch
  date_sequence <- generate_date_sequence(
    date_start = as.Date(
      from$Date[1],
      format = "%Y%m%d"
    ),
    date_end = as.Date(
      from$Date[nrow(from)],
      format = "%Y%m%d"
    ),
    sub_hyphen = TRUE
  )
  #### empty location data.frame
  sites_extracted <- NULL
  for (r in seq_len(nrow(from))) {
    #### select data layer
    data_layer <- from[r]
    layer_date <- as.Date(
      data_layer$Date,
      format = "%Y%m%d"
    )
    layer_name <- data_layer$Density
    cat(paste0(
      "Calculating daily ",
      as.character(
        layer_name
      ),
      " covariates for date ",
      layer_date,
      "...\n"
    ))
    #### extract layer data at sites
    sites_extracted_layer <- as.integer(
      terra::relate(
        sites_e,
        data_layer,
        "intersects"
      )
    )
    #### merge with site_id and date
    sites_extracted_layer <- cbind(
      sites_id,
      layer_date,
      sites_extracted_layer
    )
    #### define column names
    colnames(sites_extracted_layer) <- c(
      locs_id,
      "date",
      paste0(
        tolower(
          layer_name
        ),
        "_",
        radius
      )
    )
    #### merge with empty sites_extracted
    sites_extracted <- rbind(
      sites_extracted,
      sites_extracted_layer
    )
  }
  #### check for missing dates (missing polygons)
  if (!(identical(date_sequence, from$Date))) {
    cat(paste0(
      "Detected absent smoke plume polygons.\n"
    ))
    missing_dates <- date_sequence[
      which(!(date_sequence %in% from$Date))
    ]
    ###
    for (m in seq_along(missing_dates)) {
      missing_date <- as.Date(
        missing_dates[m],
        format = "%Y%m%d"
      )
      cat(paste0(
        "Smoke plume polygons absent for date ",
        missing_date,
        ". Returning 0 (smoke plumes absent).\n"
      ))
      missing_data <- cbind(
        sites_id,
        missing_date,
        0
      )
      colnames(missing_data) <- colnames(sites_extracted)
      sites_extracted <- rbind(
        sites_extracted,
        missing_data
      )
    }
  }
  #### coerce binary to integer
  sites_extracted[, 3] <- as.integer(sites_extracted[, 3])
  #### order by date
  sites_extracted_ordered <- sites_extracted[order(sites_extracted$date), ]
  cat(paste0(
    "Returning ",
    layer_name,
    " covariates.\n"
  ))
  #### return data.frame
  return(data.frame(sites_extracted_ordered))
}

#' Calculate elevation covariates
#' @description
#' Extract elevation values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id} and elevation variable. Elevation variable
#' column name reflects the elevation statistic, spatial resolution of
#' \code{from}, and circular buffer radius.
#' @param from SpatRaster(1). Output from \code{process_gmted()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @author Mitchell Manware
#' @seealso [process_gmted()]
#' @return a data.frame object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @export
calc_gmted <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0,
    fun = "mean") {
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### prepare sites
  sites_e <- process_locs_vector(
    locs,
    terra::crs(from),
    radius
  )
  #### site identifiers only
  sites_id <- subset(
    terra::as.data.frame(sites_e),
    select = locs_id
  )
  #### layer name
  layer_name <- names(from)
  cat(paste0(
    "Calculating ",
    process_gmted_codes(
      substr(
        layer_name,
        1,
        2
      ),
      statistic = TRUE,
      invert = TRUE
    ),
    " covariates with ",
    process_gmted_codes(
      substr(
        layer_name,
        3,
        4
      ),
      resolution = TRUE,
      invert = TRUE
    ),
    " resolution data.\n"
  ))
  #### extract layer data at sites
  sites_extracted <- terra::extract(
    from,
    sites_e,
    fun = fun,
    method = "simple",
    ID = FALSE,
    bind = FALSE,
    na.rm = TRUE
  )
  #### merge with site_id and date (year)
  sites_extracted <- cbind(
    sites_id,
    sites_extracted
  )
  #### convert integer to numeric
  sites_extracted[, 2] <- as.numeric(sites_extracted[, 2])
  #### define column names
  colnames(sites_extracted) <- c(
    locs_id,
    paste0(
      tolower(
        gsub(
          " ",
          "_",
          process_gmted_codes(
            substr(
              layer_name,
              1,
              2
            ),
            statistic = TRUE,
            invert = TRUE
          )
        )
      ),
      "_",
      substr(
        layer_name,
        3,
        4
      ),
      "_",
      radius
    )
  )
  #### return data.frame
  return(data.frame(sites_extracted))
}

#' Calculate meteorological covariates
#' @description
#' Extract meteorological values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id}, date, vertical pressure level, and
#' meteorological variable. Meteorological variable column name reflects
#' variable and circular buffer radius.
#' @param from SpatRaster(1). Output of \code{process_narr()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @author Mitchell Manware
#' @seealso [process_narr()]
#' @return a data.frame object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @export
calc_narr <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0,
    fun = "mean") {
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### prepare sites
  sites_e <- process_locs_vector(
    locs,
    terra::crs(from),
    radius
  )
  #### site identifiers only
  sites_id <- subset(
    terra::as.data.frame(sites_e),
    select = locs_id
  )
  #### empty location data.frame
  sites_extracted <- NULL
  for (l in seq_len(terra::nlyr(from))) {
    #### select data layer
    data_layer <- from[[l]]
    #### extract layer names for variable, date, and pressure level
    data_name <- strsplit(
      names(data_layer),
      "_"
    )[[1]]
    #### monolevel data
    if (length(data_name) == 2) {
      layer_level <- "monolevel"
      layer_date <- as.Date(
        data_name[2],
        format = "%Y%m%d"
      )
      cat(paste0(
        "Calculating daily ",
        data_name[1],
        " covariates at ",
        layer_level,
        " for date ",
        layer_date,
        "...\n"
      ))
      #### pressure level data
    } else if (length(data_name) == 3) {
      layer_level <- data_name[2]
      layer_date <- as.Date(
        data_name[3],
        format = "%Y%m%d"
      )
      cat(paste0(
        "Calculating daily ",
        data_name[1],
        " covariates at ",
        layer_level,
        " for date ",
        layer_date,
        "...\n"
      ))
    }
    #### extract layer data at sites
    sites_extracted_layer <- terra::extract(
      data_layer,
      sites_e,
      fun = fun,
      method = "simple",
      ID = FALSE,
      bind = FALSE,
      na.rm = TRUE
    )
    #### merge with site_id, datetime, pressure level
    sites_extracted_layer <- cbind(
      sites_id,
      layer_date,
      layer_level,
      sites_extracted_layer
    )
    #### define column names
    colnames(sites_extracted_layer) <- c(
      locs_id,
      "date",
      "level",
      paste0(
        data_name[1],
        "_",
        radius
      )
    )
    #### merge with empty sites_extracted
    sites_extracted <- rbind(
      sites_extracted,
      sites_extracted_layer
    )
    if (l == terra::nlyr(from)) {
      cat(paste0(
        "Returning ",
        data_name[1],
        " covariates.\n"
      ))
    }
  }
  #### return data.frame
  return(data.frame(sites_extracted))
}


#' Calculate atmospheric composition covariates
#' @description
#' Extract atmospheric composition values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, date and hour, vertical
#' pressure level, and atmospheric composition variable. Atmospheric
#' composition variable column name reflects variable and circular buffer
#' radius.
#' @param from SpatRaster(1). Output of \code{process_geos()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @author Mitchell Manware
#' @seealso [process_geos()]
#' @return a data.frame object
#' @importFrom terra vect
#' @importFrom terra buffer
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @export
calc_geos <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0,
    fun = "mean") {
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### prepare sites
  sites_e <- process_locs_vector(
    locs,
    terra::crs(from),
    radius
  )
  #### site identifiers
  sites_id <- subset(
    locs,
    select = locs_id
  )
  #### empty location data.frame
  sites_extracted <- NULL
  for (l in seq_len(terra::nlyr(from))) {
    #### select data layer
    data_layer <- from[[l]]
    #### extract layer names for variable and datetime sequence
    data_name <- strsplit(
      names(data_layer),
      "_"
    )[[1]]
    #### set datetime
    layer_datetime <- ISOdate(
      year = substr(data_name[3], 1, 4),
      month = substr(data_name[3], 5, 6),
      day = substr(data_name[3], 7, 8),
      hour = substr(data_name[4], 1, 2),
      min = substr(data_name[4], 3, 4),
      sec = substr(data_name[4], 5, 6),
      tz = "UTC"
    )
    layer_level <- data_name[2]
    cat(paste0(
      "Calculating hourly ",
      data_name[1],
      " covariates at ",
      layer_level,
      " for date ",
      layer_datetime,
      "...\n"
    ))
    #### extract layer data at sites
    sites_extracted_layer <- terra::extract(
      data_layer,
      sites_e,
      fun = fun,
      method = "simple",
      ID = FALSE,
      bind = FALSE,
      na.rm = TRUE
    )
    #### merge with site_id, datetime, pressure level
    sites_extracted_layer <- cbind(
      sites_id,
      layer_datetime,
      layer_level,
      sites_extracted_layer
    )
    #### define column names
    colnames(sites_extracted_layer) <- c(
      locs_id,
      "date",
      "level",
      paste0(
        data_name[1],
        "_",
        radius
      )
    )
    #### merge with empty sites_extracted
    sites_extracted <- rbind(
      sites_extracted,
      sites_extracted_layer
    )
    if (l == terra::nlyr(from)) {
      cat(paste0(
        "Returning ",
        data_name[1],
        " covariates.\n"
      ))
    }
  }
  #### return data.frame
  return(data.frame(sites_extracted))
}

#' Calculate population density covariates
#' @description
#' Extract population density values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, year, and population
#' density variable. Population density variable column name reflects
#' spatial resolution of \code{from} and circular buffer radius.
#' @param from SpatRaster(1). Output of \code{process_sedac_population()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @author Mitchell Manware
#' @seealso [process_sedac_population()]
#' @return a data.frame object
#' @importFrom terra vect
#' @importFrom terra buffer
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @export
calc_sedac_population <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0,
    fun = "mean") {
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### prepare sites
  sites_e <- process_locs_vector(
    locs,
    terra::crs(from),
    radius
  )
  #### site identifiers
  sites_id <- subset(
    locs,
    select = locs_id
  )
  #### empty location data.frame
  sites_extracted <- NULL
  for (l in seq_len(terra::nlyr(from))) {
    data_layer <- from[[l]]
    name_split <- strsplit(
      names(data_layer),
      "_"
    )[[1]]
    year <- name_split[4]
    resolution <- name_split[5:6]
    cat(
      paste0(
        "Calculating annual population density covariates for ",
        year,
        " at ",
        process_sedac_codes(
          paste0(
            resolution[1],
            "_",
            resolution[2]
          ),
          invert = TRUE
        ),
        " resolution...\n"
      )
    )
    #### extract layer data at sites
    sites_extracted_layer <- terra::extract(
      data_layer,
      sites_e,
      fun = fun,
      method = "simple",
      ID = FALSE,
      bind = FALSE,
      na.rm = TRUE
    )
    #### merge with site_id, datetime, pressure level
    sites_extracted_layer <- cbind(
      sites_id,
      as.integer(year),
      sites_extracted_layer
    )
    #### define column names
    colnames(sites_extracted_layer) <- c(
      locs_id,
      "year",
      paste0(
        "pop_",
        resolution[1],
        resolution[2],
        "_",
        radius
      )
    )
    #### merge with empty sites_extracted
    sites_extracted <- rbind(
      sites_extracted,
      sites_extracted_layer
    )
  }
  cat(
    paste0(
      "Returning population covariates.\n"
    )
  )
  #### return data frame
  return(data.frame(sites_extracted))
}



#' Calculate roads covariates
#' @description Prepared groads data is clipped with the buffer polygons
#' of `radius`. The total length of the roads are calculated.
#' Then the density of the roads is calculated by dividing
#' the total length from the area of the buffer. `terra::linearUnits()`
#' is used to convert the unit of length to meters.
#' @param from SpatVector(1). Output of `process_sedac_groads`.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 1000).
#' @param fun function(1). Function used to summarize the length of roads
#' within sites location buffer (Default is `sum`).
#' @note Unit is km / sq km.
#' @author Insang Song
#' @seealso [`process_sedac_groads`]
#' @return a data.frame object with three columns.
#' @importFrom terra vect
#' @importFrom stats aggregate
#' @importFrom stats setNames
#' @importFrom terra as.data.frame
#' @importFrom terra project
#' @importFrom terra intersect
#' @importFrom terra perim
#' @importFrom terra crs
#' @importFrom terra expanse
#' @importFrom terra linearUnits
#' @export
calc_sedac_groads <- function(
    from = NULL,
    locs = NULL,
    locs_id = NULL,
    radius = 1000,
    fun = sum) {
  #### check for null parameters
  if (radius <= 0) {
    stop("radius should be greater than 0.\n")
  }
  check_for_null_parameters(mget(ls()))
  #### prepare sites
  sites_e <- process_locs_vector(
    locs,
    terra::crs(from),
    radius
  )

  from_re <- terra::project(from, terra::crs(sites_e))
  from_re <- from[terra::ext(sites_e), ]
  from_clip <- terra::intersect(sites_e, from_re)
  area_buffer <- sites_e[1, ]
  area_buffer <- terra::expanse(area_buffer)

  from_clip$rlength <- terra::perim(from_clip)
  from_clip <-
    aggregate(
      from_clip$rlength,
      by = from_clip[[locs_id]],
      FUN = fun,
      na.rm = TRUE
    )
  # linear unit conversion
  det_unit <- terra::linearUnits(from_re)
  if (det_unit == 0) {
    det_unit <- 1
  }
  # km / sq km
  from_clip[["x"]] <- (from_clip[["x"]] * det_unit / 1e3)
  from_clip$density <-
    from_clip[["x"]] / (area_buffer * (det_unit ^ 2) / 1e6)
  from_clip <-
    setNames(
      from_clip,
      c(locs_id,
        sprintf("GRD_TOTAL_0_%05d", radius),
        sprintf("GRD_DENKM_0_%05d", radius))
    )

  return(from_clip)
}
