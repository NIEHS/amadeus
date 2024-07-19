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
#' @note `covariate` argument value is converted to lowercase.
#' @seealso
#' - [`calc_modis_par`]: `"modis"`, `"MODIS"`
#' - [`calc_koppen_geiger`]: `"koppen-geiger"`, `"koeppen-geiger"`, `"koppen"`
#' - [`calc_ecoregion`]: `"ecoregion"`, `"ecoregions"`
#' - [`calc_temporal_dummies`]: `"dummies"`
#' - [`calc_hms`]: `"hms"`, `"smoke"`
#' - [`calc_gmted`]: `"gmted"`
#' - [`calc_narr`]: `"narr"`
#' - [`calc_geos`]: `"geos"`, `"geos_cf"`
#' - [`calc_sedac_population`]: `"population"`, `"sedac_population"`
#' - [`calc_sedac_groads`]: `"roads"`, `"groads"`, `"sedac_groads"`
#' - [`calc_nlcd`]: `"nlcd"`
#' - [`calc_tri`]: `"tri"`
#' - [`calc_nei`]: `"nei"`
#' - [`calc_merra2`]: `"merra"`, `"MERRA"`, `"merra2"`, `"MERRA2"`
#' - [`calc_gridmet`]: `"gridMET"`, `"gridmet"`
#' - [`calc_terraclimate`]: `"terraclimate"`, `"TerraClimate"`
#' @return Calculated covariates as a data.frame or SpatVector object
#' @author Insang Song
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_covariates(
#'   covariate = "narr",
#'   from = narr, # derived from process_covariates() example
#'   locs = loc,
#'   locs_id = "id",
#'   geom = FALSE
#' )
#' }
#' @export
# nolint end
calc_covariates <-
  function(
      covariate = c("modis", "koppen-geiger",
                    "koeppen-geiger", "koppen", "koeppen",
                    "geos", "dummies", "gmted",
                    "sedac_groads", "groads", "roads",
                    "ecoregions", "ecoregion", "hms", "smoke",
                    "gmted", "narr", "geos",
                    "sedac_population", "population", "nlcd",
                    "merra", "merra2", "gridmet", "terraclimate",
                    "tri", "nei"),
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
      nlcd = calc_nlcd,
      smoke = calc_hms,
      hms = calc_hms,
      sedac_groads = calc_sedac_groads,
      roads = calc_sedac_groads,
      groads = calc_sedac_groads,
      sedac_population = calc_sedac_population,
      population = calc_sedac_population,
      nei = calc_nei,
      tri = calc_tri,
      geos = calc_geos,
      gmted = calc_gmted,
      dummies = calc_temporal_dummies,
      merra = calc_merra2,
      merra2 = calc_merra2,
      gridmet = calc_gridmet,
      terraclimate = calc_terraclimate
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
        stop(
          paste0(
            "Please refer to the argument list and the error message above ",
            "to rectify the error.\n"
          )
        )
      })

    return(res_covariate)
  }


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
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders.
#' @seealso [`process_koppen_geiger`]
#' @return a data.frame or SpatVector object
#' @note The returned object contains a
#' `$description` column to represent the temporal range covered by the
#' dataset. For more information, see
#' <https://www.nature.com/articles/sdata2018214>.
#' @author Insang Song
#' @importFrom terra vect
#' @importFrom terra rast
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom terra extract
#' @importFrom terra coltab
#' @importFrom terra merge
#' @importFrom methods is
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_koppen_geiger(
#'   from = kg, # derived from process_koppen_geiger() example
#'   locs = loc,
#'   locs_id = "id",
#'   geom = FALSE
#' )
#' }
#' @export
# locs (locs), from (from), locs_id (id_col), variables
calc_koppen_geiger <-
  function(
      from = NULL,
      locs = NULL,
      locs_id = "site_id",
      geom = FALSE,
      ...) {
    # prepare locations
    locs_prepared <- calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = 0,
      geom = geom
    )
    locs_kg <- locs_prepared[[1]]
    locs_df <- locs_prepared[[2]]
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

    locs_kg_extract[[locs_id]] <- locs_df[, 1]
    if (geom) {
      locs_kg_extract$geometry <- locs_df[, 2]
    }
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
        locs_id = locs_df,
        as.character(terra::metags(from)),
        df_ae_separated
      )
    names(kg_extracted)[1] <- locs_id
    if (geom) {
      names(kg_extracted)[2:3] <- c("geometry", "description")
      sites_return <- calc_return_locs(
        covar = kg_extracted,
        POSIXt = FALSE,
        geom = geom,
        crs = terra::crs(from)
      )
      #### return data.frame
      return(sites_return)
    } else {
      names(kg_extracted)[2] <- "description"
      return(kg_extracted)
    }
  }


#' Calculate land cover covariates
#' @description
#' Compute ratio of land cover class in circle buffers around points. Returns
#' a \code{data.frame} object containing \code{locs_id}, longitude, latitude,
#' time (year), and computed ratio for each land cover class.
#' @param from SpatRaster(1). Output of \code{process_nlcd()}.
#' @param locs terra::SpatVector of points geometry
#' @param locs_id character(1). Unique identifier of locations
#' @param mode character(1). One of `"exact"`
#'   (using [`exactextractr::exact_extract()`])
#'   or `"terra"` (using [`terra::freq()`]).
#' @param radius numeric (non-negative) giving the
#' radius of buffer around points
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values may expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`. Only valid when
#' `mode = "exact"`.
#' See [`exactextractr::exact_extract`] for details.
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param nthreads integer(1). Number of threads to be used
#' @param ... Placeholders.
#' @note NLCD is available in U.S. only. Users should be aware of
#' the spatial extent of the data. The results are different depending
#' on `mode` argument. The `"terra"` mode is less memory intensive
#' but less accurate because it counts the number of cells
#' intersecting with the buffer. The `"exact"` may be more accurate
#' but uses more memory as it will account for the partial overlap
#' with the buffer.
#' @seealso [`process_nlcd`]
#' @return a data.frame or SpatVector object
#' @importFrom utils read.csv
#' @importFrom methods is
#' @importFrom terra rast
#' @importFrom terra project
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom terra set.crs
#' @importFrom terra buffer
#' @importFrom sf st_union
#' @importFrom sf st_geometry
#' @importFrom terra intersect
#' @importFrom terra metags
#' @importFrom exactextractr exact_extract
#' @importFrom future plan multicore sequential
#' @importFrom future.apply future_Map
#' @importFrom collapse rowbind
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_nlcd(
#'   from = nlcd, # derived from process_nlcd() example
#'   locs = loc,
#'   locs_id = "id",
#'   mode = "exact",
#'   geom = FALSE
#' )
#' }
#' @export
calc_nlcd <- function(from,
                      locs,
                      locs_id = "site_id",
                      mode = c("exact", "terra"),
                      radius = 1000,
                      max_cells = 5e7,
                      geom = FALSE,
                      nthreads = 1L,
                      ...) {
  # check inputs
  mode <- match.arg(mode)
  if (!is.numeric(radius)) {
    stop("radius is not a numeric.")
  }
  if (radius <= 0 && terra::geomtype(locs) == "points") {
    stop("radius has not a likely value.")
  }

  if (!methods::is(from, "SpatRaster")) {
    stop("from is not a SpatRaster.")
  }
  if (nthreads > 1L) {
    stopifnot(Sys.info()["sysname"] != "Windows")
    future::plan(future::multicore, workers = nthreads)
  } else {
    future::plan(future::sequential)
  }

  # prepare locations
  locs_prepared <- calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  locs_vector <- locs_prepared[[1]]
  locs_df <- locs_prepared[[2]]

  year <- try(as.integer(terra::metags(from, name = "year")))
  # select points within mainland US and reproject on nlcd crs if necessary
  data_vect_b <-
    terra::project(locs_vector, y = terra::crs(from))
  # create circle buffers with buf_radius
  bufs_pol <- terra::buffer(data_vect_b, width = radius)
  cfpath <- system.file("extdata", "nlcd_classes.csv", package = "amadeus")
  nlcd_classes <- utils::read.csv(cfpath)

  if (mode == "terra") {
    # terra mode
    class_query <- "names"
    # extract land cover class in each buffer
    nlcd_at_bufs <- future.apply::future_Map(
      function(i) {
        terra::freq(
          from,
          zones = bufs_pol[i, ],
          wide = TRUE
        )
      }, seq_len(nrow(bufs_pol)),
      future.seed = TRUE
    )
    nlcd_at_bufs <- collapse::rowbind(nlcd_at_bufs, fill = TRUE)
    nlcd_at_bufs <- nlcd_at_bufs[, -seq(1, 2)]
    nlcd_cellcnt <- nlcd_at_bufs[, seq(1, ncol(nlcd_at_bufs), 1)]
    nlcd_cellcnt <- nlcd_cellcnt / rowSums(nlcd_cellcnt, na.rm = TRUE)
    nlcd_at_bufs[, seq(1, ncol(nlcd_at_bufs), 1)] <- nlcd_cellcnt
  } else {
    class_query <- "value"
    # ratio of each nlcd class per buffer
    bufs_polx <- bufs_pol[terra::ext(from), ] |>
      sf::st_as_sf()

    nlcd_at_bufs <- future.apply::future_Map(
      function(i) {
        exactextractr::exact_extract(
          from,
          bufs_polx[i, ],
          fun = "frac",
          force_df = TRUE,
          progress = FALSE,
          append_cols = locs_id,
          max_cells_in_memory = max_cells
        )
      }, seq_len(length(bufs_polx)),
      future.seed = TRUE
    )
    nlcd_at_bufs <- collapse::rowbind(nlcd_at_bufs, fill = TRUE)
    # select only the columns of interest
    nlcd_at_buf_names <- names(nlcd_at_bufs)
    nlcd_val_cols <-
      grep("^frac_", nlcd_at_buf_names)
    nlcd_at_bufs <- nlcd_at_bufs[, nlcd_val_cols]
  }
  # fill NAs
  nlcd_at_bufs[is.na(nlcd_at_bufs)] <- 0

  # change column names
  nlcd_names <- names(nlcd_at_bufs)
  nlcd_names <- sub(pattern = "frac_", replacement = "", x = nlcd_names)
  nlcd_names <-
    switch(
      mode,
      exact = as.numeric(nlcd_names),
      terra = nlcd_names
    )
  nlcd_names <-
    nlcd_classes$class[match(nlcd_names, nlcd_classes[[class_query]])]
  new_names <- sprintf("LDU_%s_0_%05d", nlcd_names, radius)
  names(nlcd_at_bufs) <- new_names

  # merge locs_df with nlcd class fractions
  new_data_vect <- cbind(locs_df, as.integer(year), nlcd_at_bufs)
  if (geom) {
    names(new_data_vect)[1:3] <- c(locs_id, "geometry", "time")
  } else {
    names(new_data_vect)[1:2] <- c(locs_id, "time")
  }
  new_data_vect <- calc_return_locs(
    covar = new_data_vect,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  future::plan(future::sequential)
  return(new_data_vect)
}



#' Calculate ecoregions covariates
#' @description
#' Extract ecoregions covariates (U.S. EPA Ecoregions Level 2/3) at point
#' locations. Returns a `data.frame` object containing `locs_id` and
#' binary (0 = point not in ecoregion; 1 = point in ecoregion) variables for
#' each ecoregion.
#' @param from SpatVector(1). Output of [`process_ecoregion`].
#' @param locs sf/SpatVector. Unique locs. Should include
#'  a unique identifier field named `locs_id`
#' @param locs_id character(1). Name of unique identifier.
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders.
#' @seealso [`process_ecoregion`]
#' @return a data.frame or SpatVector object object with dummy variables and
#' attributes of:
#'   - `attr(., "ecoregion2_code")`: Ecoregion lv.2 code and key
#'   - `attr(., "ecoregion3_code")`: Ecoregion lv.3 code and key
#' @author Insang Song
#' @importFrom terra extract
#' @importFrom data.table year
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_ecoregion(
#'   from = ecoregion, # derived from process_ecoregion() example
#'   locs = loc,
#'   locs_id = "id",
#'   geom = FALSE
#' )
#' }
#' @export
calc_ecoregion <-
  function(
    from = NULL,
    locs,
    locs_id = "site_id",
    geom = FALSE,
    ...
  ) {
    # prepare locations
    locs_prepared <- calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = 0,
      geom = geom
    )
    # both objects will preserve the row order
    locsp <- locs_prepared[[1]]
    locs_df <- locs_prepared[[2]]

    extracted <- terra::intersect(locsp, from)

    # Generate field names from extracted ecoregion keys
    # TODO: if we keep all-zero fields, the initial reference
    # should be the ecoregion polygon, not the extracted data
    key2_sorted <- unlist(extracted[[grep("L2", names(extracted))]])
    key2_num <-
      regmatches(key2_sorted, regexpr("\\d{1,2}\\.[1-9]", key2_sorted))
    key2_num <- as.integer(10 * as.numeric(key2_num))
    key2_num <- sprintf("DUM_E2%03d_0_00000", key2_num)
    key2_num_unique <- sort(unique(key2_num))

    key3_sorted <- unlist(extracted[[grep("L3", names(extracted))]])
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

    locs_ecoreg <- cbind(
      locs_df,
      paste0("1997 - ", data.table::year(Sys.Date())),
      df_lv2, df_lv3
    )
    locs_return <- calc_return_locs(
      covar = locs_ecoreg,
      POSIXt = FALSE,
      geom = geom,
      crs = terra::crs(from)
    )
    names(locs_return)[2] <- "description"
    attr(locs_return, "ecoregion2_code") <- sort(unique(from$L2_KEY))
    attr(locs_return, "ecoregion3_code") <- sort(unique(from$L3_KEY))
    return(locs_return)
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
#' multilayer rasters. Passed to `foo`. See [`exactextractr::exact_extract`]
#' for details.
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values will expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`.
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' See [`exactextractr::exact_extract`] for details.
#' @param ... Placeholders.
#' @description The function operates at MODIS/VIIRS products
#' on a daily basis. Given that the raw hdf files are downloaded from
#' NASA, standard file names include a data retrieval date flag starting
#' with letter "A". Leveraging that piece of information, the function will
#' select files of scope on the date of interest.
#' Please note that this function does not provide a function to filter
#' swaths or tiles, so it is strongly recommended to check and pre-filter
#' the file names at users' discretion.
#' @seealso
#' * Preprocessing: [process_modis_merge()], [process_modis_swath()],
#'     [process_blackmarble()]
#' * Parallelization: [calc_modis_par()]
#' @author Insang Song
#' @return a data.frame or SpatVector object.
#' @importFrom terra extract
#' @importFrom terra project
#' @importFrom terra vect
#' @importFrom terra nlyr
#' @importFrom terra describe
#' @importFrom methods is
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @examples
#' \dontrun{
#' locs <- data.frame(lon = -78.8277, lat = 35.95013, id = "001")
#' calc_modis_daily(
#'   from = mod06l2_warp,
#'   locs = locs,
#'   locs_id = "id",
#'   radius = 0,
#'   date = "2024-01-01",
#'   name_extracted = "cloud_fraction_0",
#'   fun_summary = "mean",
#'   max_cells = 3e7
#' )
#' }
#' @export
calc_modis_daily <- function(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  radius = 0L,
  date = NULL,
  name_extracted = NULL,
  fun_summary = "mean",
  max_cells = 3e7,
  geom = FALSE,
  ...
) {
  if (!methods::is(locs, "SpatVector")) {
    locs <- try(terra::vect(locs))
    if (inherits(locs, "try-error")) {
      stop("locs should be a SpatVector or convertible object.")
    }
  }
  if (!locs_id %in% names(locs)) {
    stop(sprintf("locs should include columns named %s.\n",
                 locs_id))
  }

  extract_with_buffer <- function(
    points,
    surf,
    radius,
    id,
    func = "mean",
    maxcells = NULL
  ) {
    # generate buffers
    if (radius == 0) radius <- 1e-6 # approximately 1 meter in degree
    bufs <- terra::buffer(points, width = radius, quadsegs = 180L)
    bufs <- terra::project(bufs, terra::crs(surf))
    # extract raster values
    surf_at_bufs <-
      exactextractr::exact_extract(
        x = surf,
        y = sf::st_as_sf(bufs),
        fun = func,
        force_df = TRUE,
        stack_apply = TRUE,
        append_cols = id,
        progress = FALSE,
        max_cells_in_memory = maxcells
      )
    return(surf_at_bufs)
  }

  ## NaN to NA
  from[is.nan(from)] <- NA

  # raster used to be vrt_today
  extracted <-
    extract_with_buffer(
      points = locs,
      surf = from,
      id = locs_id,
      radius = radius,
      func = fun_summary,
      maxcells = max_cells
    )
  # cleaning names
  # assuming that extracted is a data.frame
  name_offset <- terra::nlyr(from)
  # multiple columns will get proper names
  name_range <- seq(ncol(extracted) - name_offset + 1, ncol(extracted), 1)
  colnames(extracted)[name_range] <- name_extracted
  extracted$time <- as.POSIXlt(date)
  if (geom) {
    # convert to base date, as terra::vect does not like class "POSIXlt"
    extracted$time <- as.Date.POSIXlt(extracted$time)
    # location ID with geometry
    locs_geom_id <- suppressMessages(calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = radius,
      geom = geom
    )[[2]]
    )
    # merge
    extracted_merge <- merge(
      locs_geom_id,
      extracted,
      by = locs_id
    )
    # re-convert to POSIXlt after creating the vect
    extracted_merge$time <- as.POSIXlt(extracted_merge$time)
    extracted_return <- calc_return_locs(
      covar = extracted_merge,
      POSIXt = TRUE,
      geom = geom,
      crs = terra::crs(from)
    )
  } else {
    calc_check_time(covar = extracted, POSIXt = TRUE)
    extracted_return <- extracted
  }
  gc()
  return(extracted_return)
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
#' @param subdataset Indices, names, or search patterns for subdatasets.
#' Find detail usage of the argument in notes.
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
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values will expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`.
#' See [`exactextractr::exact_extract`] for details.
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
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
#' The argument `subdataset` should be in a proper format
#' depending on `preprocess` function:
#' * `process_modis_merge()`: Regular expression pattern.
#'   e.g., `"^LST_"`
#' * `process_modis_swath()`: Subdataset names.
#'   e.g., `c("Cloud_Fraction_Day", "Cloud_Fraction_Night")`
#' * `process_blackmarble()`: Subdataset number.
#'   e.g., for VNP46A2 product, 3L.
#' Dates with less than 80 percent of the expected number of tiles,
#' which are determined by the mode of the number of tiles, are removed.
#' Users will be informed of the dates with insufficient tiles.
#' The result data.frame will have an attribute with the dates with
#' insufficient tiles.
#' @return A data.frame or SpatVector with an attribute:
#' * `attr(., "dates_dropped")`: Dates with insufficient tiles.
#'   Note that the dates mean the dates with insufficient tiles,
#'   not the dates without available tiles.
#' @seealso See details for setting parallelization:
#' * [`future::plan()`]
#' * [`future.apply::future_lapply()`]
#' * [`parallelly::makeClusterPSOCK()`]
#' * [`parallelly::availableCores()`]
#'
#' This function leverages the calculation of single-day MODIS
#' covariates:
#' * [`calc_modis_daily()`]
#'
#' Also, for preprocessing, please refer to:
#' * [`process_modis_merge()`]
#' * [`process_modis_swath()`]
#' * [`process_blackmarble()`]
#' @importFrom methods is
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @importFrom terra nlyr
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom rlang inject
#' @importFrom future plan
#' @importFrom future cluster
#' @importFrom future.apply future_lapply
#' @importFrom parallelly availableWorkers
#' @examples
#' /dontrun{
#' locs <- data.frame(lon = -78.8277, lat = 35.95013, id = "001")
#' locs <- terra::vect(locs, geom = c("lon", "lat"), crs = "EPSG:4326")
#' calc_modis_par(
#'   from =
#'     list.files("./data", pattern = "VNP46A2.", full.names = TRUE),
#'   locs = locs,
#'   locs_id = "site_id",
#'   radius = c(0L, 1000L),
#'   preprocess = process_modis_merge,
#'   name_covariates = "cloud_fraction_0",
#'   subdataset = "Cloud_Fraction",
#'   fun_summary = "mean",
#'   nthreads = 1
#' )
#' }
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
    max_cells = 3e7,
    geom = FALSE,
    ...
  ) {
    if (!is.function(preprocess)) {
      stop("preprocess should be one of process_modis_merge,
process_modis_swath, or process_blackmarble.")
    }
    # read all arguments
    # nolint start
    hdf_args <- c(as.list(environment()), list(...))
    # nolint end
    dates_available_m <-
      regmatches(from, regexpr("A20\\d{2,2}[0-3]\\d{2,2}", from))
    dates_available <- sort(unique(dates_available_m))
    dates_available <- sub("A", "", dates_available)

    # When multiple dates are concerned,
    # the number of tiles are expected to be the same.
    # Exceptions could exist, so here the number of tiles are checked.
    summary_available <- table(dates_available_m)
    summary_available_mode <-
      sort(table(summary_available), decreasing = TRUE)[1]
    summary_available_mode <- as.numeric(names(summary_available_mode))
    summary_available_insuf <-
      which(summary_available < floor(summary_available_mode * 0.8))

    if (length(summary_available_insuf) > 0) {
      dates_insuf <-
        as.Date(dates_available[summary_available_insuf], "%Y%j")
      message(
        paste0(
          "The number of tiles on the following dates are insufficient: ",
          paste(dates_insuf, collapse = ", "),
          ".\n"
        )
      )
      # finally it removes the dates with insufficient tiles
      dates_available <- dates_available[-summary_available_insuf]
    } else {
      dates_insuf <- NA
    }

    locs_input <- try(sf::st_as_sf(locs), silent = TRUE)
    if (inherits(locs_input, "try-error")) {
      stop("locs cannot be convertible to sf.
      Please convert locs into a sf object to proceed.\n")
    }

    export_list <- c()
    package_list <-
      c("sf", "terra", "exactextractr", "data.table", "stars",
        "dplyr", "parallelly", "rlang", "amadeus", "future",
        "future.apply")
    if (!is.null(export_list_add)) {
      export_list <- append(export_list, export_list_add)
    }
    if (!is.null(package_list_add)) {
      package_list <- append(package_list, package_list_add)
    }

    # make clusters
    # doParallel::registerDoParallel(cores = nthreads)
    if (nthreads == 1) {
      future::plan(future::sequential)
    } else {
      future::plan(future::multicore, workers = nthreads)
    }
    idx_date_available <- seq_along(dates_available)
    list_date_available <-
      split(idx_date_available, idx_date_available)
    calc_results <-
      future.apply::future_lapply(
        list_date_available,
        FUN = function(datei) {
          options(sf_use_s2 = FALSE)
          # nolint start
          day_to_pick <- dates_available[datei]
          # nolint end
          day_to_pick <- as.Date(day_to_pick, format = "%Y%j")

          radiusindex <- seq_along(radius)
          radiusindexlist <- split(radiusindex, radiusindex)

          hdf_args <- c(hdf_args, list(date = day_to_pick))
          hdf_args <- c(hdf_args, list(path = hdf_args$from))
          # unified interface with rlang::inject
          vrt_today <-
            rlang::inject(preprocess(!!!hdf_args))

          if (sum(terra::nlyr(vrt_today)) != length(name_covariates)) {
            message("The number of layers in the input raster do not match
                    the length of name_covariates.\n")
          }

          res0 <-
            lapply(radiusindexlist,
              function(k) {
                name_radius <-
                  sprintf("%s%05d",
                          name_covariates,
                          radius[k])
                extracted <-
                  try(
                    calc_modis_daily(
                      locs = locs_input,
                      from = vrt_today,
                      locs_id = locs_id,
                      date = as.character(day_to_pick),
                      fun_summary = fun_summary,
                      name_extracted = name_radius,
                      radius = radius[k],
                      max_cells = max_cells,
                      geom = FALSE
                    )
                  )
                if (inherits(extracted, "try-error")) {
                  # coerce to avoid errors
                  error_df <- data.frame(
                    matrix(-99999,
                           ncol = length(name_radius) + 1,
                           nrow = nrow(locs_input))
                  )
                  error_df <- stats::setNames(error_df, c(locs_id, name_radius))
                  error_df[[locs_id]] <- unlist(locs_input[[locs_id]])
                  error_df$time <- day_to_pick
                  extracted <- error_df
                }
                return(extracted)
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

        },
        future.seed = TRUE
      )
    calc_results <- do.call(dplyr::bind_rows, calc_results)
    if (geom) {
      # merge
      calc_results_return <- merge(
        locs,
        calc_results,
        by = locs_id
      )
      if ("sf" %in% class(calc_results_return)) {
        calc_results_return <- terra::vect(calc_results_return)
      }
    } else {
      calc_results_return <- calc_results
    }
    attr(calc_results_return, "dates_dropped") <- dates_insuf
    Sys.sleep(1L)
    return(calc_results_return)
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
#'  Default is \code{seq(2018L, 2022L)}.
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders.
#' @return a data.frame or SpatVector object
#' @author Insang Song
#' @importFrom methods is
#' @importFrom data.table year
#' @importFrom data.table month
#' @importFrom data.table as.data.table
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_temporal_dummies(
#'   locs = loc,
#'   locs_id = "id",
#'   year = seq(2018L, 2022L)
#' )
#' }
#' @export
calc_temporal_dummies <-
  function(
    locs,
    locs_id = "site_id",
    year = seq(2018L, 2022L),
    geom = FALSE,
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

    calc_check_time(covar = locs, POSIXt = TRUE)
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

    # weekday (starts from 0 - Sunday)
    vec_wday <- as.POSIXlt(locs$time)$wday
    # subtracting 1 due to the difference in the base
    dt_wday_dum <- dummify(vec_wday, seq(1L, 7L) - 1)
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

    # geom
    locs_return <- calc_return_locs(
      covar = locs_dums,
      POSIXt = TRUE,
      geom = geom,
      crs = "EPSG:4326"
    )
    return(locs_return)
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
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @return a data.frame (tibble) or SpatVector object with input field names with
#'  a suffix \code{"_sedc"} where the sums of EDC are stored.
#'  Additional attributes are attached for the EDC information.
#'    - `attr(result, "sedc_bandwidth")``: the bandwidth where
#'  concentration reduces to approximately five percent
#'    - `attr(result, "sedc_threshold")``: the threshold distance
#'  at which emission source points are excluded beyond that
#' @note The function is originally from
#' [chopin](https://github.com/kyle-messier/chopin)
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
    target_fields = NULL,
    geom = FALSE
  ) {
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
      # exp(-3) is about 0.05 * (value at origin)
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

    if (geom) {
      res_sedc <- merge(
        terra::as.data.frame(locs, geom = "WKT")[, c("site_id", "geometry")],
        res_sedc,
        "site_id"
      )
    }

    res_sedc_return <- calc_return_locs(
      covar = res_sedc,
      POSIXt = TRUE,
      geom = geom,
      crs = terra::crs(from)
    )

    attr(res_sedc_return, "sedc_bandwidth") <- sedc_bandwidth
    attr(res_sedc_return, "sedc_threshold") <- sedc_bandwidth * 2

    return(res_sedc_return)
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
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders.
#' @author Insang Song, Mariana Kassien
#' @return a data.frame or SpatVector object
#' @note U.S. context.
#' @seealso [`calc_sedc`], [`process_tri`]
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
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_tri(
#'   from = tri, # derived from process_tri() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = c(1e3L, 1e4L, 5e4L)
#' )
#' }
#' @export
calc_tri <- function(
  from = NULL,
  locs,
  locs_id = "site_id",
  radius = c(1e3L, 1e4L, 5e4L),
  geom = FALSE,
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
    Map(
      function(x) {
        locs_tri_s <-
          calc_sedc(
            locs = locs_re,
            from = from,
            locs_id = locs_id,
            sedc_bandwidth = x,
            target_fields = tri_cols,
            geom = FALSE
          )
        return(locs_tri_s)
      },
      list_radius
    )

  # bind element data.frames into one
  df_tri <- Reduce(function(x, y) dplyr::full_join(x, y), list_locs_tri)
  if (nrow(df_tri) != nrow(locs)) {
    df_tri <- dplyr::left_join(as.data.frame(locs), df_tri)
  }

  df_tri_return <- calc_return_locs(
    covar = df_tri,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )

  # read attr
  df_tri_return$time <- as.integer(attr(from, "tri_year"))

  return(df_tri_return)
}


#' Calculate road emissions covariates
#' @param from SpatVector(1). Output of \code{process_nei()}.
#' @param locs sf/SpatVector. Locations at NEI values are joined.
#' @param locs_id character(1). Unique site identifier column name.
#' Unused but kept for compatibility.
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders.
#' @author Insang Song, Ranadeep Daw
#' @seealso [`process_nei`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom methods is
#' @importFrom terra project
#' @importFrom terra intersect
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_nei(
#'   from = nei, # derived from process_nei example,
#'   locs = loc,
#'   locs_id = "id"
#' )
#' }
#' @export
calc_nei <- function(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  geom = FALSE,
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
  locs_re <- as.data.frame(locs_re)

  locs_return <- calc_return_locs(
    covar = locs_re,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(locs_return)
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
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders.
#' @seealso [process_hms()]
#' @author Mitchell Manware
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_hms(
#'   from = hms, # derived from process_hms() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   geom = FALSE
#' )
#' }
#' @export
calc_hms <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0,
    geom = FALSE,
    ...) {
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### from == character indicates no wildfire smoke plumes are present
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
        "time",
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
  #### prepare locations list
  sites_list <- calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
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
    sub_hyphen = FALSE
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
      "Calculating ",
      tolower(
        as.character(
          layer_name
        )
      ),
      " covariates for ",
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
    binary_colname <- paste0(
      tolower(
        layer_name
      ),
      "_",
      radius
    )
    #### define column names
    if (geom) {
      colnames(sites_extracted_layer) <- c(
        locs_id,
        "geometry",
        "time",
        binary_colname
      )
    } else {
      colnames(sites_extracted_layer) <- c(
        locs_id,
        "time",
        binary_colname
      )
    }
    #### merge with empty sites_extracted
    sites_extracted <- rbind(
      sites_extracted,
      sites_extracted_layer
    )
  }
  #### check for missing dates (missing polygons)
  if (!(identical(date_sequence, sort(unique(from$Date))))) {
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
  sites_extracted[[binary_colname]] <-
    as.integer(sites_extracted[[binary_colname]])
  #### date to POSIXct
  sites_extracted$time <- as.POSIXct(sites_extracted$time)
  #### order by date
  sites_extracted_ordered <- as.data.frame(
    sites_extracted[order(sites_extracted$time), ]
  )
  cat(paste0(
    "Returning ",
    layer_name,
    " covariates.\n"
  ))
  sites_extracted_ordered <- calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_extracted_ordered)
}

#' Calculate elevation covariates
#' @description
#' Extract elevation values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id}, year of release, and elevation variable.
#' Elevation variable column name reflects the elevation statistic, spatial
#' resolution of \code{from}, and circular buffer radius (ie. Breakline Emphasis
#' at 7.5 arc-second resolution with 0 meter buffer: breakline_emphasis_r75_0).
#' @param from SpatRaster(1). Output from \code{process_gmted()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders
#' @author Mitchell Manware
#' @seealso [`process_gmted()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_gmted(
#'   from = gmted, # derived from process_gmted() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calc_gmted <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0,
    fun = "mean",
    geom = FALSE,
    ...) {
  #### prepare locations list
  sites_list <- calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- calc_worker(
    dataset = "gmted",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 2,
    time = 3,
    time_type = "year"
  )
  #### variable column name
  statistic_codes <- c("be", "ds", "md", "mi", "mn", "mx", "sd")
  statistic_to <- c(
    "BRK", "SUB", "MED", "MEA", "MIN", "MAX", "STD"
  )
  name_from <- names(from)
  code_unique <-
    regmatches(
      name_from,
      regexpr(
        paste0("(",
               paste(statistic_codes, collapse = "|"),
               ")[0-9]{2,2}"),
        name_from
      )
    )
  statistic <- substr(code_unique, 1, 2)
  resolution <- substr(code_unique, 3, 4)
  statistic_to <-
    sprintf(
      "%s%s",
      statistic_to[match(statistic, statistic_codes)],
      resolution
    )

  variable_name <- paste0(
    statistic_to,
    "_",
    sprintf("%05d", as.integer(radius))
  )
  if (geom) {
    #### convert integer to numeric
    sites_extracted[, 4] <- as.numeric(sites_extracted[, 4])
    names(sites_extracted) <- c(locs_id, "geometry", "time", variable_name)
  } else {
    #### convert integer to numeric
    sites_extracted[, 3] <- as.numeric(sites_extracted[, 3])
    names(sites_extracted) <- c(locs_id, "time", variable_name)
  }
  sites_return <- calc_return_locs(
    covar = sites_extracted,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
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
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders
#' @author Mitchell Manware
#' @seealso [`process_narr`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_narr(
#'   from = narr, # derived from process_narr() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calc_narr <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0,
    fun = "mean",
    geom = FALSE,
    ...) {
  #### prepare locations list
  sites_list <- calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### identify pressure level or monolevel data
  if (grepl("level", names(from)[1])) {
    narr_time <- 3
    narr_level <- 2
  } else {
    narr_time <- 2
    narr_level <- NULL
  }
  #### perform extraction
  sites_extracted <- calc_worker(
    dataset = "narr",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = narr_time,
    time_type = "date",
    level = narr_level,
    ...
  )
  sites_return <- calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
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
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders.
#' @author Mitchell Manware
#' @seealso [process_geos()]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra buffer
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_geos(
#'   from = geos, # derived from process_geos() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calc_geos <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0,
    fun = "mean",
    geom = FALSE,
    ...) {
  #### prepare locations list
  sites_list <- calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- calc_worker(
    dataset = "geos",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = c(3, 4),
    time_type = "hour",
    level = 2,
    ...
  )
  sites_return <- calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
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
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders
#' @author Mitchell Manware
#' @seealso [process_sedac_population()]
#' @return a data.frame or SpatVector object
#' @importFrom methods is
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_sedac_population(
#'   from = pop, # derived from process_sedac_population() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calc_sedac_population <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0,
    fun = "mean",
    geom = FALSE,
    ...) {
  #### prepare locations list
  sites_list <- calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### message information
  name_split <- strsplit(
    names(from),
    "_"
  )[[1]]
  cat(
    paste0(
      "Calculating population covariates for ",
      name_split[4],
      " at ",
      process_sedac_codes(
        paste0(
          name_split[5],
          "_",
          name_split[6]
        ),
        invert = TRUE
      ),
      " resolution...\n"
    )
  )
  #### perform extraction
  sites_extracted <- calc_worker(
    dataset = "skip",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 3,
    time = 4,
    time_type = "year",
    ...
  )
  sites_return <- calc_return_locs(
    covar = sites_extracted,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
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
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders.
# nolint start
#' @note Unit is km / sq km. The returned `data.frame` object contains a
#' `$time` column to represent the temporal range covered by the
#' dataset. For more information, see <https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1/metadata>.
# nolint end
#' @author Insang Song
#' @seealso [`process_sedac_groads`]
#' @return a data.frame or SpatVector object
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
#' @importFrom methods is
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_sedac_groads(
#'   from = groads, # derived from process_sedac_groads() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 1000,
#'   fun = "sum",
#'   geom = FALSE
#' )
#' }
#' @export
calc_sedac_groads <- function(
    from = NULL,
    locs = NULL,
    locs_id = NULL,
    radius = 1000,
    fun = "sum",
    geom = FALSE,
    ...) {
  #### check for null parameters
  if (radius <= 0) {
    stop("radius should be greater than 0.\n")
  }
  #### prepare locations list
  sites_list <- calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]

  from_re <- terra::project(from, terra::crs(sites_e))
  from_re <- from[terra::ext(sites_e), ]
  from_clip <- terra::intersect(sites_e, from_re)
  area_buffer <- sites_e[1, ]
  area_buffer <- terra::expanse(area_buffer)

  # assign road lengths to rlength field
  from_clip$rlength <- terra::perim(from_clip)
  from_clip <-
    aggregate(
      from_clip$rlength,
      by = from_clip[[locs_id]],
      FUN = fun,
      na.rm = TRUE
    )
  # linear unit conversion
  # if no unit is detected, set to 1
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
  #### time period
  from_clip$description <- "1980 - 2010"
  if (geom) {
    from_clip$geometry <- sites_list[[2]]$geometry
    from_clip_reorder <- from_clip[, c(1, 5, 4, 2, 3)]
  } else {
    #### reorder
    from_clip_reorder <- from_clip[, c(1, 4, 2, 3)]
  }
  sites_return <- calc_return_locs(
    covar = from_clip_reorder,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}

#' Calculate meteorological and atmospheric covariates
#' @description
#' Extract meteorological and atmospheric values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, date and hour, vertical
#' pressure level, and meteorological or atmospheric variable. Variable column
#' name reflects variable and circular buffer radius.
#' @param from SpatRaster(1). Output of \code{process_merra2()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders
#' @author Mitchell Manware
#' @seealso [calc_geos()], [process_merra2()]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra buffer
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_merra2(
#'   from = merra2, # derived from process_merra2() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calc_merra2 <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0,
    fun = "mean",
    geom = FALSE,
    ...) {
  #### prepare locations list
  sites_list <- calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### identify pressure level or monolevel data
  if (grepl("lev", names(from)[1])) {
    merra2_time <- c(3, 4)
    merra2_level <- 2
  } else {
    merra2_time <- c(2, 3)
    merra2_level <- NULL
  }
  #### perform extraction
  sites_extracted <- calc_worker(
    dataset = "merra2",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = merra2_time,
    time_type = "hour",
    level = merra2_level,
    ...
  )
  sites_return <- calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}

#' Calculate gridMET covariates
#' @description
#' Extract gridMET values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id} and gridMET variable. gridMET variable
#' column name reflects the gridMET variable and circular buffer radius.
#' @param from SpatRaster(1). Output from \code{process_gridmet()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders.
#' @author Mitchell Manware
#' @seealso [`process_gridmet()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_gridmet(
#'   from = gridmet, # derived from process_gridmet() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calc_gridmet <- function(
    from,
    locs,
    locs_id = NULL,
    radius = 0,
    fun = "mean",
    geom = FALSE,
    ...) {
  #### prepare locations list
  sites_list <- calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- calc_worker(
    dataset = "gridmet",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = 2,
    time_type = "date",
    ...
  )
  sites_return <- calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}

#' Calculate TerraClimate covariates
#' @description
#' Extract TerraClimate values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id} and TerraClimate variable. TerraClimate
#' variable column name reflects the TerraClimate variable and
#' circular buffer radius.
#' @param from SpatRaster(1). Output from \code{process_terraclimate()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.`
#' @param ... Placeholders.
#' @note
#' TerraClimate data has monthly temporal resolution, so the `$time` column
#' will contain the year and month in YYYYMM format (ie. January, 2018 =
#' 201801).
#' @author Mitchell Manware
#' @seealso [`process_terraclimate()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calc_terraclimate(
#'   from = terraclimate, # derived from process_terraclimate() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calc_terraclimate <- function(
    from = NULL,
    locs = NULL,
    locs_id = NULL,
    radius = 0,
    fun = "mean",
    geom = FALSE,
    ...) {
  #### prepare locations list
  sites_list <- calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- calc_worker(
    dataset = "terraclimate",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = 2,
    time_type = "yearmonth",
    ...
  )
  sites_return <- calc_return_locs(
    covar = sites_extracted,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}

# nolint start
#' Calculate temporally lagged covariates
#' @description
#' The \code{calc_lagged()} function calculates daily temporal lagged covariates
#' from the output of \code{calculate_covariates()} or \code{calc_*()}.
#' @param from data.frame(1). A `data.frame` containing calculated covariates
#' returned from \code{calculate_covariates()} or \code{calc_*()}.
#' @param date character(2). Start and end dates of desired lagged covariates.
#' Length of 10 each, format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param lag integer(1). Number of lag days.
#' @param time_id character(1). Column containing time values.
#' @param locs_id character(1). Name of unique identifier.
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.` To return as a `SpatVector`, `from` must also be a `SpatVector`
#' @seealso [calc_covariates()]
#' @note
#' In order to calculate temporally lagged covariates, `from` must contain at
#' least the number of lag days before the desired start date. For example, if
#' `date = c("2024-01-01", "2024-01-31)` and `lag = 1`, `from` must contain data
#' starting at 2023-12-31.
#' If `from` contains geometry features, `calc_lagged` will return a column
#' with geometry features of the same name.
#' \code{calc_lagged()} assumes that all columns other than `time_id`,
#' `locs_id`, and fixed columns of "lat" and "lon", follow the genre, variable,
#' lag, buffer radius format adopted in \code{calc_setcolumns()}.
#' @return a `data.frame` object
#' @importFrom dplyr lag
#' @importFrom dplyr select
#' @examples
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' terracliamte_covar <- calc_terraclimate(
#'   from = terraclimate, # derived from process_terraclimate() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' calc_lagged(
#'   from = terracliamte_covar,
#'   locs_id = "id",
#'   date = c("2023-01-02", "2023-01-10"),
#'   lag = 1,
#'   time_id = "time"
#' )
#' }
# nolint end
#' @export
calc_lagged <- function(
    from,
    date,
    lag,
    locs_id,
    time_id = "time",
    geom = FALSE) {
  #### geom and from
  if (geom && !("SpatVector" %in% class(from))) {
    stop(
      paste0(
        "To return with geometry, `from` must be a `SpatVector` object.\n"
      )
    )
  }
  #### check input data types
  if ("SpatVector" %in% class(from)) {
    from_full <- terra::as.data.frame(from, geom = "WKT")
    geoms <- unique(from_full[, c("site_id", "geometry")])
    from <- from_full |> dplyr::select(-"geometry")
  }
  stopifnot(methods::is(from, "data.frame"))
  #### check if time_id is not null
  stopifnot(!is.null(time_id))
  #### return from if lag == 0
  if (lag == 0) {
    cat("`lag` set to 0. Returning `from`.\n")
    return(from)
  }
  #### extract times
  time <- as.character(from[[time_id]])
  dateseq <- seq(as.Date(date[1]) - lag, as.Date(date[2]), by = 1)
  dateseq <- as.character(dateseq)
  align <- setdiff(dateseq, unique(time))
  ### check temporal alignment
  if (length(align) > 0) {
    stop(
      "Dates requested in `date` do not align with data available in `from`."
    )
  }
  unique_locs <- unique(from[[locs_id]])
  variables_merge <- NULL
  for (u in seq_along(unique_locs)) {
    from_u <- subset(
      from,
      from[[locs_id]] == unique_locs[u]
    )
    time_u <- from_u[[time_id]]
    #### extract variables
    variables <- from_u[
      , !(names(from_u) %in% c(locs_id, time_id)),
      drop = FALSE
    ]
    #### apply lag using dplyr::lag
    variables_lag <- dplyr::lag(variables, lag, default = NA)
    colnames(variables_lag) <- gsub(
      paste0("_[0-9]{1}_"),
      paste0("_", lag, "_"),
      colnames(variables_lag)
    )
    #### create the return dataframe
    variables_return <- cbind(from_u[[locs_id]], time_u, variables_lag)
    colnames(variables_return)[1:2] <- c(locs_id, time_id)
    #### identify dates of interest
    date_sequence <- generate_date_sequence(
      date[1],
      date[2],
      sub_hyphen = FALSE
    )
    #### filter to dates of interest
    variables_return_date <- variables_return[time_u %in% date_sequence, ]
    #### merge with other locations
    variables_merge <- rbind(variables_merge, variables_return_date)
  }
  if (geom) {
    variables_merge <- merge(variables_merge, geoms)
  }
  variables_return <- calc_return_locs(
    covar = variables_merge,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(variables_return)
}
