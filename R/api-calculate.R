################################################################################
# Primary {calculate_covariates} function.
################################################################################

################################################################################
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
#' @param from character, SpatRaster, SpatVector, or data.frame depending on
#'   the selected `covariate` route.
#' @param locs sf/SpatVector. Unique locations. Should include
#'  a unique identifier field named `locs_id`
#' @param locs_id character(1). Name of unique identifier.
#'  Default is `"site_id"`.
#' @param .by_time NULL or character(1). Name of the time column to use
#'   temporal summarization unit token. \code{NULL} (default) disables
#'   temporal summarization.
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Passed through to the underlying source-specific function for
#'   weighted extraction. If `NULL` (default), unweighted extraction is
#'   performed.
#' @param ... Arguments passed to each covariate calculation
#'  function.
#' @note `covariate` argument value is converted to lowercase.
#' @seealso
#' * \code{\link{calculate_modis}}: "modis", "MODIS"
#' * \code{\link{calculate_koppen_geiger}}: "koppen-geiger", "koeppen-geiger", "koppen"
#' * \code{\link{calculate_ecoregion}}: "ecoregion", "ecoregions"
#' * \code{\link{calculate_temporal_dummies}}: "dummies", "Dummies"
#' * \code{\link{calculate_hms}}: "hms", "smoke", "HMS"
#' * \code{\link{calculate_gmted}}: "gmted", "GMTED"
#' * \code{\link{calculate_narr}}: "narr", "NARR"
#' * \code{\link{calculate_geos}}: "geos", "geos_cf", "GEOS"
#' * \code{\link{calculate_goes}}: "goes", "goes_adp", "GOES"
#' * \code{\link{calculate_population}}: "population", "sedac_population"
#' * \code{\link{calculate_groads}}: "roads", "groads", "sedac_groads"
#' * \code{\link{calculate_nlcd}}: "nlcd", "NLCD"
#' * \code{\link{calculate_tri}}: "tri", "TRI"
#' * \code{\link{calculate_nei}}: "nei", "NEI"
#' * \code{\link{calculate_merra2}}: "merra", "MERRA", "merra2", "MERRA2"
#' * \code{\link{calculate_gridmet}}: "gridMET", "gridmet"
#' * \code{\link{calculate_terraclimate}}: "terraclimate", "TerraClimate"
#' * \code{\link{calculate_prism}}: "prism", "PRISM"
#' * \code{\link{calculate_cropscape}}: "cropscape", "cdl"
#' * \code{\link{calculate_huc}}: "huc", "HUC"
#' * \code{\link{calculate_edgar}}: "edgar"
#' * \code{\link{calculate_drought}}: "drought", "spei", "eddi", "usdm"
#' @return Calculated covariates as a data.frame or SpatVector object
#' @author Insang Song
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_covariates(
#'   covariate = "narr",
#'   from = narr, # derived from process_covariates() example
#'   locs = loc,
#'   locs_id = "id",
#'   geom = FALSE
#' )
#' }
#' @export
# nolint end
calculate_covariates <-
  function(
    covariate = c(
      "modis",
      "koppen-geiger",
      "koeppen-geiger",
      "koppen",
      "koeppen",
      "geos",
      "dummies",
      "gmted",
      "sedac_groads",
      "groads",
      "roads",
      "ecoregions",
      "ecoregion",
      "hms",
      "smoke",
      "gmted",
      "narr",
      "geos",
      "sedac_population",
      "population",
      "nlcd",
      "merra",
      "merra2",
      "gridmet",
      "terraclimate",
      "tri",
      "nei",
      "mcd14ml",
      "prism",
      "cropscape",
      "cdl",
      "huc",
      "edgar",
      "goes",
      "goes_adp",
      "GOES",
      "drought",
      "spei",
      "eddi",
      "usdm"
    ),
    from,
    locs,
    locs_id = "site_id",
    .by_time = NULL,
    weights = NULL,
    ...
  ) {
    amadeus::check_unsupported_by(..., .call = sys.call())
    amadeus::check_by_time(.by_time)
    covariate <- tolower(covariate)
    covariate <- match.arg(covariate)
    if (startsWith(covariate, "ko")) {
      covariate <- "koppen"
    }

    # select function to run
    what_to_run <- switch(
      covariate,
      modis = amadeus::calculate_modis,
      ecoregion = amadeus::calculate_ecoregion,
      ecoregions = amadeus::calculate_ecoregion,
      koppen = amadeus::calculate_koppen_geiger,
      narr = amadeus::calculate_narr,
      nlcd = amadeus::calculate_nlcd,
      smoke = amadeus::calculate_hms,
      hms = amadeus::calculate_hms,
      sedac_groads = amadeus::calculate_groads,
      roads = amadeus::calculate_groads,
      groads = amadeus::calculate_groads,
      sedac_population = amadeus::calculate_population,
      population = amadeus::calculate_population,
      nei = amadeus::calculate_nei,
      mcd14ml = amadeus::calculate_modis,
      tri = amadeus::calculate_tri,
      geos = amadeus::calculate_geos,
      gmted = amadeus::calculate_gmted,
      dummies = amadeus::calculate_temporal_dummies,
      merra = amadeus::calculate_merra2,
      merra2 = amadeus::calculate_merra2,
      gridmet = amadeus::calculate_gridmet,
      terraclimate = amadeus::calculate_terraclimate,
      prism = amadeus::calculate_prism,
      cropscape = amadeus::calculate_cropscape,
      cdl = amadeus::calculate_cropscape,
      huc = amadeus::calculate_huc,
      edgar = amadeus::calculate_edgar,
      goes = amadeus::calculate_goes,
      goes_adp = amadeus::calculate_goes,
      drought = amadeus::calculate_drought,
      spei = amadeus::calculate_drought,
      eddi = amadeus::calculate_drought,
      usdm = amadeus::calculate_drought
    )

    res_covariate <-
      tryCatch(
        {
          calc_args <- c(
            list(from = from, locs = locs, locs_id = locs_id),
            list(...)
          )
          if (!is.null(weights)) {
            calc_args$weights <- weights
          }
          if (!is.null(.by_time)) {
            calc_args$.by_time <- .by_time
          }
          do.call(what_to_run, calc_args)
        },
        error = function(e) {
          stop(
            paste0(
              e,
              "\n",
              paste0(deparse(args(what_to_run)), collapse = "\n"),
              "\n",
              "Please refer to the argument list and ",
              "the error message above to rectify the error.\n"
            )
          )
        }
      )

    return(res_covariate)
  }

################################################################################
# General {calculate} functions.
################################################################################
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
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @return a data.frame or SpatVector object
#' @author Insang Song
#' @importFrom methods is
#' @importFrom data.table year
#' @importFrom data.table month
#' @importFrom data.table as.data.table
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_temporal_dummies(
#'   locs = loc,
#'   locs_id = "id",
#'   year = seq(2018L, 2022L)
#' )
#' }
#' @export
calculate_temporal_dummies <-
  function(
    locs,
    locs_id = "site_id",
    year = seq(2018L, 2022L),
    weights = NULL,
    geom = FALSE,
    ...
  ) {
    amadeus::check_unsupported_by(..., .call = sys.call())
    amadeus::check_geom(geom)
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
        lapply(vec_split, function(x) {
          as.integer(vec == x)
        })
      dt_dum <- Reduce(cbind, vec_assigned)
      dt_dum <- data.table::as.data.table(dt_dum)
      return(dt_dum)
    }

    amadeus::calc_check_time(covar = locs, POSIXt = TRUE)
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
      c(
        "JANUA",
        "FEBRU",
        "MARCH",
        "APRIL",
        "MAYMA",
        "JUNEJ",
        "JULYJ",
        "AUGUS",
        "SEPTE",
        "OCTOB",
        "NOVEM",
        "DECEM"
      )
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
    locs_return <- amadeus::calc_return_locs(
      covar = locs_dums,
      POSIXt = TRUE,
      geom = geom,
      crs = "EPSG:4326"
    )
    return(locs_return)
  }

################################################################################
# nolint start
#' Calculate isotropic Sum of Exponentially Decaying Contributions (SEDC) covariates
#' @param from `SpatVector`(1). Point locations which contain point-source
#' covariate data.
#' @param locs sf/SpatVector(1). Locations where the sum of exponentially
#' decaying contributions are calculated.
#' @param locs_id character(1). Name of the unique id field in `point_to`.
#' @param decay_range numeric(1).
#' Distance at which the source concentration is reduced to
#'  `exp(-3)` (approximately -95 %)
#' @param target_fields character(varying). Field names in characters.
#' @param C0 `NULL`, character(1), or numeric vector of length `nrow(from)`.
#' Optional initial source values at pollutant locations. If `NULL`
#' (default), all source values are set to 1. If character(1), the value
#' is treated as a column name in `from` and used as source values.
#' @param use_threshold logical(1). If `TRUE` (default), include only source
#' points within \code{5 * decay_range} from each target location. If `FALSE`,
#' include all source points in `from`.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @return a data.frame (tibble) or SpatVector object with input field names with
#'  a suffix \code{"_sedc"} where the sums of EDC are stored.
#'  Additional attributes are attached for the EDC information.
#'    - `attr(result, "decay_range")``: the range where
#'  concentration reduces to approximately five percent
#'    - `attr(result, "sedc_threshold")``: the threshold distance
#'  at which emission source points are excluded beyond that
#' @note The function is originally from
#' [chopin](https://github.com/ropensci/chopin)
#' Distance calculation is done with terra functions internally.
#'  Thus, the function internally converts sf objects in
#'  \code{point_*} arguments to terra.
#'  The threshold should be carefully chosen by users.
#' @author Insang Song
#' @references
#' \insertRef{messier2012integrating}{amadeus}
#'
#' \insertRef{web_sedctutorial_package}{amadeus}
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' set.seed(101)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#' nc <- terra::project(nc, "EPSG:5070")
#' pnt_locs <- terra::centroids(nc, inside = TRUE)
#' pnt_locs <- pnt_locs[, "NAME"]
#' pnt_from <- terra::spatSample(nc, 10L)
#' pnt_from$pid <- seq(1, 10)
#' pnt_from <- pnt_from[, "pid"]
#' pnt_from$val1 <- rgamma(10L, 1, 0.05)
#' pnt_from$val2 <- rgamma(10L, 2, 1)
#'
#' vals <- c("val1", "val2")
#' sum_edc(pnt_locs, pnt_from, "NAME", 1e4, vals)
#' }
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
sum_edc <-
  function(
    from = NULL,
    locs = NULL,
    locs_id = NULL,
    decay_range = NULL,
    target_fields = NULL,
    C0 = NULL, # nolint: object_name_linter
    use_threshold = TRUE,
    geom = FALSE
  ) {
    amadeus::check_geom(geom)
    if (!methods::is(locs, "SpatVector")) {
      locs <- try(terra::vect(locs), silent = TRUE)
    }
    if (!methods::is(from, "SpatVector")) {
      from <- try(terra::vect(from), silent = TRUE)
    }

    if (
      !is.numeric(decay_range) ||
        length(decay_range) != 1L ||
        is.na(decay_range) ||
        decay_range <= 0
    ) {
      stop("`decay_range` must be a single positive numeric value.\n")
    }
    if (!is.character(target_fields) || length(target_fields) < 1L) {
      stop("`target_fields` must be a non-empty character vector.\n")
    }
    if (
      !is.logical(use_threshold) ||
        length(use_threshold) != 1L ||
        is.na(use_threshold)
    ) {
      stop("`use_threshold` must be TRUE or FALSE.\n")
    }
    missing_targets <- setdiff(target_fields, names(from))
    if (length(missing_targets) > 0L) {
      stop(
        "The following `target_fields` are missing in `from`: ",
        paste(missing_targets, collapse = ", "),
        "\n"
      )
    }
    c0_values <- C0
    if (is.null(c0_values)) {
      from$C0_source <- rep(1, nrow(from))
    } else {
      if (is.character(c0_values)) {
        if (
          length(c0_values) != 1L ||
            anyNA(c0_values) ||
            !nzchar(trimws(c0_values))
        ) {
          stop("`C0` as character must be a single non-empty column name.\n")
        }
        if (!c0_values %in% names(from)) {
          stop("`C0` column `", c0_values, "` was not found in `from`.\n")
        }
        c0_values <- from[[c0_values]]
      }
      if (is.data.frame(c0_values) && ncol(c0_values) == 1L) {
        c0_values <- c0_values[[1]]
      }
      if (!is.numeric(c0_values) || length(c0_values) != nrow(from)) {
        stop(
          "`C0` must be NULL, character(1) column name in `from`, ",
          "or a numeric vector with length `nrow(from)`.\n"
        )
      }
      if (length(target_fields) != 1L) {
        stop("When `C0` is provided, `target_fields` must have length 1.\n")
      }
      from$C0_source <- c0_values
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
    threshold_distance <- if (use_threshold) decay_range * 5 else Inf

    locs$from_id <- len_point_locs
    if (use_threshold) {
      locs_buf <-
        terra::buffer(
          locs,
          width = threshold_distance,
          quadsegs = 90
        )
      from_in <- from[locs_buf, ]
    } else {
      from_in <- from
    }
    if (nrow(from_in) < 1L) {
      res_sedc <- data.frame(locs_id = terra::as.data.frame(locs)[[locs_id]])
      names(res_sedc)[1] <- locs_id
      for (target_field in target_fields) {
        res_sedc[[target_field]] <- 0
      }
      idx_air <- grep("_AIR_", names(res_sedc))
      names(res_sedc)[idx_air] <-
        sprintf("%s_%05d", names(res_sedc)[idx_air], decay_range)

      if (geom %in% c("sf", "terra")) {
        res_sedc <- merge(
          terra::as.data.frame(locs, geom = "WKT")[, c(locs_id, "geometry")],
          res_sedc,
          locs_id
        )
      }
      res_sedc_return <- amadeus::calc_return_locs(
        covar = res_sedc,
        POSIXt = TRUE,
        geom = geom,
        crs = terra::crs(from)
      )
      attr(res_sedc_return, "decay_range") <- decay_range
      attr(res_sedc_return, "sedc_threshold") <- threshold_distance
      return(res_sedc_return)
    }
    len_point_from <- seq_len(nrow(from_in))

    # len point from? len point to?
    from_in$to_id <- len_point_from
    dist <- NULL

    # near features with distance argument: only returns integer indices
    if (use_threshold) {
      res_nearby <-
        terra::nearby(locs, from_in, distance = threshold_distance)
    } else {
      res_nearby <-
        expand.grid(
          from_id = len_point_locs,
          to_id = len_point_from
        )
    }
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
      dplyr::mutate(w_sedc = exp((-3 * dist) / decay_range)) |>
      dplyr::group_by(!!rlang::sym(locs_id)) |>
      dplyr::summarize(
        dplyr::across(
          dplyr::all_of(target_fields),
          ~ sum(w_sedc * C0_source, na.rm = TRUE)
        )
      ) |>
      dplyr::ungroup()
    idx_air <- grep("_AIR_", names(res_sedc))
    names(res_sedc)[idx_air] <-
      sprintf("%s_%05d", names(res_sedc)[idx_air], decay_range)

    if (geom %in% c("sf", "terra")) {
      res_sedc <- merge(
        terra::as.data.frame(locs, geom = "WKT")[, c(locs_id, "geometry")],
        res_sedc,
        locs_id
      )
    }

    res_sedc_return <- amadeus::calc_return_locs(
      covar = res_sedc,
      POSIXt = TRUE,
      geom = geom,
      crs = terra::crs(from)
    )

    attr(res_sedc_return, "decay_range") <- decay_range
    attr(res_sedc_return, "sedc_threshold") <- threshold_distance

    return(res_sedc_return)
  }

################################################################################
# nolint start
#' Calculate temporally lagged covariates
#' @description
#' The \code{calculate_lagged()} function calculates daily temporal lagged covariates
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
#' @seealso [calculate_covariates()]
#' @note
#' In order to calculate temporally lagged covariates, `from` must contain at
#' least the number of lag days before the desired start date. For example, if
#' `date = c("2024-01-01", "2024-01-31)` and `lag = 1`, `from` must contain data
#' starting at 2023-12-31.
#' If `from` contains geometry features, `calculate_lagged` will return a column
#' with geometry features of the same name.
#' \code{calculate_lagged()} assumes that all columns other than `time_id`,
#' `locs_id`, and fixed columns of "lat" and "lon", follow the genre, variable,
#' lag, buffer radius format adopted in \code{calc_setcolumns()}.
#' @return a `data.frame` object
#' @importFrom dplyr lag
#' @importFrom dplyr select
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' terracliamte_covar <- calculate_terraclimate(
#'   from = terraclimate, # derived from process_terraclimate() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' calculate_lagged(
#'   from = terracliamte_covar,
#'   locs_id = "id",
#'   date = c("2023-01-02", "2023-01-10"),
#'   lag = 1,
#'   time_id = "time"
#' )
#' }
# nolint end
#' @export
calculate_lagged <- function(
  from,
  date,
  lag,
  locs_id,
  time_id = "time",
  geom = FALSE
) {
  amadeus::check_geom(geom)
  #### check years
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### geom and from
  if (geom %in% c("sf", "terra") && !("SpatVector" %in% class(from))) {
    stop(
      paste0(
        "To return with geometry, `from` must be a `SpatVector` object.\n"
      )
    )
  }
  #### check input data types
  if ("SpatVector" %in% class(from)) {
    from_full <- terra::as.data.frame(from, geom = "WKT")
    geoms <- unique(from_full[, c(locs_id, "geometry")])
    from <- from_full |> dplyr::select(-"geometry")
  }
  stopifnot(methods::is(from, "data.frame"))
  #### check if time_id is not null
  stopifnot(!is.null(time_id))
  #### return from if lag == 0
  if (lag == 0) {
    message("`lag` set to 0. Returning `from`.\n")
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
    variables <- from_u[,
      !(names(from_u) %in% c(locs_id, time_id)),
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
    date_sequence <- amadeus::generate_date_sequence(
      date[1],
      date[2],
      sub_hyphen = FALSE
    )
    #### filter to dates of interest
    variables_return_date <- variables_return[time_u %in% date_sequence, ]
    #### merge with other locations
    variables_merge <- rbind(variables_merge, variables_return_date)
  }
  if (geom %in% c("sf", "terra")) {
    variables_merge <- merge(variables_merge, geoms)
  }
  variables_return <- amadeus::calc_return_locs(
    covar = variables_merge,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(variables_return)
}
