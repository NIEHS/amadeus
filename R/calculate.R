################################################################################
# Primary {calculate_covariates} function.
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

    if (!is.numeric(decay_range) || length(decay_range) != 1L ||
          is.na(decay_range) || decay_range <= 0) {
      stop("`decay_range` must be a single positive numeric value.\n")
    }
    if (!is.character(target_fields) || length(target_fields) < 1L) {
      stop("`target_fields` must be a non-empty character vector.\n")
    }
    if (!is.logical(use_threshold) || length(use_threshold) != 1L ||
          is.na(use_threshold)) {
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
        if (length(c0_values) != 1L || anyNA(c0_values) ||
              !nzchar(trimws(c0_values))) {
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

################################################################################
# Support and internal {calculate} functions.
################################################################################
#' Reject deprecated legacy grouping argument in dots
#' @description
#' Internal helper for calculate APIs that now support temporal
#' summarization via \code{.by_time} only. Stops immediately when a
#' deprecated legacy grouping argument is supplied through \code{...}.
#' @param ... Placeholders.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return \code{NULL} invisibly; stops on deprecated legacy grouping input.
#' @export
check_unsupported_by <- function(..., .call = NULL) {
  dots <- list(...)
  call_names <- character(0)
  if (!is.null(.call)) {
    call_names <- names(as.list(.call)[-1])
  }
  if (".by" %in% names(dots) || ".by" %in% call_names) {
    stop_legacy_by_error()
  }
  invisible(NULL)
}

################################################################################
#' Build standardized error for legacy grouping usage
#' @keywords internal
#' @noRd
stop_legacy_by_error <- function() {
  stop(
    paste0(
      "`.by` is no longer supported in calculate APIs. ",
      "Use `.by_time` for temporal summarization (e.g., 'day', ",
      "'week', 'month', or 'year').\n"
    )
  )
}

################################################################################
#' Validate the `.by_time` temporal summarization argument
#' @description
#' Validates the \code{.by_time} argument used by covariate extraction
#' functions for temporal summarization. When non-\code{NULL},
#' \code{.by_time} must be a single character string naming a supported
#' temporal unit token (singular or plural): \code{"minute"},
#' \code{"hour"}, \code{"day"}, \code{"week"}, \code{"month"},
#' \code{"quarter"}, or \code{"year"}.
#' @param .by_time NULL or character(1). Temporal summarization unit.
#'   \code{NULL} means no temporal summarization.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return \code{NULL} invisibly; stops with an informative error if the
#'   value is invalid.
#' @export
check_by_time <- function(.by_time) {
  if (is.null(.by_time)) {
    return(invisible(NULL))
  }
  if (!is.character(.by_time) || length(.by_time) != 1L) {
    stop(
      paste0(
        "`.by_time` must be NULL or a single character string naming ",
        "a temporal unit.
"
      )
    )
  }
  allowed <- c(
    "minute",
    "minutes",
    "hour",
    "hours",
    "day",
    "days",
    "week",
    "weeks",
    "month",
    "months",
    "quarter",
    "quarters",
    "year",
    "years"
  )
  if (!.by_time %in% allowed) {
    stop(
      paste0(
        "`.by_time` must be one of: ",
        paste(allowed, collapse = ", "),
        ".
"
      )
    )
  }
  invisible(NULL)
}

################################################################################
#' Validate the `fun_temporal` parameter
#' @description
#' Validates the `fun_temporal` argument used by covariate
#' extraction functions.  When \code{NULL} (the default), no
#' temporal aggregation is applied and existing per-layer
#' extraction behavior is preserved.  When non-\code{NULL},
#' the value must be one of \code{"mean"}, \code{"median"},
#' \code{"sum"}, \code{"max"}, or \code{"min"}.
#' @param fun_temporal NULL or character(1). Name of the
#'   temporal summary function. \code{NULL} means no temporal
#'   aggregation (default / backward-compatible behavior).
#' @keywords internal auxiliary
#' @author Insang Song
#' @return \code{NULL} invisibly; stops with an informative
#'   error if the value is invalid.
#' @export
check_fun_temporal <- function(fun_temporal) {
  if (is.null(fun_temporal)) {
    return(invisible(NULL))
  }
  allowed <- c("mean", "median", "sum", "max", "min")
  if (
    !is.character(fun_temporal) ||
      length(fun_temporal) != 1L
  ) {
    stop(
      paste0(
        "`fun_temporal` must be NULL or a single ",
        "character string.\n"
      )
    )
  }
  if (!fun_temporal %in% allowed) {
    stop(
      sprintf(
        paste0(
          "`fun_temporal` must be one of: %s.\n"
        ),
        paste(allowed, collapse = ", ")
      )
    )
  }
  invisible(NULL)
}

################################################################################
#' Check that `geom` value is one of `FALSE`, `"sf"`, or `"terra"`
#' @description Check that `geom` value is one of `FALSE`, `"sf"`,
#' or `"terra"`.
#' @param geom FALSE/"sf"/"terra".'
#' @keywords internal auxiliary
#' @author Mitchell Manware
#' @return NULL; will stop if `geom` is not one of the three options
#' @export
check_geom <- function(geom) {
  if (!geom %in% c(FALSE, "sf", "terra")) {
    stop("`geom` must be one of FALSE, 'sf', or 'terra'.")
  }
}

################################################################################
#' Prepare extraction locations
#' @description
#' Prepare the point locations for extracting data by transforming `locs` to
#' a `SpatVector`, projecting to the coordinate reference system of `from`,
#' and creating a `data.frame` containing `locs_id` for retaining extracted
#' values.
#' @param from SpatRaster(1) or SpatVector(1). Output from
#' \code{process_\*()}. Passed from \code{calc_\*()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' Passed from \code{calc_\*()}.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' Passed from \code{calc_\*()}.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0). Passed from \code{calc_\*()}.
#' @param geom logical(1). Should the geometry of `locs` be returned in the
#' `data.frame`? Default is `FALSE`, options "sf" or "terra" will preserve
#' geometry, but will use `terra` for extraction.
#' @return A `list` containing `SpatVector` and `data.frame` objects
#' @seealso [`process_locs_vector()`], [`check_for_null_parameters()`]
#' @keywords internal auxiliary
#' @importFrom terra as.data.frame
#' @importFrom terra crs
#' @export
calc_prepare_locs <- function(
  from,
  locs,
  locs_id,
  radius,
  geom = FALSE
) {
  #### check for null parameters
  amadeus:::check_for_null_parameters(mget(ls()))
  if (!locs_id %in% names(locs)) {
    stop(sprintf("locs should include columns named %s.\n", locs_id))
  }
  locs_id_values <- as.data.frame(locs)[[locs_id]]
  #### prepare sites
  sites_e <- amadeus::process_locs_vector(
    locs,
    terra::crs(from),
    radius
  )
  #### site identifiers and geometry
  # check geom
  amadeus::check_geom(geom)
  if (geom %in% c("sf", "terra")) {
    geom <- TRUE
  }
  sites_df <- if (geom) {
    terra::as.data.frame(sites_e, geom = "WKT")
  } else {
    terra::as.data.frame(sites_e)
  }
  if (!locs_id %in% names(sites_df)) {
    if (nrow(sites_df) != length(locs_id_values)) {
      stop(
        paste0(
          "`locs_id` was not retained in prepared locations and could not ",
          "be reconstructed because row counts differ."
        )
      )
    }
    sites_df[[locs_id]] <- locs_id_values
  }
  if (geom) {
    sites_id <- subset(
      sites_df,
      select = c(locs_id, "geometry")
    )
  } else {
    #### site identifiers only
    sites_id <- subset(
      sites_df,
      select = locs_id
    )
  }
  return(list(sites_e, sites_id))
}

################################################################################
#' Prepare optional weighting raster
#' @param from SpatRaster(1). Template raster.
#' @param weights NULL, SpatRaster, SpatVector/sf polygon, or file path.
#' @return NULL or single-layer SpatRaster aligned to `from`.
#' @keywords internal auxiliary
#' @export
calc_prepare_weights <- function(from, weights = NULL) {
  if (is.null(weights)) {
    return(NULL)
  }
  if (!inherits(from, "SpatRaster")) {
    stop("`from` must be a SpatRaster when `weights` are supplied.")
  }

  normalize_vector_weights <- function(vect_weights) {
    if (terra::geomtype(vect_weights)[1] != "polygons") {
      stop(
        "`weights` vector input must contain polygons when supplied as ",
        "SpatVector/sf."
      )
    }
    from_crs <- terra::crs(from)
    if (is.na(from_crs) || from_crs == "") {
      stop("`from` is missing CRS; cannot validate weighted extraction CRS.")
    }
    vect_weights <- terra::project(vect_weights, from_crs)
    vect_df <- terra::as.data.frame(vect_weights)
    val_cols <- names(vect_weights)
    val_cols <- val_cols[sapply(val_cols, function(x) {
      is.numeric(vect_df[[x]])
    })]
    if (length(val_cols) == 0L) {
      vect_weights$.amadeus_weight <- 1
      val_col <- ".amadeus_weight"
    } else {
      val_col <- val_cols[1]
      if (length(val_cols) > 1L) {
        message(
          "Multiple numeric columns found in `weights`; using first column: ",
          val_col
        )
      }
    }
    if (any(vect_df[[val_col]] < 0, na.rm = TRUE)) {
      stop("`weights` values must be non-negative.")
    }
    weights_r <- terra::rasterize(
      vect_weights,
      from[[1]],
      field = val_col,
      background = NA,
      touches = TRUE
    )
    if (all(is.na(terra::values(weights_r)))) {
      stop("`weights` polygons do not overlap `from` extent.")
    }
    weights_r
  }

  weights_obj <- weights
  if (is.character(weights) && length(weights) == 1L) {
    weights_obj <- suppressWarnings(try(terra::rast(weights), silent = TRUE))
    if (inherits(weights_obj, "try-error")) {
      weights_obj <- suppressWarnings(try(terra::vect(weights), silent = TRUE))
      if (inherits(weights_obj, "try-error")) {
        stop("`weights` path could not be read as raster or vector data.")
      }
    }
  }

  if (inherits(weights_obj, c("sf", "sfc"))) {
    weights_obj <- terra::vect(weights_obj)
  }

  if (inherits(weights_obj, "SpatVector")) {
    return(normalize_vector_weights(weights_obj))
  }
  if (!inherits(weights_obj, "SpatRaster")) {
    stop(
      "`weights` must be NULL, SpatRaster, polygon SpatVector/sf, ",
      "or a file path to one of those."
    )
  }
  if (terra::nlyr(weights_obj) != 1L) {
    stop("`weights` raster must have exactly one layer.")
  }
  if (!is.numeric(terra::values(weights_obj)[, 1])) {
    stop("`weights` raster values must be numeric.")
  }
  if (any(terra::values(weights_obj)[, 1] < 0, na.rm = TRUE)) {
    stop("`weights` values must be non-negative.")
  }

  from_crs <- terra::crs(from)
  weights_crs <- terra::crs(weights_obj)
  if (is.na(from_crs) || from_crs == "") {
    stop("`from` is missing CRS; cannot validate weighted extraction CRS.")
  }
  if (is.na(weights_crs) || weights_crs == "") {
    stop("`weights` is missing CRS; cannot validate weighted extraction CRS.")
  }

  weights_re <- terra::project(weights_obj, from[[1]], method = "bilinear")
  if (!calc_extents_overlap(from[[1]], weights_re)) {
    stop("`weights` extent does not overlap `from` extent.")
  }
  terra::resample(weights_re, from[[1]], method = "bilinear")
}

################################################################################
#' Validate extents overlap
#' @param x SpatRaster(1)
#' @param y SpatRaster(1)
#' @return logical(1)
#' @keywords internal auxiliary
#' @export
calc_extents_overlap <- function(x, y) {
  ext_x <- as.vector(terra::ext(x))
  ext_y <- as.vector(terra::ext(y))
  !(ext_x[2] < ext_y[1] ||
      ext_y[2] < ext_x[1] ||
      ext_x[4] < ext_y[3] ||
      ext_y[4] < ext_x[3])
}

################################################################################
#' Convert point extractions to tiny polygons for exact extraction
#' @param locs_vector SpatVector(1)
#' @param radius numeric(1)
#' @return sf object for exactextractr
#' @keywords internal auxiliary
#' @export
calc_prepare_exact_geoms <- function(locs_vector, radius) {
  geom_type <- terra::geomtype(locs_vector)[1]
  if (geom_type == "polygons") {
    return(sf::st_as_sf(locs_vector))
  }
  if (geom_type != "points") {
    stop("Unsupported location geometry for weighted extraction.")
  }
  width <- as.numeric(radius)
  if (!is.finite(width) || width <= 0) {
    if (terra::is.lonlat(locs_vector)) {
      width <- 1e-6
    } else {
      width <- 1
    }
  }
  sf::st_as_sf(terra::buffer(locs_vector, width = width, quadsegs = 90L))
}

################################################################################
#' Resolve weighted summary function names for exactextractr
#' @param fun character(1)
#' @param weighted logical(1)
#' @return character(1)
#' @keywords internal auxiliary
#' @export
calc_weighted_fun <- function(fun, weighted = FALSE) {
  if (!weighted) {
    return(fun)
  }
  switch(
    fun,
    mean = "weighted_mean",
    sum = "weighted_sum",
    fun
  )
}

################################################################################
#' Prepare time values
#' @description
#' Prepare the time values for covariate calculation based on type of time
#' value.
#' @param time Time value
#' @param format Type of time to return in the `$time` column. Can be
#' "timeless" (ie. Ecoregions data), "date" (ie. NARR data), "hour"
#' (ie. GEOS data), "year" (ie. SEDAC population data), or "yearmonth"
#' (ie. TerraClimate data).
#' @return a `Date`, `POSIXt`, or `integer` object based on `format =`
#' @keywords internal auxiliary
#' @export
calc_time <- function(
  time,
  format,
  dataset = NULL,
  layer_name = NULL,
  layer_time = NULL
) {
  parse_gridmet_day_code <- function(x) {
    x_chr <- as.character(x)
    if (!grepl("^[0-9]+$", x_chr)) {
      return(as.Date(NA))
    }
    as.Date(as.numeric(x_chr), origin = "1900-01-01")
  }
  extract_ymd_from_text <- function(x) {
    x_chr <- as.character(x)
    x_digits <- gsub("[^0-9]", "", x_chr)
    if (grepl("^[0-9]{8}$", x_digits)) {
      return(as.Date(x_digits, format = "%Y%m%d"))
    }
    if (grepl("^[0-9]{7}$", x_digits)) {
      return(as.Date(x_digits, format = "%Y%j"))
    }
    as.Date(NA)
  }
  to_posixlt_utc <- function(x) {
    as.POSIXlt(as.POSIXct(x, tz = "UTC"))
  }
  extract_digits <- function(x) {
    token <- as.character(x)[1]
    if (is.na(token)) {
      return("")
    }
    gsub("[^0-9]", "", token)
  }
  has_layer_time <- !is.null(layer_time) &&
    length(layer_time) > 0 &&
    !all(is.na(layer_time))

  if (format == "timeless") {
    return(time)
  }

  if (has_layer_time) {
    if (format == "hour") {
      return(to_posixlt_utc(layer_time[1]))
    }
    if (format == "date") {
      return(to_posixlt_utc(as.Date(layer_time[1])))
    }
    if (format == "year") {
      return(as.integer(format(as.Date(layer_time[1]), "%Y")))
    }
    if (format == "yearmonth") {
      return(as.integer(format(as.Date(layer_time[1]), "%Y%m")))
    }
  }

  if (format == "date") {
    time_chr <- as.character(time[1])
    parsed <- extract_ymd_from_text(time_chr)
    if (!is.na(parsed)) {
      return(to_posixlt_utc(parsed))
    }
    if (!is.null(layer_name) && grepl("=[0-9]+$", layer_name)) {
      day_code <- sub(".*=([0-9]+)$", "\\1", layer_name)
      parsed <- parse_gridmet_day_code(day_code)
      if (!is.na(parsed)) {
        return(to_posixlt_utc(parsed))
      }
    }
    stop(
      sprintf(
        "Unable to parse date for dataset '%s' from token '%s' (layer '%s').\n",
        ifelse(is.null(dataset), "unknown", dataset),
        paste(time, collapse = "_"),
        ifelse(is.null(layer_name), "unknown", layer_name)
      )
    )
  }

  if (format == "hour") {
    time_chr <- as.character(time)
    if (length(time_chr) >= 2) {
      date_digits <- gsub("[^0-9]", "", time_chr[1])
      hour_digits <- gsub("[^0-9]", "", time_chr[2])
      if (
        !is.na(date_digits) &&
          !is.na(hour_digits) &&
          nchar(date_digits) == 8 &&
          nchar(hour_digits) >= 2
      ) {
        hour_digits <- sprintf("%06d", as.integer(substr(hour_digits, 1, 6)))
        return(
          to_posixlt_utc(ISOdatetime(
            year = substr(date_digits, 1, 4),
            month = substr(date_digits, 5, 6),
            day = substr(date_digits, 7, 8),
            hour = substr(hour_digits, 1, 2),
            min = substr(hour_digits, 3, 4),
            sec = substr(hour_digits, 5, 6),
            tz = "UTC"
          ))
        )
      }
    }
    full_digits <- gsub("[^0-9]", "", paste(time_chr, collapse = ""))
    if (nchar(full_digits) >= 10) {
      dt <- as.POSIXct(
        substr(full_digits, 1, 14),
        format = "%Y%m%d%H%M%S",
        tz = "UTC"
      )
      if (!is.na(dt)) {
        return(to_posixlt_utc(dt))
      }
    }
    stop(
      sprintf(
        paste0(
          "Unable to parse datetime for dataset '%s' from token '%s' ",
          "(layer '%s').\n"
        ),
        ifelse(is.null(dataset), "unknown", dataset),
        paste(time, collapse = "_"),
        ifelse(is.null(layer_name), "unknown", layer_name)
      )
    )
  }

  if (format == "yearmonth") {
    digits <- extract_digits(time)
    if (nchar(digits) >= 6) {
      return(as.integer(substr(digits, 1, 6)))
    }
    stop(
      sprintf(
        paste0(
          "Unable to parse year-month for dataset '%s' from token '%s' ",
          "(layer '%s').\n"
        ),
        ifelse(is.null(dataset), "unknown", dataset),
        paste(time, collapse = "_"),
        ifelse(is.null(layer_name), "unknown", layer_name)
      )
    )
  }

  if (format == "year") {
    digits <- extract_digits(time)
    if (nchar(digits) >= 4) {
      return(as.integer(substr(digits, 1, 4)))
    }
    stop(
      sprintf(
        "Unable to parse year for dataset '%s' from token '%s' (layer '%s').\n",
        ifelse(is.null(dataset), "unknown", dataset),
        paste(time, collapse = "_"),
        ifelse(is.null(layer_name), "unknown", layer_name)
      )
    )
  }

  stop("Unsupported time format.\n")
}

################################################################################
#' Send progress messages
#' @description
#' Send messages updating covariate extraction progress.
#' @param dataset character(1). Data source.
#' @param variable placeholder
#' @param time placeholder
#' @param time_type placeholder
#' @param level placeholder
#' @return NULL; provides progress messages to R console.
#' @keywords internal auxiliary
#' @export
calc_message <- function(
  dataset,
  variable,
  time,
  time_type,
  level,
  layer_time = NULL
) {
  message_time <- calc_time(
    time = time,
    format = time_type,
    dataset = dataset,
    layer_time = layer_time
  )
  if (dataset == "skip") {
    return()
  }
  if (dataset == "gmted") {
    return_message <- paste0(
      "Calculating ",
      amadeus::process_gmted_codes(
        substr(
          variable,
          1,
          2
        ),
        statistic = TRUE,
        invert = TRUE
      ),
      " covariates with ",
      amadeus::process_gmted_codes(
        substr(
          variable,
          3,
          4
        ),
        resolution = TRUE,
        invert = TRUE
      ),
      " resolution data.\n"
    )
  } else {
    if (is.null(level)) {
      return_message <- paste0(
        "Calculating ",
        variable,
        " covariates for ",
        message_time,
        "...\n"
      )
    } else {
      return_message <- paste0(
        "Calculating ",
        variable,
        " covariates at ",
        level,
        " for ",
        message_time,
        "...\n"
      )
    }
  }
  message(return_message)
}

################################################################################
#' Perform covariate extraction
#' @description
#' Extract covariate values from `SpatRaster` object passed from
#' \code{process_*()}.
#' @param dataset character(1). Dataset name.
#' @param from SpatRaster(1). Cleaned `SpatRaster` object.
#' @param locs_vector SpatVector(1). Cleaned `SpatVector` object passed
#' from \code{calc_prepare_locs()}. Contains location point/polygon values.
#' @param locs_df data.frame(1). Cleaned `data.frame` object passed from
#' \code{calc_prepare_locs()}. Contains location identifiers.
#' @param fun character(1). Summary function. Passed to \code{terra::extract()}.
#' @param variable integer. Position within the layer name containing the
#' variable name/code.
#' @param time integer. Position within the layer name containing the time
#' value(s).
#' @param time_type character(1). Type of time observation. One of "date",
#' "hour", "year", "yearmonth", "timeless".
#' @param level integer. Position within the layer name containing the vertical
#' pressure level value (if applicable). Default = `NULL`.
#' @param radius integer(1). Buffer distance (m). Passed from
#' \code{calc_prepare_locs()}. Used in column naming.
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values will expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`.
#' See [`exactextractr::exact_extract`] for details.
#' @param weights NULL, SpatRaster, polygon SpatVector/sf, or file path.
#'   Optional weighting surface used for weighted extraction.
#' @param ... Placeholders.
#' @importFrom terra nlyr
#' @importFrom terra extract
#' @importFrom exactextractr exact_extract
#' @importFrom sf st_as_sf
#' @return a `data.frame` object
#' @keywords internal auxiliary
#' @export
calc_worker <- function(
  dataset,
  from,
  locs_vector,
  locs_df,
  fun = "mean",
  variable = 1,
  time,
  time_type = c("date", "hour", "year", "yearmonth", "timeless"),
  radius,
  level = NULL,
  max_cells = 1e8,
  weights = NULL,
  ...
) {
  #### empty location data.frame
  sites_extracted <- NULL
  time_type <- match.arg(time_type)
  weights_prepared <- amadeus::calc_prepare_weights(
    from = from[[1]],
    weights = weights
  )
  fun_extract <- amadeus::calc_weighted_fun(
    fun = fun,
    weighted = !is.null(weights_prepared)
  )
  for (l in seq_len(terra::nlyr(from))) {
    #### select data layer
    data_layer <- from[[l]]
    layer_time <- NULL
    #### split layer name
    data_split <- strsplit(
      names(data_layer),
      "_"
    )[[1]]
    #### extract variable
    data_name <- data_split[variable]
    if (!is.null(time)) {
      layer_time <- try(terra::time(data_layer), silent = TRUE)
      if (inherits(layer_time, "try-error")) {
        layer_time <- NULL
      }
      #### extract time
      data_time <- calc_time(
        time = data_split[time],
        format = time_type,
        dataset = dataset,
        layer_name = names(data_layer),
        layer_time = layer_time
      )
    }
    #### extract level (if applicable)
    if (!is.null(level)) {
      data_level <- data_split[level]
    } else {
      data_level <- NULL
    }
    layer_time_msg <- if (!is.null(time)) data_split[time] else NA_character_
    #### message
    calc_message(
      dataset = dataset,
      variable = data_name,
      time = layer_time_msg,
      time_type = time_type,
      level = data_level,
      layer_time = layer_time
    )
    #### extract layer data at sites
    if (terra::geomtype(locs_vector) == "polygons") {
      ### apply exactextractr::exact_extract for polygons
      extract_args <- list(
        x = data_layer,
        y = sf::st_as_sf(locs_vector),
        progress = FALSE,
        force_df = TRUE,
        fun = fun_extract,
        max_cells_in_memory = max_cells
      )
      if (!is.null(weights_prepared)) {
        extract_args$weights <- weights_prepared
      }
      sites_extracted_layer <- do.call(
        exactextractr::exact_extract,
        extract_args
      )
    } else if (terra::geomtype(locs_vector) == "points") {
      if (is.null(weights_prepared)) {
        #### apply terra::extract for points
        sites_extracted_layer <- terra::extract(
          data_layer,
          locs_vector,
          method = "simple",
          ID = FALSE,
          bind = FALSE,
          na.rm = TRUE
        )
      } else {
        weighted_geoms <- amadeus::calc_prepare_exact_geoms(
          locs_vector = locs_vector,
          radius = radius
        )
        sites_extracted_layer <- exactextractr::exact_extract(
          x = data_layer,
          y = weighted_geoms,
          weights = weights_prepared,
          progress = FALSE,
          force_df = TRUE,
          fun = fun_extract,
          max_cells_in_memory = max_cells
        )
      }
    }
    # merge with site_id, time, and pressure levels (if applicable)
    if (time_type == "timeless") {
      sites_extracted_layer <- cbind(
        locs_df,
        sites_extracted_layer
      )
      colnames(sites_extracted_layer) <- c(
        colnames(locs_df),
        paste0(
          data_name,
          "_",
          radius
        )
      )
    } else {
      if (is.null(level)) {
        sites_extracted_layer <- cbind(
          locs_df,
          data_time,
          sites_extracted_layer
        )
        colnames(sites_extracted_layer) <- c(
          colnames(locs_df),
          "time",
          paste0(
            data_name,
            "_",
            radius
          )
        )
      } else {
        sites_extracted_layer <- cbind(
          locs_df,
          data_time,
          gsub(
            "level=|lev=",
            "",
            data_level
          ),
          sites_extracted_layer
        )
        colnames(sites_extracted_layer) <- c(
          colnames(locs_df),
          "time",
          "level",
          paste0(
            tolower(data_name),
            "_",
            radius
          )
        )
      }
    }
    #### merge with empty sites_extracted
    sites_extracted <- rbind(
      sites_extracted,
      sites_extracted_layer
    )
  }
  #### finish message
  message(
    paste0(
      "Returning extracted covariates.\n"
    )
  )
  #### return data.frame
  return(data.frame(sites_extracted))
}

################################################################################
#' Normalize `.by_time` time-unit aliases
#' @description Internal helper that maps singular/plural `.by_time` tokens
#' to canonical units.
#' @param unit character(1). Time-unit alias.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return character(1). Canonical unit.
#' @export
normalize_by_time_unit <- function(unit) {
  aliases <- c(
    minute = "minute",
    minutes = "minute",
    hour = "hour",
    hours = "hour",
    day = "day",
    days = "day",
    week = "week",
    weeks = "week",
    month = "month",
    months = "month",
    quarter = "quarter",
    quarters = "quarter",
    year = "year",
    years = "year"
  )
  if (!is.character(unit) || length(unit) != 1L || !unit %in% names(aliases)) {
    stop("`unit` must be one valid `.by_time` time-unit token.\n")
  }
  aliases[[unit]]
}

################################################################################
#' Bucket a time column to a `.by_time` unit
#' @description Buckets time values to one of the supported `.by_time` units.
#' @param time_vals vector. Time values to bucket.
#' @param unit character(1). A valid `.by_time` time-unit token.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return vector. Bucketed values as POSIXct (minute/hour) or Date.
#' @export
bucket_time_by_unit <- function(time_vals, unit) {
  unit_norm <- normalize_by_time_unit(unit)
  if (unit_norm %in% c("minute", "hour")) {
    breaks <- if (unit_norm == "minute") "min" else "hour"
    return(as.POSIXct(cut(as.POSIXct(time_vals, tz = "UTC"), breaks = breaks)))
  }
  time_vals_chr <- as.character(time_vals)
  if (all(grepl("^[0-9]{8}$", time_vals_chr))) {
    time_date <- as.Date(time_vals_chr, format = "%Y%m%d")
  } else if (all(grepl("^[0-9]{7}$", time_vals_chr))) {
    time_date <- as.Date(time_vals_chr, format = "%Y%j")
  } else if (all(grepl("^[0-9]{6}$", time_vals_chr))) {
    time_date <- as.Date(paste0(time_vals_chr, "01"), format = "%Y%m%d")
  } else if (all(grepl("^[0-9]{4}$", time_vals_chr))) {
    time_date <- as.Date(paste0(time_vals_chr, "-01-01"))
  } else {
    time_date <- as.Date(time_vals, tz = "UTC")
    if (any(is.na(time_date))) {
      stop(
        "Unable to bucket time values. Provide parseable Date/POSIXct values ",
        "or recognized numeric encodings (YYYYDDD, YYYYMMDD, YYYYMM, YYYY).\n"
      )
    }
  }
  switch(
    unit_norm,
    day = time_date,
    week = as.Date(cut(time_date, breaks = "week")),
    month = as.Date(cut(time_date, breaks = "month")),
    quarter = as.Date(cut(time_date, breaks = "quarter")),
    year = as.Date(cut(time_date, breaks = "year"))
  )
}

################################################################################
#' Summarize extracted covariates by `.by_time` temporal unit
#' @description
#' Generic temporal summarizer for covariate tables. When
#' \code{.by_time} is \code{NULL}, the input is returned unchanged.
#' Otherwise, numeric covariates are summarized by
#' \code{locs_id + bucketed time + group_cols_extra}.
#' @param covar data.frame. Extracted covariates.
#' @param fun_summary character(1) or function. Summary function
#'   (e.g., \code{"mean"}, \code{"sum"}).
#' @param locs_id character(1). Location-id column.
#' @param time_col character(1). Time column in \code{covar}.
#' @param .by_time NULL or character(1). Temporal unit token.
#' @param group_cols_extra character or NULL. Extra grouping columns.
#' @param ... Placeholders.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return a data.frame.
#' @importFrom dplyr across all_of group_by summarize left_join
#' @export
calc_summarize_by <- function(
  covar,
  fun_summary = "mean",
  locs_id = "site_id",
  time_col = "time",
  .by_time = NULL,
  group_cols_extra = NULL,
  ...
) {
  check_unsupported_by(..., .call = sys.call())
  if (is.null(.by_time)) {
    return(covar)
  }
  stopifnot(is.data.frame(covar))
  check_by_time(.by_time)

  if (!locs_id %in% names(covar)) {
    stop(sprintf("`locs_id` column '%s' not found in `covar`.\n", locs_id))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf("`time_col` column '%s' not found in `covar`.\n", time_col))
  }
  if (!is.null(group_cols_extra)) {
    missing_extra <- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) > 0L) {
      stop(
        sprintf(
          "Grouping column(s) not found in `covar`: %s.\n",
          paste(missing_extra, collapse = ", ")
        )
      )
    }
  }

  if (is.character(fun_summary)) {
    if (length(fun_summary) != 1L) {
      stop("`fun_summary` must be a single function name.\n")
    }
    fun_r <- match.fun(fun_summary)
  } else if (is.function(fun_summary)) {
    fun_r <- fun_summary
  } else {
    stop("`fun_summary` must be a character string or function.\n")
  }

  group_cols <- c(locs_id, time_col, group_cols_extra)
  group_cols <- unique(stats::na.omit(group_cols))
  covar2 <- covar
  covar2[[time_col]] <- bucket_time_by_unit(covar2[[time_col]], .by_time)

  cov_cols <- names(covar2)[vapply(covar2, is.numeric, logical(1))]
  cov_cols <- setdiff(cov_cols, group_cols)
  if (length(cov_cols) == 0L) {
    stop("No numeric covariate columns found to summarize.\n")
  }

  summary_df <- covar2 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(cov_cols), \(x) fun_r(x, na.rm = TRUE)),
      .groups = "drop"
    )

  if ("geometry" %in% names(covar2)) {
    geom_first <- covar2[
      !duplicated(covar2[, group_cols, drop = FALSE]),
      c(group_cols, "geometry"),
      drop = FALSE
    ]
    summary_df <- dplyr::left_join(summary_df, geom_first, by = group_cols)
  }

  col_order <- c(group_cols, cov_cols)
  if ("geometry" %in% names(summary_df)) {
    col_order <- c(col_order, "geometry")
  }
  data.frame(summary_df[, unique(col_order), drop = FALSE])
}

################################################################################
#' Summarize extracted covariates at native temporal grain
#' @description Internal helper that summarizes numeric covariates by
#' \code{locs_id + time + group_cols_extra} while preserving the original time
#' representation.
#' @param covar data.frame. Extracted covariates.
#' @param fun_summary character(1) or function. Summary function.
#' @param locs_id character(1). Location-id column.
#' @param time_col character(1). Time column in \code{covar}.
#' @param group_cols_extra character or NULL. Extra grouping columns.
#' @return a data.frame.
#' @keywords internal auxiliary
#' @export
calc_summarize_native_time <- function(
  covar,
  fun_summary = "mean",
  locs_id = "site_id",
  time_col = "time",
  group_cols_extra = NULL
) {
  stopifnot(is.data.frame(covar))
  if (!locs_id %in% names(covar)) {
    stop(sprintf("`locs_id` column '%s' not found in `covar`.\n", locs_id))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf("`time_col` column '%s' not found in `covar`.\n", time_col))
  }
  if (!is.null(group_cols_extra)) {
    missing_extra <- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) > 0L) {
      stop(
        sprintf(
          "Grouping column(s) not found in `covar`: %s.\n",
          paste(missing_extra, collapse = ", ")
        )
      )
    }
  }
  if (is.character(fun_summary)) {
    if (length(fun_summary) != 1L) {
      stop("`fun_summary` must be a single function name.\n")
    }
    fun_r <- match.fun(fun_summary)
  } else if (is.function(fun_summary)) {
    fun_r <- fun_summary
  } else {
    stop("`fun_summary` must be a character string or function.\n")
  }

  group_cols <- unique(stats::na.omit(c(locs_id, time_col, group_cols_extra)))
  cov_cols <- names(covar)[vapply(covar, is.numeric, logical(1))]
  cov_cols <- setdiff(cov_cols, group_cols)
  if (length(cov_cols) == 0L) {
    stop("No numeric covariate columns found to summarize.\n")
  }

  summary_df <- covar |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(cov_cols), \(x) fun_r(x, na.rm = TRUE)),
      .groups = "drop"
    )

  if ("geometry" %in% names(covar)) {
    geom_first <- covar[
      !duplicated(covar[, group_cols, drop = FALSE]),
      c(group_cols, "geometry"),
      drop = FALSE
    ]
    summary_df <- dplyr::left_join(summary_df, geom_first, by = group_cols)
  }

  col_order <- c(group_cols, cov_cols)
  if ("geometry" %in% names(summary_df)) {
    col_order <- c(col_order, "geometry")
  }
  data.frame(summary_df[, unique(col_order), drop = FALSE])
}

################################################################################
#' Apply default/native or explicit temporal summarization
#' @param covar data.frame.
#' @param .by_time NULL or character(1).
#' @param fun_summary character(1) or function.
#' @param locs_id character(1).
#' @param time_col character(1).
#' @param group_cols_extra character or NULL.
#' @return data.frame
#' @keywords internal auxiliary
#' @export
calc_apply_time_summary <- function(
  covar,
  .by_time = NULL,
  fun_summary = "mean",
  locs_id = "site_id",
  time_col = "time",
  group_cols_extra = NULL
) {
  if (is.null(.by_time)) {
    return(
      calc_summarize_native_time(
        covar = covar,
        fun_summary = fun_summary,
        locs_id = locs_id,
        time_col = time_col,
        group_cols_extra = group_cols_extra
      )
    )
  }
  calc_summarize_by(
    covar = covar,
    fun_summary = fun_summary,
    locs_id = locs_id,
    time_col = time_col,
    .by_time = .by_time,
    group_cols_extra = group_cols_extra
  )
}

################################################################################
#' Summarize extracted covariates by temporal bucket
#' @description
#' Applies a named summary function across covariate columns
#' after bucketing the \code{time} column to a coarser temporal
#' resolution (daily by default).  When \code{fun_temporal} is
#' \code{NULL}, the input is returned unchanged
#' (backward-compatible default).  A WKT \code{"geometry"} column
#' produced by \code{calc_prepare_locs()} is preserved by
#' carrying forward the first observed geometry per group.
#' @param covar data.frame. Extracted covariate table, typically
#'   the output of \code{calc_worker()} or a
#'   \code{calculate_*()} function before the
#'   \code{calc_return_locs()} call. Must contain the columns
#'   named by \code{locs_id} and \code{time_col}.
#' @param fun_temporal NULL or character(1). Name of the summary
#'   function.  One of \code{"mean"}, \code{"median"},
#'   \code{"sum"}, \code{"max"}, \code{"min"}, or \code{NULL}
#'   (no aggregation; backward-compatible default).
#' @param locs_id character(1). Name of the location-identifier
#'   column in \code{covar}. Default \code{"site_id"}.
#' @param time_col character(1). Name of the time column in
#'   \code{covar}. Default \code{"time"}.
#' @param time_bucket character(1). Temporal resolution to
#'   summarise to.  One of \code{"day"} (default),
#'   \code{"week"}, \code{"month"}, or \code{"year"}.
#' @param group_cols_extra character or NULL. Additional column
#'   names to include in the grouping key (e.g. \code{"level"}
#'   for pressure-level data). Default \code{NULL}.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return a data.frame.  When \code{fun_temporal} is
#'   \code{NULL}, \code{covar} is returned as-is.  Otherwise
#'   each row represents one unique group / time-bucket
#'   combination with covariate columns aggregated by
#'   \code{fun_temporal}.
#' @importFrom dplyr across all_of group_by summarize left_join
#' @export
calc_summarize_temporal <- function(
  covar,
  fun_temporal,
  locs_id = "site_id",
  time_col = "time",
  time_bucket = "day",
  group_cols_extra = NULL
) {
  if (is.null(fun_temporal)) {
    return(covar)
  }
  stopifnot(is.data.frame(covar))
  if (!locs_id %in% names(covar)) {
    stop(sprintf(
      "`locs_id` column '%s' not found in `covar`.",
      locs_id
    ))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf(
      "`time_col` column '%s' not found in `covar`.",
      time_col
    ))
  }
  if (!is.null(group_cols_extra)) {
    missing_extra <- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) > 0L) {
      stop(sprintf(
        paste0(
          "Extra grouping column(s) not found in `covar`: %s."
        ),
        paste(missing_extra, collapse = ", ")
      ))
    }
  }
  time_bucket <- match.arg(
    time_bucket,
    c("day", "week", "month", "year")
  )
  grp_cols <- c(locs_id, group_cols_extra, time_col)
  skip_cols <- c(grp_cols, "geometry")
  cov_cols <- setdiff(names(covar), skip_cols)
  has_geom <- "geometry" %in% names(covar)
  if (length(cov_cols) == 0L) {
    stop(paste0(
      "No covariate columns found in `covar` to summarize."
    ))
  }
  covar2 <- covar
  covar2[[time_col]] <- switch(
    time_bucket,
    day = as.Date(covar[[time_col]]),
    week = as.Date(
      cut(as.Date(covar[[time_col]]), breaks = "week")
    ),
    month = as.Date(
      cut(as.Date(covar[[time_col]]), breaks = "month")
    ),
    year = as.Date(
      cut(as.Date(covar[[time_col]]), breaks = "year")
    )
  )
  fun_r <- match.fun(fun_temporal)
  force(fun_r)
  summary_df <- covar2 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(cov_cols),
        \(x) fun_r(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  if (has_geom) {
    geom_first <- covar2[
      !duplicated(covar2[, grp_cols, drop = FALSE]),
      c(grp_cols, "geometry"),
      drop = FALSE
    ]
    summary_df <- dplyr::left_join(
      summary_df,
      geom_first,
      by = grp_cols
    )
  }
  col_order <- c(grp_cols, cov_cols)
  if (has_geom) {
    col_order <- c(col_order, "geometry")
  }
  data.frame(summary_df[, col_order, drop = FALSE])
}

################################################################################
#' Set column names
#' @description
#' Apply standard column names to calculated covariates consistent with
#' the requirements of the \code{beethoven} package. Column names follow fixed
#' format of 3 character data genre, 2 - 15 character variable code, 1 digit
#' temporal lag, and 5 digit buffer radius (in meters). Variable code character
#' range is required to retain interpretable column names across datasets.
#' @note
#' \code{beethoven} utilizes point, 1km, and 10km radius buffer distance for
#' covariate calculation, and therefore the buffer radius column is
#' padded to 5 digits. If provided a buffer radius greater than 5 digits,
#'  \code{calc_setcolumns()} will expand to the number of digits. (ie. buffer
#' radius of 100km = CCC_CCCCC_I_100000).
#' @param from data.frame(1) or SpatVector(1). Calculated covariates as
#' returned from \code{calc_covariates()} or a source specific covariate
#' function.
#' @param lag integer(1). Temporal lag.
#' @param dataset character(1). Covariate parent dataset.
#' @param locs_id character(1). Column containing identifier for each unique
#' coordinate location.
#' @keywords internal auxiliary
#' @importFrom stringi stri_pad
#' @return a data.frame or SpatVector object (depending on `from`)
#' @export
calc_setcolumns <- function(
  from,
  lag,
  dataset,
  locs_id
) {
  #### check from is data.frame
  stopifnot(class(from) %in% c("data.frame", "SpatVector"))
  #### original names
  names_from <- colnames(from)
  #### copy
  names_return <- names_from
  #### identifier
  id_index <- which(names_from == locs_id)
  names_return[id_index] <- locs_id
  #### time
  time_index <- which(
    names_from %in% c("time", "year", "date", "hour")
  )
  stopifnot(length(time_index) <= 1)
  names_return[time_index] <- "time"
  #### description (for time period coverage)
  description_index <- which(names_from == "description")
  stopifnot(length(description_index) <= 1)
  names_return[description_index] <- "description"
  #### geometry
  geometry_index <- which(names_from == "geometry")
  stopifnot(length(geometry_index) <= 1)
  names_return[geometry_index] <- "geometry"
  #### latitude and longitude
  lat_index <- which(
    tolower(names_from) %in% c("lat", "latitude")
  )
  stopifnot(length(lat_index) <= 1)
  names_return[lat_index] <- "lat"
  lon_index <- which(
    tolower(names_from) %in% c("lon", "longitude")
  )
  stopifnot(length(lon_index) <= 1)
  names_return[lon_index] <- "lon"
  #### vertical pressure level
  level_index <- which(names_from == "level")
  stopifnot(length(level_index) <= 1)
  names_return[level_index] <- "level"
  #### dataset and genre
  datasets <- c(
    "aqs",
    "ecoregions",
    "geos",
    "gmted",
    "koppen geiger",
    "merra2",
    "modis",
    "narr",
    "nlcd",
    "hms",
    "groads",
    "pop",
    "tri",
    "nei",
    "gridmet",
    "terraclimate"
  )
  stopifnot(dataset %in% datasets)
  genre <- substr(dataset, 1, 3)
  #### covariates
  cov_index <- which(
    !(c(
      names_from %in%
        c(
          locs_id,
          "geometry",
          "time",
          "lat",
          "lon",
          "level",
          "description"
        )
    ))
  )
  for (c in seq_along(cov_index)) {
    name_covariate <- names_return[cov_index[c]]
    name_split <- strsplit(name_covariate, "_")[[1]]
    name_variable <- tolower(name_split[1])
    name_variable <- gsub(
      "\\.",
      "",
      gsub(
        "-",
        "",
        gsub(
          " ",
          "",
          name_variable
        )
      )
    )
    nchar_radius <- nchar(as.character(name_split[2]))
    width_radius <- ifelse(nchar_radius > 5, nchar_radius, 5)
    name_new <- paste0(
      genre,
      "_",
      strsplit(
        name_variable,
        1,
        15
      ),
      "_",
      lag,
      "_",
      stringi::stri_pad(
        name_split[2],
        width = width_radius,
        pad = 0,
        side = "left"
      )
    )
    names_return[cov_index[c]] <- toupper(name_new)
  }
  #### check for unique names
  stopifnot(length(names_return) == length(unique(names_return)))
  return(from)
}

################################################################################
#' Check time values
#' @description
#' Check the time values within calculated covariates `data.frame`
#' @param covar data.frame(1). Calculated covariates `data.frame`.
#' @param POSIXt logical(1). Should the time values in `covar` be of class
#' `POSIXt`? If `FALSE`, the time values will be checked for integer class
#' (year and year-month).
#' @return NULL; returns a stop error if `time` is wrong class
#' @keywords internal auxiliary
#' @export
# nolint start
calc_check_time <- function(
  covar,
  POSIXt = TRUE
) {
  stopifnot(methods::is(covar, "data.frame"))
  if ("time" %in% names(covar)) {
    if (POSIXt) {
      stopifnot(all(sapply(covar$time, methods::is, "POSIXt")))
    } else {
      stopifnot(all(sapply(covar$time, methods::is, "integer")))
    }
  } else {
    message(
      "`$time` not detected in `data.frame` provided.\n"
    )
  }
}
# nolint end

################################################################################
#' Prepare covariates for return
#' @description
#' Check the time column for proper class and, if `geom = TRUE`,
#' transform `data.frame` into a `SpatVector` object.
#' @param covar data.frame(1). Calculated covariates `data.frame`.
#' @param POSIXt logical(1). Should the time values in `covar` be of class
#' `POSIXt`? If `FALSE`, the time values will be checked for integer class
#' (year and year-month).
#' @param geom FALSE/"sf"/"terra". Should `covar` be returned as a
#' `data.frame`? Default is `FALSE`, options with geometry are "sf" or "terra".
#' @param crs terra::crs(1). Coordinate reference system (inherited from
#' `from`).
#' @importFrom terra vect
#' @keywords internal auxiliary
#' @author Mitchell Manware
#' @return a data.frame or SpatVector object (depending on `geom` paramter)
#' @export
# nolint start
calc_return_locs <- function(
  covar,
  POSIXt = TRUE,
  geom,
  crs
) {
  # time value check
  if ("time" %in% names(covar)) {
    calc_check_time(covar = covar, POSIXt = POSIXt)
  }
  # nolint end
  # if geom, convert to and return SpatVector
  if (geom %in% c("terra", "sf")) {
    if (nrow(covar) == 0) {
      if ("geometry" %in% names(covar)) {
        covar_sf <- sf::st_as_sf(covar, wkt = "geometry", crs = crs)
      } else if (all(c("lon", "lat") %in% names(covar))) {
        covar_sf <- sf::st_sf(
          covar,
          geometry = sf::st_sfc(crs = crs)
        )
      } else {
        warning(
          paste(
            "`geom` was requested but no geometry columns were found in",
            "`covar`; returning data.frame."
          )
        )
        return(data.frame(covar))
      }
      if (geom == "sf") {
        return(covar_sf)
      }
      return(suppressWarnings(terra::vect(covar_sf)))
    }
    covar_return <- NULL
    if ("geometry" %in% names(covar)) {
      covar_sf <- sf::st_as_sf(covar, wkt = "geometry", crs = crs)
      covar_return <- if (geom == "sf") {
        covar_sf
      } else {
        suppressWarnings(terra::vect(covar_sf))
      }
    } else if (all(c("lon", "lat") %in% names(covar))) {
      covar_return <- terra::vect(
        covar,
        geom = c("lon", "lat"),
        crs = crs
      )
    }
    if (is.null(covar_return)) {
      warning(
        paste(
          "`geom` was requested but no geometry columns were found in",
          "`covar`; returning data.frame."
        )
      )
      return(data.frame(covar))
    }
    if (geom == "terra") {
      return(covar_return)
    } else if (geom == "sf") {
      return(
        if (inherits(covar_return, "sf")) {
          covar_return
        } else {
          sf::st_as_sf(covar_return)
        }
      )
    }
  } else {
    return(data.frame(covar))
  }
}
################################################################################
