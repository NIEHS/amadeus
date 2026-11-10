################################################################################
# {amadeus} functions for US EPA Ecoregions.
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download ecoregion data
#' @description
#' The \code{download_ecoregion()} function accesses and downloads United
#' States Ecoregions data from the U.S. Environmental Protection Agency's (EPA)
#' Ecoregions.
#' @note Ecoregion data does not require authentication.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#'   Set to FALSE to skip downloading and return the planned file list.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param unzip logical(1). Unzip zip files (default TRUE).
#' @param remove_zip logical(1). Remove zip files after unzipping (default
#' FALSE).
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Insang Song
#' @return
#' When `download = TRUE` and `hash = FALSE`, an invisible list containing
#' `success`, `failed`, and `skipped` file counts. If the requested archive
#' already exists and is nonempty, no file is downloaded; the returned list
#' contains `success = 0`, `failed = 0`, and `skipped = 1`. When a download is
#' attempted, the list also contains `failed_urls` and `failed_files` character
#' vectors, empty if the download succeeded.
#'
#' When `download = FALSE`, an invisible list containing `urls`, `destfiles`,
#' and `n_files = 1` is returned without downloading, regardless of `hash`.
#'
#' When `download = TRUE` and `hash = TRUE`, a character(1) combined MD5 hash
#' of the files in the `data_files` directory is returned instead of a list.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_omernik2014ecoregions}{amadeus}
#' @examples
#' \dontrun{
#' download_ecoregion(
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_ecoregion <- function(
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "ecoregion",
    acknowledgement = acknowledgement,
    download = download,
    remove_command = remove_command,
    unzip = unzip,
    remove_zip = remove_zip,
    directory_to_save = directory_to_save,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit,
    hash = hash
  )
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus::download_setup_dir(directory_to_save, zip = TRUE)
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Standard download URLs.
  chr_url <- paste0(
    "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/",
    "us_eco_l3_state_boundaries.zip"
  )
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filename <- file.path(
    chr_dir_to_download,
    "us_eco_l3_state_boundaries.zip"
  )
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus::download_preview(chr_url, chr_filename)))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus::download_run_method(
    urls = chr_url,
    destfiles = chr_filename,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Unzip downloaded .zip files.
  amadeus::download_unzip(
    file_name = chr_filename,
    directory_to_unzip = chr_dir_to_save,
    unzip = unzip,
    remove = remove_zip
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  amadeus:::download_return(download_result, hash, chr_dir_to_save)
}

################################################################################
# {process} tier
################################################################################
#' Process ecoregion data
#' @description
#' The [`process_ecoregion`] function imports and cleans raw ecoregion
#' data, returning a `SpatVector` object.
#' @param path character(1). Path to Ecoregion Shapefiles
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note The function will fix Tukey's bridge in Portland, ME.
#' This fix will ensure that the EPA air quality monitoring sites
#' will be located within the ecoregion.
#' @author Insang Song
#' @return a `SpatVector` object
#' @importFrom terra vect
#' @importFrom sf st_read st_crs st_as_sfc st_transform st_intersects st_union
#' @importFrom data.table year
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' ecoregion <- process_ecoregion(
#'   path = "./data/epa_ecoregion.gpkg"
#' )
#' }
#' @export
process_ecoregion <-
  function(
    path = NULL,
    extent = NULL,
    ...
  ) {
    ecoreg <- sf::st_read(path)
    # fix Tukey's bridge in Portland, ME
    # nolint start
    poly_tukey <-
      "POLYGON ((-70.258 43.68, -70.2555 43.68, -70.255 43.6733, -70.2576 43.6732, -70.258 43.68))"
    poly_tukey <- sf::st_as_sfc(poly_tukey, crs = "EPSG:4326")
    poly_tukey <- sf::st_transform(poly_tukey, sf::st_crs(ecoreg))

    # nolint end
    ecoreg <- ecoreg[,
      grepl(
        "^(L2_KEY|L3_KEY|NA_L2NAME|US_L3NAME|NA_L3NAME)",
        names(ecoreg)
      )
    ]
    ecoreg_edit_idx <- sf::st_intersects(ecoreg, poly_tukey, sparse = FALSE)
    ecoreg_edit_idx <- vapply(ecoreg_edit_idx, function(x) any(x), logical(1))
    if (!all(ecoreg_edit_idx == 0)) {
      ecoreg_else <- ecoreg[!ecoreg_edit_idx, ]
      ecoreg_edit <- sf::st_union(ecoreg[ecoreg_edit_idx, ], poly_tukey)
      ecoreg <- rbind(ecoreg_else, ecoreg_edit)
    }
    ecoreg$time <- paste0(
      "1997 - ",
      format(Sys.Date(), "%Y")
    )
    ecoreg <- terra::vect(ecoreg)
    if (!is.null(extent)) {
      ecoreg_crop <- terra::crop(ecoreg, extent)
      return(ecoreg_crop)
    } else {
      return(ecoreg)
    }
  }

################################################################################
# {calculate} tier
################################################################################
#' Calculate ecoregions covariates
#' @description
#' Extract ecoregions covariates (U.S. EPA Ecoregions Level 2/3) at point or
#' polygon locations. Returns a `data.frame` object containing `locs_id` and
#' either dummy indicators (`frac = FALSE`) or area fractions (`frac = TRUE`)
#' for each ecoregion.
#' @param from SpatVector(1). Output of [`process_ecoregion`].
#' @param locs sf/SpatVector. Unique locs. Should include
#'  a unique identifier field named `locs_id`
#' @param locs_id character(1). Name of unique identifier.
#' @param colnames character(1). Naming convention for ecoregion indicator
#'   columns. Default is `"coded"` for the existing numeric key-based names.
#'   Use `"full_ecoregion"` to emit sanitized full ecoregion names.
#' @param frac logical(1). Default `FALSE`. If `FALSE`, returns binary dummy
#'   indicators (0/1). If `TRUE`, returns fractional overlap values.
#' @param radius numeric(1). Circular buffer size (meters) around point
#'   locations. Use `0` (default) for exact point extraction.
#' @param drop logical(1). Default `FALSE`. If `TRUE`, remove ecoregion columns
#'   that are all 0 or `NA` across returned locations.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @seealso [`process_ecoregion`]
#' @return a data.frame or SpatVector object with ecoregion indicator/fraction
#' variables and attributes of:
#'   - Indicator names are controlled by `colnames`: `"coded"` (default)
#'   creates key-based names such as `DUM_E2083_00000` and
#'   `DUM_E3064_00000` when `frac = FALSE`, or `FRC_E2083_00000` and
#'   `FRC_E3064_00000` when `frac = TRUE`; `"full_ecoregion"` creates
#'   sanitized name-based columns such as
#'   `DUM_E2_SOUTHEASTERN_USA_PLAINS_00000` /
#'   `FRC_E2_SOUTHEASTERN_USA_PLAINS_00000` and
#'   `DUM_E3_NORTHERN_PIEDMONT_00000` /
#'   `FRC_E3_NORTHERN_PIEDMONT_00000` (duplicates are suffixed, e.g. `_1`).
#'   - `attr(., "ecoregion2_code")`: Ecoregion lv.2 code and key
#'   - `attr(., "ecoregion3_code")`: Ecoregion lv.3 code and key
#' @author Insang Song
#' @importFrom terra extract
#' @importFrom data.table year
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_ecoregion(
#'   from = ecoregion, # derived from process_ecoregion() example
#'   locs = loc,
#'   locs_id = "id",
#'   colnames = "coded",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_ecoregion <-
  function(
    from = NULL,
    locs,
    locs_id = "site_id",
    colnames = c("coded", "full_ecoregion"),
    frac = FALSE,
    drop = FALSE,
    weights = NULL,
    geom = FALSE,
    radius = 0,
    ...
  ) {
    amadeus::check_unsupported_by(..., .call = sys.call())
    sanitize_ecoregion_name <- function(x) {
      x <- iconv(x, to = "ASCII//TRANSLIT")
      x[is.na(x)] <- "UNKNOWN"
      x <- toupper(x)
      x <- gsub("[^A-Z0-9]+", "_", x)
      x <- gsub("_+", "_", x)
      x <- gsub("^_|_$", "", x)
      x[x == ""] <- "UNKNOWN"
      x
    }
    build_ecoregion_lookup <- function(
      keys,
      labels,
      prefix,
      naming_mode,
      value_prefix
    ) {
      lookup <- unique(data.frame(
        key = as.character(keys),
        label = as.character(labels),
        stringsAsFactors = FALSE
      ))
      if (naming_mode == "coded") {
        if (prefix == "E2") {
          key_num <- regmatches(
            lookup$key,
            regexpr("\\d{1,2}\\.[1-9]", lookup$key)
          )
          key_num <- sprintf(
            "%s_%s%03d_%s",
            value_prefix,
            prefix,
            as.integer(
              10 * as.numeric(key_num)
            ),
            radius_suffix
          )
        } else {
          key_num <- regmatches(
            lookup$key,
            regexpr("\\d{1,3}", lookup$key)
          )
          key_num <- sprintf(
            "%s_%s%03d_%s",
            value_prefix,
            prefix,
            as.integer(as.numeric(key_num)),
            radius_suffix
          )
        }
        lookup$column_name <- key_num
      } else {
        safe_label <- sanitize_ecoregion_name(lookup$label)
        lookup$column_name <- paste0(
          value_prefix,
          "_",
          prefix,
          "_",
          safe_label,
          "_",
          radius_suffix
        )
        lookup$column_name <- make.unique(lookup$column_name, sep = "_")
      }
      lookup
    }
    get_extracted_field <- function(x, field) {
      candidate_names <- c(field, paste0(field, "_2"), paste0(field, ".1"))
      match_name <- candidate_names[candidate_names %in% names(x)][1]
      if (is.na(match_name) || length(match_name) == 0) {
        stop(
          "Required ecoregion field missing from intersection output: ",
          field
        )
      }
      values <- x[[match_name]]
      if (is.matrix(values) || is.data.frame(values)) {
        values <- values[, 1, drop = TRUE]
      }
      values
    }
    get_spatvector_field <- function(x, field) {
      vals <- terra::as.data.frame(x)[[field]]
      if (is.matrix(vals) || is.data.frame(vals)) {
        vals <- vals[, 1, drop = TRUE]
      }
      vals
    }
    colnames <- match.arg(colnames)
    if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
      stop("`frac` should be a single logical value (TRUE/FALSE).")
    }
    if (!is.numeric(radius) || length(radius) != 1L || is.na(radius)) {
      stop("`radius` should be a single numeric value.")
    }
    if (radius < 0) {
      stop("`radius` should be greater than or equal to 0.")
    }
    radius_value <- as.integer(round(radius))
    width_radius <- max(5L, nchar(as.character(abs(radius_value))))
    radius_suffix <- sprintf(paste0("%0", width_radius, "d"), radius_value)
    if (!is.logical(drop) || length(drop) != 1L || is.na(drop)) {
      stop("`drop` should be a single logical value (TRUE/FALSE).")
    }
    value_prefix <- if (isTRUE(frac)) "FRC" else "DUM"
    # prepare locations
    locs_prepared <- amadeus::calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = radius,
      geom = geom
    )
    # both objects will preserve the row order
    locsp <- locs_prepared[[1]]
    locs_df <- locs_prepared[[2]]
    is_point_locs <- all(
      tolower(terra::geomtype(locsp)) %in% c("points", "point")
    )

    extracted <- terra::intersect(locsp, from)
    key2_lookup <- build_ecoregion_lookup(
      keys = from$L2_KEY,
      labels = from$NA_L2NAME,
      prefix = "E2",
      naming_mode = colnames,
      value_prefix = value_prefix
    )
    key3_labels <- if ("US_L3NAME" %in% names(from)) {
      from$US_L3NAME
    } else {
      from$NA_L3NAME
    }
    key3_lookup <- build_ecoregion_lookup(
      keys = from$L3_KEY,
      labels = key3_labels,
      prefix = "E3",
      naming_mode = colnames,
      value_prefix = value_prefix
    )
    locs_ecoreg <- data.frame(
      locs_df[0, , drop = FALSE],
      description = character(0),
      stringsAsFactors = FALSE
    )

    if (nrow(extracted) > 0) {
      extracted_df <- terra::as.data.frame(extracted)
      key2_sorted <- as.character(get_extracted_field(extracted_df, "L2_KEY"))
      key3_sorted <- as.character(get_extracted_field(extracted_df, "L3_KEY"))
      site_sorted <- as.character(extracted_df[[locs_id]])

      if (is_point_locs || !isTRUE(frac)) {
        base_value <- rep(1, length(site_sorted))
      } else {
        site_areas <- terra::expanse(locsp)
        site_ids <- as.character(get_spatvector_field(locsp, locs_id))
        site_lookup <- setNames(site_areas, site_ids)
        inter_areas <- terra::expanse(extracted)
        denom <- as.numeric(site_lookup[site_sorted])
        denom[!is.finite(denom) | denom <= 0] <- NA_real_
        base_value <- inter_areas / denom
        base_value[!is.finite(base_value)] <- 0
      }

      build_wide <- function(site_ids, keys, lookup_tbl, values) {
        vals_df <- data.frame(
          site_id = site_ids,
          key = as.character(keys),
          base_value = as.numeric(values),
          stringsAsFactors = FALSE
        )
        vals_df <- stats::aggregate(
          base_value ~ site_id + key,
          data = vals_df,
          FUN = sum
        )
        if (!isTRUE(frac)) {
          vals_df$base_value <- as.integer(vals_df$base_value > 0)
        } else {
          vals_df$base_value <- pmin(vals_df$base_value, 1)
        }
        vals_df$column_name <- lookup_tbl$column_name[
          match(vals_df$key, lookup_tbl$key)
        ]
        vals_df <- vals_df[!is.na(vals_df$column_name), , drop = FALSE]
        if (nrow(vals_df) == 0) {
          return(data.frame(
            site_id = character(),
            stringsAsFactors = FALSE
          ))
        }
        tidyr::pivot_wider(
          vals_df[, c("site_id", "column_name", "base_value"), drop = FALSE],
          names_from = "column_name",
          values_from = "base_value",
          values_fill = 0
        )
      }

      matched_ids <- unique(site_sorted)
      locs_ecoreg <- cbind(
        locs_df[locs_df[[locs_id]] %in% matched_ids, , drop = FALSE],
        description = paste0("1997 - ", data.table::year(Sys.Date()))
      )
      df_lv2 <- build_wide(site_sorted, key2_sorted, key2_lookup, base_value)
      df_lv3 <- build_wide(site_sorted, key3_sorted, key3_lookup, base_value)

      locs_ecoreg <- merge(
        locs_ecoreg,
        df_lv2,
        by.x = locs_id,
        by.y = "site_id",
        all.x = TRUE,
        sort = FALSE
      )
      locs_ecoreg <- merge(
        locs_ecoreg,
        df_lv3,
        by.x = locs_id,
        by.y = "site_id",
        all.x = TRUE,
        sort = FALSE
      )
    }

    # Catch and patch for sites with no matching ecoregions
    n_locs <- nrow(locs_df)
    n_match <- if (nrow(extracted) > 0) {
      length(unique(as.character(terra::as.data.frame(extracted)[[locs_id]])))
    } else {
      0L
    }
    if (n_match != n_locs) {
      message(
        "Warning: only ",
        n_match,
        " of the ",
        n_locs,
        " locations provided had matching ecoregions. ",
        n_locs - n_match,
        " unmatched locations will present NAs."
      )
      locs_ecoreg <- merge(locs_df, locs_ecoreg, by = locs_id, all.x = TRUE)
    }

    ecoreg_cols <- grep(
      paste0("^", value_prefix, "_E[23]_"),
      names(locs_ecoreg),
      value = TRUE
    )
    if (!isTRUE(frac) && length(ecoreg_cols) > 0) {
      locs_ecoreg[, ecoreg_cols] <- lapply(
        locs_ecoreg[, ecoreg_cols, drop = FALSE],
        function(x) {
          x[!is.na(x)] <- as.integer(x[!is.na(x)] > 0)
          x
        }
      )
    }
    if (drop && length(ecoreg_cols) > 0) {
      keep_cols <- ecoreg_cols[vapply(
        ecoreg_cols,
        function(x) any(locs_ecoreg[[x]] > 0, na.rm = TRUE),
        logical(1)
      )]
      fixed_cols <- c(
        locs_id,
        if ("geometry" %in% names(locs_ecoreg)) "geometry",
        "description"
      )
      locs_ecoreg <- locs_ecoreg[, c(fixed_cols, keep_cols), drop = FALSE]
    }

    locs_return <- amadeus::calc_return_locs(
      covar = locs_ecoreg,
      POSIXt = FALSE,
      geom = geom,
      crs = terra::crs(from)
    )
    attr(locs_return, "ecoregion2_code") <- sort(unique(from$L2_KEY))
    attr(locs_return, "ecoregion3_code") <- sort(unique(from$L3_KEY))
    return(locs_return)
  }
################################################################################
