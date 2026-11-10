################################################################################
# {amadeus} functions for Köppen-Geiger climate classification.
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download climate classification data
#' @description
#' The \code{download_koppen_geiger()} function accesses and downloads
#' climate classification data.
#' @note Köppen-Geiger data does not require authentication.
#' @param data_resolution character(1). One of `"0.0083"` (default),
#'   `"0.083"`, or `"0.5"`.
#' @param time_period character(1). `"Present"` (default; 1980-2016) or
#'   `"Future"` (2071-2100).
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
#' @details
#' `data_resolution` and `time_period` each select one value. Matching is
#' case-sensitive, and unambiguous abbreviations are accepted. If omitted,
#' each argument uses its first listed choice.
#' @author Mitchell Manware, Insang Song
#' @return
#' When `download = TRUE` and `hash = FALSE`, an invisible list containing
#' `success`, `failed`, and `skipped` file counts. If the requested archive
#' already exists and is nonempty, no file is downloaded; the returned list
#' contains `success = 0`, `failed = 0`, and `skipped = 1`. The list always
#' contains `failed_urls` and `failed_files` character vectors, both
#' `character(0)` when no downloads failed, including when the file was skipped.
#'
#' When `download = FALSE`, an invisible list containing `urls`, `destfiles`,
#' and `n_files = 1` is returned without downloading, regardless of `hash`.
#'
#' When `download = TRUE` and `hash = TRUE`, a character(1) combined MD5 hash
#' of the files in the `data_files` directory is returned instead of a list.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_beck2023koppen}{amadeus}
#' \insertRef{article_beck2018present}{amadeus}
#' @examples
#' \dontrun{
#' download_koppen_geiger(
#'   data_resolution = "0.0083",
#'   time_period = "Present",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_koppen_geiger <- function(
  data_resolution = c("0.0083", "0.083", "0.5"),
  time_period = c("Present", "Future"),
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
    dataset_name = "koppengeiger",
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
  # Check source-specific arguments.
  chr_resolution <- match.arg(data_resolution)
  chr_timeperiod <- match.arg(time_period)
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus:::download_setup_dir(directory_to_save, zip = TRUE)
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Generate download URL.
  chr_url <- paste0(
    "https://s3-eu-west-1.amazonaws.com/",
    "pfigshare-u-files/12407516/Beck_KG_V1.zip"
  )
  # ----------------------------------------------------------------------------
  # Generate download file name.
  chr_filename <- paste0(
    chr_dir_to_download,
    "koppen_geiger_",
    tolower(chr_timeperiod),
    "_",
    gsub("\\.", "p", chr_resolution),
    ".zip"
  )
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus:::download_preview(chr_url, chr_filename)))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus:::download_run_method(
    urls = chr_url,
    destfiles = chr_filename,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Unzip downloaded .zip files and remove if requested.
  amadeus:::download_unzip(
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
#' Process climate classification data
#' @description
#' The \code{process_koppen_geiger()} function imports and cleans raw climate
#' classification data, returning a single `SpatRaster` object.
#' @param path character(1). Path to Koppen-Geiger
#'  climate zone raster file
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @return a `SpatRaster` object
#' @author Insang Song
#' @importFrom terra rast
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' kg <- process_koppen_geiger(
#'   path = "./data/koppen_geiger_data.tif"
#' )
#' }
#' @export
process_koppen_geiger <-
  function(
    path = NULL,
    extent = NULL,
    ...
  ) {
    # import data
    kg_rast <- terra::rast(path, win = extent)
    # identify time period
    period <- strsplit(
      names(kg_rast),
      "_"
    )[[1]][4]
    if (period == "present") {
      terra::metags(kg_rast) <- "year=1980 - 2016"
    } else {
      terra::metags(kg_rast) <- "year=2071 - 2100"
    }
    return(kg_rast)
  }

################################################################################
# {calculate} tier
################################################################################
#' Calculate climate classification covariates
#' @description
#' Extract Koppen-Geiger climate classes at point or buffered locations. Returns
#' a \code{data.frame} with \code{locs_id}, a \code{description} column, and
#' either binary indicators (\code{frac = FALSE}) or fractional overlap values
#' (\code{frac = TRUE}) for climate groups A-E.
#' @param from SpatRaster(1). Output of \code{process_koppen_geiger()}.
#' @param locs sf/SpatVector. Unique locs. Should include
#'  a unique identifier field named `locs_id`
#' @param locs_id character(1). Name of unique identifier.
#' @param frac logical(1). Default `FALSE`. If `FALSE`, return binary 0/1
#'   indicators by climate group. If `TRUE`, return fractional overlap in the
#'   extraction footprint.
#' @param radius numeric(1). Circular buffer size (meters) around point
#'   locations. Use `0` (default) for exact point extraction.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @seealso [`process_koppen_geiger`]
#' @return a data.frame or SpatVector object with climate columns named like
#'   `DUM_CLRGA_00000` (`frac = FALSE`) or `FRC_CLRGA_100000` (`frac = TRUE`)
#'   where the suffix reflects the extraction radius.
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
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_koppen_geiger(
#'   from = kg, # derived from process_koppen_geiger() example
#'   locs = loc,
#'   locs_id = "id",
#'   geom = FALSE
#' )
#' }
#' @export
# locs (locs), from (from), locs_id (id_col), variables
calculate_koppen_geiger <-
  function(
    from = NULL,
    locs = NULL,
    locs_id = "site_id",
    weights = NULL,
    geom = FALSE,
    frac = FALSE,
    radius = 0,
    ...
  ) {
    amadeus::check_unsupported_by(..., .call = sys.call())
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
    value_prefix <- if (isTRUE(frac)) "FRC" else "DUM"

    # prepare locations
    locs_prepared <- amadeus::calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = radius,
      geom = geom
    )
    locs_kg <- locs_prepared[[1]]
    locs_df <- locs_prepared[[2]]
    is_point_locs <- all(
      tolower(terra::geomtype(locs_kg)) %in% c("points", "point")
    )
    if (is_point_locs && radius_value == 0L) {
      locs_kg_extract <- terra::extract(from, locs_kg)
      locs_kg_extract[[locs_id]] <- locs_df[[locs_id]][locs_kg_extract$ID]
      locs_kg_extract$base_value <- 1
    } else {
      locs_kg_extract <- terra::extract(
        from,
        locs_kg,
        cells = TRUE,
        weights = TRUE
      )
      locs_kg_extract[[locs_id]] <- locs_df[[locs_id]][locs_kg_extract$ID]
      coverage_col <- c("weight", "weights", "fraction")
      coverage_col <- coverage_col[coverage_col %in% names(locs_kg_extract)][1]
      if (is.na(coverage_col) || length(coverage_col) == 0) {
        locs_kg_extract$base_value <- 1
      } else {
        locs_kg_extract$base_value <- as.numeric(
          locs_kg_extract[[coverage_col]]
        )
        locs_kg_extract$base_value[!is.finite(locs_kg_extract$base_value)] <- 0
      }
    }
    value_col <- setdiff(
      names(locs_kg_extract),
      c("ID", locs_id, "cell", "weight", "weights", "fraction", "base_value")
    )
    value_col <- value_col[1]
    locs_kg_extract <- locs_kg_extract[
      !is.na(locs_kg_extract[[value_col]]),
      c(locs_id, value_col, "base_value"),
      drop = FALSE
    ]
    names(locs_kg_extract)[2] <- "value"

    # The starting value is NA as the color table has 0 value in it
    kg_class <-
      c(
        NA,
        "Af",
        "Am",
        "Aw",
        "BWh",
        "BWk",
        "BSh",
        "BSk",
        "Csa",
        "Csb",
        "Csc",
        "Cwa",
        "Cwb",
        "Cwc",
        "Cfa",
        "Cfb",
        "Cfc",
        "Dsa",
        "Dsb",
        "Dsc",
        "Dsd",
        "Dwa",
        "Dwb",
        "Dwc",
        "Dwd",
        "Dfa",
        "Dfb",
        "Dfc",
        "Dfd",
        "ET",
        "EF"
      )
    kg_coltab <- terra::coltab(from)
    kg_coltab <- kg_coltab[[1]][seq(1, 31), ]
    kg_colclass <- data.frame(
      value = kg_coltab$value,
      class_kg = kg_class
    )
    locs_kg_extract_e <- merge(
      locs_kg_extract,
      kg_colclass,
      by = "value",
      all.x = TRUE,
      sort = FALSE
    )

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
    locs_kg_extract_e$class_kg <- substr(locs_kg_extract_e$class_kg, 1, 1)
    aelabels <- LETTERS[1:5]
    class_values <- data.frame(
      site_id = as.character(locs_kg_extract_e[[locs_id]]),
      class_kg = as.character(locs_kg_extract_e$class_kg),
      base_value = as.numeric(locs_kg_extract_e$base_value),
      stringsAsFactors = FALSE
    )
    class_values <- class_values[
      class_values$class_kg %in% aelabels,
      ,
      drop = FALSE
    ]
    if (nrow(class_values) > 0) {
      class_values <- stats::aggregate(
        base_value ~ site_id + class_kg,
        data = class_values,
        FUN = sum
      )
      if (!isTRUE(frac)) {
        class_values$base_value <- as.integer(class_values$base_value > 0)
      } else {
        totals <- stats::aggregate(
          base_value ~ site_id,
          data = class_values,
          FUN = sum
        )
        denom <- totals$base_value[match(class_values$site_id, totals$site_id)]
        denom[!is.finite(denom) | denom <= 0] <- NA_real_
        class_values$base_value <- class_values$base_value / denom
        class_values$base_value[!is.finite(class_values$base_value)] <- 0
        class_values$base_value <- pmin(class_values$base_value, 1)
      }
      class_matrix <- stats::xtabs(
        base_value ~ site_id + class_kg,
        data = class_values
      )
      df_ae_separated <- as.data.frame.matrix(class_matrix)
      df_ae_separated$site_id <- rownames(df_ae_separated)
      rownames(df_ae_separated) <- NULL
    } else {
      df_ae_separated <- data.frame(
        site_id = character(0),
        stringsAsFactors = FALSE
      )
    }
    for (class_name in aelabels) {
      if (!class_name %in% names(df_ae_separated)) {
        df_ae_separated[[class_name]] <- if (isTRUE(frac)) 0 else 0L
      }
    }
    df_ae_separated <- df_ae_separated[, c("site_id", aelabels), drop = FALSE]
    column_names <- sprintf(
      "%s_CLRG%s_%s",
      value_prefix,
      aelabels,
      radius_suffix
    )
    names(df_ae_separated)[
      match(aelabels, names(df_ae_separated))
    ] <- column_names
    kg_extracted <- merge(
      locs_df,
      df_ae_separated,
      by.x = locs_id,
      by.y = "site_id",
      all.x = TRUE,
      sort = FALSE
    )
    for (kg_col in column_names) {
      if (!kg_col %in% names(kg_extracted)) {
        kg_extracted[[kg_col]] <- if (isTRUE(frac)) 0 else 0L
      }
      kg_extracted[[kg_col]][is.na(kg_extracted[[kg_col]])] <-
        if (isTRUE(frac)) 0 else 0L
      if (!isTRUE(frac)) {
        kg_extracted[[kg_col]] <- as.integer(kg_extracted[[kg_col]])
      } else {
        kg_extracted[[kg_col]] <- as.numeric(kg_extracted[[kg_col]])
      }
    }
    desc_vals <- terra::metags(from)$value
    description <- if (length(desc_vals) >= 2) {
      as.character(desc_vals[2])
    } else {
      NA_character_
    }
    kg_extracted$description <- description
    if (geom %in% c("sf", "terra")) {
      kg_extracted <- kg_extracted[,
        c(locs_id, "geometry", "description", column_names),
        drop = FALSE
      ]
      sites_return <- amadeus::calc_return_locs(
        covar = kg_extracted,
        POSIXt = FALSE,
        geom = geom,
        crs = terra::crs(from)
      )
      #### return data.frame
      return(sites_return)
    } else {
      kg_extracted <- kg_extracted[,
        c(locs_id, "description", column_names),
        drop = FALSE
      ]
      return(kg_extracted)
    }
  }
################################################################################
