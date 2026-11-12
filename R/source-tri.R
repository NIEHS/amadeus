################################################################################
# {amadeus} functions for US EPA Toxic Release Inventory (TRI).
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download toxic release data
#' @description
#' The \code{download_tri()} function accesses and downloads toxic release
#' data from the U.S. Environmental Protection Agency's (EPA) Toxic Release
#' Inventory (TRI) Program. The EPA TRI basic data files contain annual,
#' facility-reported toxic chemical release and waste management information.
#' EPA publishes TRI basic files in multiple annual variants under the same
#' service endpoint: a nationwide file (\code{"US"}), state-specific files
#' identified by two-letter postal abbreviations (for example \code{"AZ"} or
#' \code{"NC"}), and a tribal file (\code{"tbl"}).
#' @note TRI data does not require authentication. State and tribal downloads
#'   are saved with jurisdiction-specific file names, while the U.S.-wide
#'   download keeps the historical \code{tri_raw_<year>.csv} naming pattern.
#' @param year integer(1 or more). One year, two endpoints, or a non-empty
#'   vector of years. Every year from the minimum through the maximum is
#'   requested, including gaps: `c(2018, 2023)` requests `2018:2023`.
#'   Default is `c(2018L, 2022L)`. Missing and fractional years are rejected.
#'   File availability depends on EPA coverage for the selected jurisdiction.
#' @param jurisdiction character(1). TRI file variant to download. Use
#'   `"US"` (default) for the nationwide file, a two-letter state or territory
#'   code such as `"AZ"` or `"NC"`, or `"tbl"` for the tribal file. Leading and
#'   trailing whitespace is removed and case is normalized, so `" nc "` and
#'   `"TBL"` are accepted. Validation accepts any two-letter alphabetic code
#'   or `"tbl"`; it does not check a list of supported jurisdiction codes.
#' @param directory_to_save character(1). Non-empty output directory path.
#'   CSV files are saved directly in this directory, which is created if
#'   missing, including when `download = FALSE`.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed.
#' @param download logical(1). Deprecated. `FALSE` emits a warning and returns
#'   a preview of the planned files without downloading CSV files.
#'   Default is `TRUE`.
#' @param remove_command logical(1). Deprecated and ignored; `TRUE` emits a
#'   warning. Default is `FALSE`.
#' @param show_progress logical(1). Show file download progress. Default is
#'   `TRUE`. Other status messages may still be printed when `FALSE`.
#' @param hash logical(1). Return a combined MD5 hash of the files under
#'   `directory_to_save` instead of the download summary when `download = TRUE`.
#'   Ignored when `download = FALSE`. Default is `FALSE`.
#' @param max_tries integer(1). Maximum attempts per file download request,
#'   including the initial attempt. Default is 20.
#' @param rate_limit numeric(1). Request-throttling interval in seconds,
#'   passed to [download_run_method()]. Controls the average request rate,
#'   rather than guaranteeing a minimum gap between requests. Default is 2.
#' @details
#' File names depend on the selected jurisdiction. For example, requests for
#' 2020 produce `tri_raw_2020.csv` for `"US"`, `tri_raw_2020_NC.csv` for `"NC"`,
#' and `tri_raw_2020_TBL.csv` for `"tbl"`. Tribal download URLs use lowercase
#' `tbl`, while local file names use uppercase `TBL`.
#'
#' The output directory is created before returning a preview or downloading.
#' Preview mode makes no network requests. Individual download failures are
#' recorded by [download_run_method()] during file transfers.
#'
#' Previews include all planned files, including those already present locally.
#' During downloading, [download_run_method()] skips existing non-empty files;
#' missing or empty destination files are downloaded.
#'
#' Hashing includes all files recursively under `directory_to_save`, including
#' existing files and files unrelated to the requested years or jurisdiction.
#' @author Mariana Kassien, Insang Song
#' @return
#' When `download = FALSE`, an invisible list containing `urls`, `destfiles`,
#' and `n_files` is returned regardless of `hash`. The file count is the number
#' of planned URLs, including files that already exist locally.
#'
#' When `download = TRUE` and `hash = FALSE`, returns the download summary from
#' [download_run_method()] invisibly, including `success`, `failed`, and
#' `skipped` file counts. The summary always contains `failed_urls` and
#' `failed_files` character vectors, both `character(0)` when no downloads
#' failed, including when every file was skipped.
#'
#' When `download = TRUE` and `hash = TRUE`, returns a visible character scalar
#' containing the combined MD5 hash of the files under `directory_to_save`.
#' @seealso [download_run_method()], [process_tri()]
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_usepa2024tri}{amadeus}
#' @examples
#' \dontrun{
#' download_tri(
#'   year = 2021L,
#'   directory_to_save = tempdir(),
#'   jurisdiction = "NC",
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_tri <- function(
  year = c(2018L, 2022L),
  jurisdiction = "US",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "tri",
    acknowledgement = acknowledgement,
    download = download,
    remove_command = remove_command,
    unzip = FALSE,
    remove_zip = FALSE,
    directory_to_save = directory_to_save,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit,
    hash = hash
  )
  # ----------------------------------------------------------------------------
  # Check source-specific arguments.
  checkmate::assert_string(jurisdiction)
  chr_juris <- toupper(trimws(jurisdiction))
  checkmate::assert_string(chr_juris, pattern = "^([A-Z]{2}|TBL)$")
  # ----------------------------------------------------------------------------
  # Check `years` and generate sequence.
  int_years <- amadeus:::sequence_years(year)
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_dir_to_save <- amadeus:::download_setup_dir(directory_to_save)
  # ----------------------------------------------------------------------------
  # Format the validated jurisdiction for URLs and file names.
  chr_jurl <- if (chr_juris == "TBL") "tbl" else chr_juris
  chr_suffix <- if (chr_juris == "US") "" else paste0("_", chr_juris)
  # ----------------------------------------------------------------------------
  chr_urls <- sprintf(
    paste0(
      "https://data.epa.gov/efservice/downloads/tri/",
      "mv_tri_basic_download/%d_%s/csv"
    ),
    int_years,
    chr_jurl
  )
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filenames <- paste0(
    chr_dir_to_save,
    "tri_raw_",
    int_years,
    chr_suffix,
    ".csv"
  )
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus:::download_preview(chr_urls, chr_filenames)))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus:::download_run_method(
    urls = chr_urls,
    destfiles = chr_filenames,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  amadeus:::download_return(download_result, hash, chr_dir_to_save)
}

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process toxic release data
#' @description
#' This function imports and cleans raw toxic release data,
#' returning a single `SpatVector` (points) object for the selected `year`.
#' @param path character(1). Path to the directory with TRI CSV files
#' @param year integer(1). Single year to select.
#' @param variables character. One or more regular expressions used to select
#'   TRI release variables by column name after normalization to underscore
#'   naming (for example, `STACK_AIR`, `FUGITIVE_AIR`, `WATER`). Default is
#'   `"STACK_AIR"`. Matching first uses raw TRI column names, then falls back
#'   to a normalized match where punctuation and spaces are converted to
#'   underscores (for example, `"ON-SITE RELEASE TOTAL"` matches
#'   `ON_SITE_RELEASE_TOTAL`). Recommended options include:
#'   \itemize{
#'     \item `FUGITIVE_AIR`
#'     \item `STACK_AIR`
#'     \item `WATER`
#'     \item `UNDERGROUND`
#'     \item `UNDERGROUND_CL_I`
#'     \item `UNDERGROUND_C_II_V`
#'     \item `LANDFILLS`
#'     \item `RCRA_C_LANDFILL`
#'     \item `OTHER_LANDFILLS`
#'     \item `LAND_TREATMENT`
#'     \item `SURFACE_IMPNDMNT`
#'     \item `RCRA_SURFACE_IM`
#'     \item `OTHER_SURFACE_I`
#'     \item `OTHER_DISPOSAL`
#'     \item `ON_SITE_RELEASE_TOTAL`
#'     \item `POTW_TRNS_RLSE`
#'     \item `POTW_TRNS_TRT`
#'     \item `POTW_TOTAL_TRANSFERS`
#'   }
#' @param chemical `NULL` or character. Optional one or more regular
#'   expressions used to filter chemicals. Patterns are matched against
#'   `TRI_CHEMICAL_COMPOUND_ID`, `CHEMICAL`, and `CAS`/`CAS.` values. If
#'   `NULL` (default), all chemicals are retained.
#' @param industry_group character(1). Optional additional grouping level.
#'   One of `"none"` (default), `"industry_sector"`,
#'   `"industry_sector_code"`, or `"both"`.
#' @param ignore_case logical(1). If `TRUE` (default), regular expression
#'   matching in `variables` and `chemical` is case-insensitive.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @author Kyle Messier
#' @return a `SpatVector` object (points) in `year`
#' `year` is stored in a field named `"year"`.
#' @note Use [get_tri_info()] to inspect
#' available TRI chemical IDs/names/CAS numbers and industry sector codes in
#' local TRI files. Visit [TRI Data and Tools](https://www.epa.gov/toxics-release-inventory-tri-program/tri-toolbox)
#' to view the available years and variables.
#' @references
#' https://www.epa.gov/toxics-release-inventory-tri-program/tri-toolbox
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
#' @importFrom stats setNames
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' tri <- process_tri(
#'   path = "./data",
#'   year = 2020,
#'   variables = c("STACK_AIR", "FUGITIVE_AIR"),
#'   chemical = "benzene",
#'   industry_group = "industry_sector"
#' )
#' }
# nolint end
#' @export
process_tri <- function(
  path = NULL,
  year = 2018,
  variables = "STACK_AIR",
  chemical = NULL,
  industry_group = c(
    "none",
    "industry_sector",
    "industry_sector_code",
    "both"
  ),
  ignore_case = TRUE,
  extent = NULL,
  ...
) {
  if (!is.character(variables) || length(variables) < 1 || anyNA(variables)) {
    stop(
      "`variables` must be a non-empty character vector of regex patterns.\n"
    )
  }
  if (length(variables) > 0 && any(!nzchar(trimws(variables)))) {
    stop("`variables` cannot include empty patterns.\n")
  }
  if (!is.null(chemical)) {
    if (!is.character(chemical) || length(chemical) < 1 || anyNA(chemical)) {
      stop("`chemical` must be NULL or a non-empty character vector.\n")
    }
    if (any(!nzchar(trimws(chemical)))) {
      stop("`chemical` cannot include empty patterns.\n")
    }
  }
  if (
    !is.logical(ignore_case) || length(ignore_case) != 1 || is.na(ignore_case)
  ) {
    stop("`ignore_case` must be TRUE or FALSE.\n")
  }
  industry_group <- match.arg(industry_group)

  dt_tri <- tri_read_raw(path = path)

  required_cols <- c(
    "YEAR",
    "LONGITUDE",
    "LATITUDE",
    "TRI_CHEMICAL_COMPOUND_ID",
    "UNIT_OF_MEASURE"
  )
  missing_required <- setdiff(required_cols, names(dt_tri))
  if (length(missing_required) > 0) {
    stop(
      "TRI input is missing required columns: ",
      paste(missing_required, collapse = ", "),
      "\n"
    )
  }

  select_by_pattern <- function(column_names, patterns) {
    unique(unlist(
      lapply(
        patterns,
        function(pat) {
          grep(
            pat,
            column_names,
            ignore.case = ignore_case,
            value = TRUE
          )
        }
      )
    ))
  }
  normalize_name <- function(x) {
    x <- gsub("[^[:alnum:]]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x
  }
  select_by_normalized_pattern <- function(column_names, patterns) {
    normalized_names <- normalize_name(column_names)
    if (ignore_case) {
      normalized_names <- tolower(normalized_names)
    }
    matched_idx <- unique(unlist(
      lapply(
        patterns,
        function(pat) {
          pat_norm <- normalize_name(pat)
          if (ignore_case) {
            pat_norm <- tolower(pat_norm)
          }
          grep(
            pat_norm,
            normalized_names,
            fixed = TRUE
          )
        }
      )
    ))
    column_names[matched_idx]
  }

  selected_variable_cols <- select_by_pattern(names(dt_tri), variables)
  if (length(selected_variable_cols) < 1) {
    selected_variable_cols <-
      select_by_normalized_pattern(names(dt_tri), variables)
  }
  selected_variable_cols <- setdiff(selected_variable_cols, required_cols)
  if (length(selected_variable_cols) < 1) {
    stop("`variables` did not match any TRI variable columns.\n")
  }

  industry_cols <- switch(
    industry_group,
    none = character(0),
    industry_sector = "INDUSTRY_SECTOR",
    industry_sector_code = "INDUSTRY_SECTOR_CODE",
    both = c("INDUSTRY_SECTOR_CODE", "INDUSTRY_SECTOR")
  )
  missing_industry <- setdiff(industry_cols, names(dt_tri))
  if (length(missing_industry) > 0) {
    stop(
      "TRI input is missing industry grouping columns: ",
      paste(missing_industry, collapse = ", "),
      "\n"
    )
  }

  tri_chemical_fields <- intersect(
    c("TRI_CHEMICAL_COMPOUND_ID", "CHEMICAL", "CAS", "CAS_"),
    names(dt_tri)
  )

  selected_cols <- unique(c(
    required_cols,
    selected_variable_cols,
    tri_chemical_fields,
    industry_cols
  ))
  dt_tri <- dt_tri[, selected_cols, drop = FALSE]
  dt_tri <- dt_tri[dt_tri$YEAR == year, ]
  if (nrow(dt_tri) < 1) {
    stop("No TRI rows found for requested `year`.\n")
  }

  if (!is.null(chemical)) {
    chemical_filter <- rep(FALSE, nrow(dt_tri))
    for (field in tri_chemical_fields) {
      field_vals <- as.character(dt_tri[[field]])
      field_hits <- Reduce(
        `|`,
        lapply(
          chemical,
          function(pat) grepl(pat, field_vals, ignore.case = ignore_case)
        )
      )
      chemical_filter <- chemical_filter | field_hits
    }
    dt_tri <- dt_tri[chemical_filter, , drop = FALSE]
    if (nrow(dt_tri) < 1) {
      stop("`chemical` did not match any TRI rows for requested year.\n")
    }
  }

  for (col_nm in selected_variable_cols) {
    original_col <- dt_tri[[col_nm]]
    numeric_col <- suppressWarnings(as.numeric(original_col))
    if (any(!is.na(original_col) & is.na(numeric_col))) {
      stop("Selected TRI variable column `", col_nm, "` is not numeric.\n")
    }
    dt_tri[[col_nm]] <- numeric_col
  }

  # depending on the way the chemicals are summarized
  # Unit is kilogram
  # nolint start
  unit_to_kg <- function(value, unit) {
    ifelse(
      unit == "Pounds",
      value * (453.592 / 1e3),
      ifelse(
        unit %in% c("Grams", "Gram"),
        value / 1e3,
        value
      )
    )
  }
  group_fields <- c(
    "YEAR",
    "LONGITUDE",
    "LATITUDE",
    "TRI_CHEMICAL_COMPOUND_ID",
    industry_cols
  )
  names_from_cols <- c(industry_cols, "TRI_CHEMICAL_COMPOUND_ID")
  tri_name_prefix <- if (length(selected_variable_cols) == 1) {
    paste0(selected_variable_cols, "_")
  } else {
    ""
  }
  dt_tri_x <-
    dt_tri |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(selected_variable_cols),
        ~ unit_to_kg(., UNIT_OF_MEASURE)
      )
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_fields))
    ) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(selected_variable_cols),
        ~ sum(., na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      values_from = dplyr::all_of(selected_variable_cols),
      names_from = dplyr::all_of(names_from_cols),
      names_prefix = tri_name_prefix,
      names_sep = "_",
      values_fill = 0
    ) |>
    dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE))
  names(dt_tri_x) <- sub(" ", "_", names(dt_tri_x))
  tri_value_cols <- setdiff(names(dt_tri_x), c("YEAR", "LONGITUDE", "LATITUDE"))
  if (length(tri_value_cols) > 0L) {
    tri_value_df <- dt_tri_x[, tri_value_cols, drop = FALSE]
    has_tri_signal <- rowSums(!is.na(tri_value_df) & tri_value_df != 0) > 0
    dt_tri_x <- dt_tri_x[has_tri_signal, , drop = FALSE]
  }
  if (nrow(dt_tri_x) < 1) {
    stop("No TRI sites found after filtering missing/zero source values.\n")
  }

  spvect_tri <-
    terra::vect(
      dt_tri_x,
      geom = c("LONGITUDE", "LATITUDE"),
      crs = "EPSG:4269", # all are NAD83
      keepgeom = TRUE
    )
  attr(spvect_tri, "tri_year") <- year
  tri_target_fields <- setdiff(
    names(spvect_tri),
    c("YEAR", "LONGITUDE", "LATITUDE")
  )
  attr(spvect_tri, "tri_target_fields") <- tri_target_fields
  attr(spvect_tri, "tri_grouping") <- names_from_cols
  attr(spvect_tri, "tri_variables") <- selected_variable_cols
  attr(spvect_tri, "tri_chemical_selector") <- chemical
  if (!is.null(extent)) {
    tri_final <- apply_extent(spvect_tri, extent)
    return(tri_final)
  } else {
    return(spvect_tri)
  }
}
# nolint end

################################################################################
# Internal TRI helper: read and normalize TRI csv column names
tri_read_raw <- function(path = NULL) {
  csvs_tri_from <-
    list.files(path = path, pattern = "*.csv$", full.names = TRUE)
  if (length(csvs_tri_from) < 1) {
    stop("No TRI CSV files found in `path`.\n")
  }
  csvs_tri <- lapply(csvs_tri_from, read.csv)
  dt_tri <- data.table::rbindlist(csvs_tri)
  tri_cns <- colnames(dt_tri)
  tri_cns <- sub(".*?\\.\\.", "", tri_cns)
  tri_cns <- sub("^[^A-Za-z]*", "", tri_cns)
  tri_cns <- gsub("\\.", "_", tri_cns)
  dt_tri <- stats::setNames(dt_tri, tri_cns)
  dt_tri <- as.data.frame(dt_tri, stringsAsFactors = FALSE)
  return(dt_tri)
}

################################################################################
#' Get TRI lookup information for chemicals or industries
#' @description
#' Returns a lookup table from local TRI files. By default it returns chemical
#' information (`TRI_CHEMICAL_COMPOUND_ID`, `CHEMICAL`, `CASN`). Set
#' `type = "industries"` to return industry sector information
#' (`INDUSTRY_SECTOR_CODE`, `INDUSTRY_SECTOR`).
#' @param path character(1). Path to the directory with TRI CSV files
#'   (from `download_tri`).
#' @param type character(1). Lookup table to return. One of `"chemicals"`
#'   (default) or `"industries"`.
#' @param year `NULL` or integer(1). Optional single year filter. If `NULL`
#'   (default), all years in `path` are included.
#' @param include_na logical(1). If `FALSE` (default), rows where lookup fields
#'   are all missing are removed.
#' @param ... Placeholders.
#' @return a `data.frame` containing the requested TRI lookup table.
#' @author Kyle Messier
#' @examples
#' \dontrun{
#' get_tri_info(path = "./data")
#' get_tri_info(path = "./data", type = "industries")
#' get_tri_info(path = "./data", year = 2020)
#' }
#' @export
get_tri_info <- function(
  path = NULL,
  type = c("chemicals", "industries"),
  year = NULL,
  include_na = FALSE,
  ...
) {
  type <- match.arg(type)
  dt_tri <- tri_read_raw(path = path)
  if (!is.null(year)) {
    if (!is.numeric(year) || length(year) != 1 || is.na(year)) {
      stop("`year` must be NULL or a single numeric value.\n")
    }
    if (!("YEAR" %in% names(dt_tri))) {
      stop("TRI input is missing `YEAR` column needed for filtering.\n")
    }
    dt_tri <- dt_tri[dt_tri$YEAR == year, , drop = FALSE]
  }

  if (type == "chemicals") {
    required_cols <- c("TRI_CHEMICAL_COMPOUND_ID", "CHEMICAL")
    missing_cols <- setdiff(required_cols, names(dt_tri))
    if (length(missing_cols) > 0) {
      stop(
        "TRI input is missing required chemical lookup columns: ",
        paste(missing_cols, collapse = ", "),
        "\n"
      )
    }
    cas_col <- if ("CAS" %in% names(dt_tri)) {
      "CAS"
    } else if ("CAS_" %in% names(dt_tri)) {
      "CAS_"
    } else {
      NULL
    }
    cas_vals <- if (is.null(cas_col)) {
      rep(NA_character_, nrow(dt_tri))
    } else {
      as.character(dt_tri[[cas_col]])
    }
    out <- data.frame(
      TRI_CHEMICAL_COMPOUND_ID = as.character(dt_tri$TRI_CHEMICAL_COMPOUND_ID),
      CHEMICAL = as.character(dt_tri$CHEMICAL),
      CASN = cas_vals,
      stringsAsFactors = FALSE
    )
    if (!include_na) {
      out <- out[
        !(is.na(out$TRI_CHEMICAL_COMPOUND_ID) &
          is.na(out$CHEMICAL) &
          is.na(out$CASN)),
        ,
        drop = FALSE
      ]
    }
    out <- unique(out)
    out <- out[
      order(out$CHEMICAL, out$TRI_CHEMICAL_COMPOUND_ID, out$CASN),
      ,
      drop = FALSE
    ]
  } else {
    required_cols <- c("INDUSTRY_SECTOR_CODE", "INDUSTRY_SECTOR")
    missing_cols <- setdiff(required_cols, names(dt_tri))
    if (length(missing_cols) > 0) {
      stop(
        "TRI input is missing required industry lookup columns: ",
        paste(missing_cols, collapse = ", "),
        "\n"
      )
    }
    out <- data.frame(
      INDUSTRY_SECTOR_CODE = as.character(dt_tri$INDUSTRY_SECTOR_CODE),
      INDUSTRY_SECTOR = as.character(dt_tri$INDUSTRY_SECTOR),
      stringsAsFactors = FALSE
    )
    if (!include_na) {
      out <- out[
        !(is.na(out$INDUSTRY_SECTOR_CODE) &
          is.na(out$INDUSTRY_SECTOR)),
        ,
        drop = FALSE
      ]
    }
    out <- unique(out)
    out <- out[
      order(out$INDUSTRY_SECTOR_CODE, out$INDUSTRY_SECTOR),
      ,
      drop = FALSE
    ]
  }

  rownames(out) <- NULL
  return(out)
}

################################################################################
# {calculate} tier
################################################################################
#' Calculate toxic release covariates
#' @description
#' Calculate toxic release values for polygons or isotropic buffer point
#' locations. Returns a \code{data.frame} object containing \code{locs_id}
#' and variables for each processed TRI field in \code{from}. Target fields are
#' derived from metadata attached by \code{process_tri()}, with a fallback to
#' non-coordinate columns in \code{from}.
#' @param from SpatVector(1). Output of \code{process_tri()}.
#' @param locs sf/SpatVector. Locations where TRI variables are calculated.
#' @param locs_id character(1). Unique site identifier column name.
#'  Default is `"site_id"`.
#' @param decay_range Circular buffer radius.
#' Default is \code{c(1000, 10000, 50000)} (meters)
#' @param C0 `NULL` or character vector of column names in `from`.
#' Optional source-value columns used by `sum_edc()`. If `NULL` and
#' there is one TRI target field, that field is inferred with a warning.
#' If `NULL` and there are multiple TRI target fields, each TRI target field
#' is used as its own source values (for example `STACK_AIR_*`).
#' @param use_threshold logical(1). Passed to \code{sum_edc()}. If `TRUE`
#' (default), include only source points within \code{5 * decay_range}.
#' If `FALSE`, include all source points in `from`.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Insang Song, Mariana Kassien
#' @return a data.frame or SpatVector object
#' @note U.S. context.
#' @seealso [`sum_edc`], [`process_tri`]
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
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_tri(
#'   from = tri, # derived from process_tri() example
#'   locs = loc,
#'   locs_id = "id",
#'   decay_range = c(1e3L, 1e4L, 5e4L)
#' )
#' }
#' @export
calculate_tri <- function(
  from = NULL,
  locs,
  locs_id = "site_id",
  decay_range = c(1e3L, 1e4L, 5e4L),
  C0 = NULL, # nolint: object_name_linter
  use_threshold = TRUE,
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_geom(geom)
  if (!methods::is(locs, "SpatVector")) {
    if (methods::is(locs, "sf")) {
      locs <- terra::vect(locs)
    }
  }
  if (!is.numeric(decay_range)) {
    stop("`decay_range` should be numeric.\n")
  }
  if (
    !is.logical(use_threshold) ||
      length(use_threshold) != 1L ||
      is.na(use_threshold)
  ) {
    stop("`use_threshold` must be TRUE or FALSE.\n")
  }
  c0_input <- C0
  if (
    !is.null(c0_input) &&
      (!is.character(c0_input) ||
        length(c0_input) < 1L ||
        anyNA(c0_input) ||
        any(!nzchar(trimws(c0_input))))
  ) {
    stop("`C0` must be NULL or a non-empty character vector of column names.\n")
  }
  locs_re <- terra::project(locs, terra::crs(from))

  # split by year: locs and tri locations
  tri_cols <- attr(from, "tri_target_fields")
  if (is.null(tri_cols) || length(tri_cols) < 1) {
    tri_cols <- setdiff(names(from), c("YEAR", "LONGITUDE", "LATITUDE"))
  }
  # error fix: no whitespace
  tri_cols <- sub(" ", "_", tri_cols)
  if (length(tri_cols) < 1) {
    stop(
      "No TRI target fields found in `from`. ",
      "Process TRI data using `process_tri()` before calculation.\n"
    )
  }
  if (is.null(c0_input)) {
    if (length(tri_cols) == 1L) {
      warning(
        "`C0` is NULL and only one TRI field is available; ",
        "using `",
        tri_cols[1],
        "` as source values.\n"
      )
    }
    c0_cols <- tri_cols
  } else {
    missing_c0_cols <- setdiff(c0_input, names(from))
    if (length(missing_c0_cols) > 0L) {
      stop(
        "The following `C0` columns are missing in `from`: ",
        paste(missing_c0_cols, collapse = ", "),
        "\n"
      )
    }
    if (length(c0_input) == 1L) {
      c0_cols <- rep(c0_input, length(tri_cols))
    } else if (length(c0_input) == length(tri_cols)) {
      c0_cols <- c0_input
    } else {
      stop(
        "`C0` must have length 1 or match the number of TRI target fields (",
        length(tri_cols),
        ").\n"
      )
    }
  }

  # inner lapply
  list_decay_range <- split(decay_range, decay_range)
  list_locs_tri <-
    Map(
      function(x) {
        locs_tri_s <- Reduce(
          function(df_x, df_y) dplyr::full_join(df_x, df_y, by = locs_id),
          lapply(
            seq_along(tri_cols),
            function(i) {
              tri_col <- tri_cols[i]
              tri_col_c0 <- from[[c0_cols[i]]]
              if (is.data.frame(tri_col_c0) && ncol(tri_col_c0) == 1L) {
                tri_col_c0 <- tri_col_c0[[1]]
              }
              if (!is.numeric(tri_col_c0)) {
                stop("TRI target field `", tri_col, "` is not numeric.\n")
              }
              sum_edc(
                locs = locs_re,
                from = from,
                locs_id = locs_id,
                decay_range = x,
                target_fields = tri_col,
                C0 = tri_col_c0,
                use_threshold = use_threshold,
                geom = FALSE
              )
            }
          )
        )
        return(locs_tri_s)
      },
      list_decay_range
    )

  # bind element data.frames into one
  df_tri <- Reduce(function(x, y) dplyr::full_join(x, y), list_locs_tri)
  locs_df <- as.data.frame(locs)
  if (!locs_id %in% names(locs_df)) {
    stop("`locs_id` was not found in `locs`.\n")
  }
  if (geom %in% c("sf", "terra")) {
    locs_geom <- terra::as.data.frame(locs_re, geom = "WKT")
    if (!locs_id %in% names(locs_geom)) {
      stop("`locs_id` was not found in `locs` after CRS transformation.\n")
    }
    df_tri <- dplyr::left_join(
      locs_geom[, c(locs_id, "geometry"), drop = FALSE],
      df_tri,
      by = locs_id
    )
  } else if (nrow(df_tri) != nrow(locs)) {
    df_tri <- dplyr::left_join(locs_df, df_tri, by = locs_id)
  }

  df_tri_return <- amadeus::calc_return_locs(
    covar = df_tri,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )

  # read attr
  df_tri_return$time <- as.integer(attr(from, "tri_year"))

  return(df_tri_return)
}
################################################################################
