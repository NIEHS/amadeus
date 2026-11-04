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
#' @param year integer(1 or 2). Year or start/end years for downloading data.
#' @param directory_to_save character(1). Directory to download files.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param jurisdiction character(1). TRI file variant to download. Use
#'   \code{"US"} for the nationwide file, a two-letter state or territory code
#'   such as \code{"AZ"} or \code{"NC"} for a jurisdiction-specific file, or
#'   \code{"tbl"} for the tribal file. Default is \code{"US"}.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mariana Kassien, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
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
  directory_to_save = NULL,
  acknowledgement = FALSE,
  jurisdiction = "US",
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### Check years
  year <- if (length(year) == 1) rep(year, 2) else year
  stopifnot(length(year) == 2)
  year <- year[order(year)]

  #### Check jurisdiction
  if (
    !is.character(jurisdiction) ||
      length(jurisdiction) != 1 ||
      is.na(jurisdiction)
  ) {
    stop(
      "`jurisdiction` must be a single character value such as ",
      "\"US\", \"AZ\", or \"tbl\".\n",
      call. = FALSE
    )
  }
  jurisdiction <- trimws(jurisdiction)
  if (!nzchar(jurisdiction)) {
    stop(
      "`jurisdiction` must be \"US\", a two-letter state code, or \"tbl\".\n",
      call. = FALSE
    )
  }
  jurisdiction_upper <- toupper(jurisdiction)
  if (identical(jurisdiction_upper, "TBL")) {
    jurisdiction_url <- "tbl"
    jurisdiction_suffix <- "_tbl"
  } else if (
    identical(jurisdiction_upper, "US") ||
      grepl("^[A-Z]{2}$", jurisdiction_upper)
  ) {
    jurisdiction_url <- jurisdiction_upper
    jurisdiction_suffix <- if (identical(jurisdiction_upper, "US")) {
      ""
    } else {
      paste0("_", jurisdiction_upper)
    }
  } else {
    stop(
      "`jurisdiction` must be \"US\", a two-letter state code such as ",
      "\"AZ\", or \"tbl\".\n",
      call. = FALSE
    )
  }

  #### Handle deprecated parameters
  if (!isTRUE(download)) {
    warning(
      "Setting download=FALSE is deprecated.",
      " Downloads now use httr2 by default.\n",
      "To skip downloading, the function will return",
      " after discovering files.\n",
      call. = FALSE
    )
  }

  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### Define measurement data paths
  url_download <- paste0(
    # nolint: line_length_linter.
    "https://data.epa.gov/efservice/downloads/tri/",
    "mv_tri_basic_download/"
  )
  year_sequence <- seq(year[1], year[2], 1)
  download_urls <- paste0(
    url_download,
    year_sequence,
    "_",
    jurisdiction_url,
    "/csv"
  )
  download_names <- paste0(
    directory_to_save,
    "tri_raw_",
    year_sequence,
    jurisdiction_suffix,
    ".csv"
  )

  #### Filter to files that need downloading
  needs_download <- sapply(download_names, amadeus::check_destfile)
  download_urls_filtered <- download_urls[needs_download]
  download_names_filtered <- download_names[needs_download]

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available for download.\n",
      length(download_urls_filtered)
    ))
    return(invisible(list(
      urls = download_urls_filtered,
      destfiles = download_names_filtered,
      n_files = length(download_urls_filtered)
    )))
  }

  #### Download files using httr2
  download_result <- amadeus::download_run_method(
    urls = download_urls_filtered,
    destfiles = download_names_filtered,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  message("Requests were processed.\n")

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
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
