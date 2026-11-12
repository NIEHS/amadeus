################################################################################
# {amadeus} functions for National Land Cover Database (NLCD).
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download National Land Cover Database (NLCD) data
#' @description
#' Downloads NLCD data products from the Multi-Resolution Land Characteristics
#' (MRLC) Consortium. NLCD provides nationwide land cover and land cover change
#' information for the United States at a 30m resolution.
#' @param product character(1). NLCD product type. One of:
#' \itemize{
#'   \item "Land Cover" (default)
#'   \item "Land Cover Change"
#'   \item "Land Cover Confidence"
#'   \item "Fractional Impervious Surface"
#'   \item "Impervious Descriptor"
#'   \item "Spectral Change Day of Year"
#' }
#' @param year integer(1 or more). One year, two endpoints, or a non-empty
#'   vector of years. Every year from the minimum through the maximum is
#'   requested, including gaps: `c(2018, 2023)` requests `2018:2023`.
#'   Requested years must be within 1985-2024. Default is 2021.
#'   Missing, fractional, and future years are rejected.
#' @param directory_to_save character(1). Parent directory. Archives are saved
#'   in `zip_files/` and extracted data in `data_files/`.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed with
#' download.
#' @param download logical(1). DEPRECATED. Downloads now happen automatically.
#'   Set to FALSE to skip downloading (generates file list only).
#' @param remove_command logical(1). Deprecated, ignored.
#' @param unzip logical(1). Extract all planned yearly archives, including
#'   existing archives, into the shared `data_files/` directory. Default is
#'   `TRUE`; `FALSE` skips extraction.
#' @param remove_zip logical(1). Remove archives and their `zip_files/`
#'   directory recursively, including remaining contents, after the extraction
#'   loop finishes. Default is `FALSE`; requires `unzip = TRUE`.
#' @param show_progress logical(1). Show download progress? Default is
#' \code{TRUE}.
#' @param hash logical(1). Return a combined MD5 hash of all files recursively
#'   under `data_files/`, including existing files. Default is `FALSE`; ignored
#'   when `download = FALSE`. With `unzip = FALSE`, hashing still uses
#'   `data_files/`, which may be empty or contain previously extracted data.
#' @param max_tries integer(1). Maximum download retry attempts. Default is 20.
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song, Kyle Messier
#' @return
#' When `download = TRUE` and `hash = FALSE`, an invisible list containing
#' `success`, `failed`, and `skipped` file counts. If all requested archives
#' already exist and are nonempty, no files are downloaded; the returned list
#' contains `success = 0`, `failed = 0`, and `skipped` equal to the number of
#' requested files. The list always contains `failed_urls` and `failed_files`
#' character vectors, both `character(0)` when no downloads failed, including
#' when every file was skipped.
#'
#' When `download = FALSE`, an invisible list containing `urls`, `destfiles`,
#' and `n_files` is returned without downloading, regardless of `hash`.
#'
#' When `download = TRUE` and `hash = TRUE`, a character(1) combined MD5 hash
#' of the files in the `data_files` directory is returned instead of a list.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{dewitz_national_2023}{amadeus}
#' @examples
#' \dontrun{
#' # Download 2021 Land Cover
#' download_nlcd(
#'   product = "Land Cover",
#'   year = 2021,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#'
#' # Download Land Cover Change for 2019
#' download_nlcd(
#'   product = "Land Cover Change",
#'   year = 2019,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   unzip = TRUE,
#'   remove_zip = TRUE
#' )
#' }
#' @export
download_nlcd <- function(
  product = "Land Cover",
  year = 2021,
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
    dataset_name = "nlcd",
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
  checkmate::assert_choice(
    tolower(product),
    c(
      "land cover",
      "land cover change",
      "land cover confidence",
      "fractional impervious surface",
      "impervious descriptor",
      "spectral change day of year"
    )
  )
  # ----------------------------------------------------------------------------
  chr_collection <- switch(
    tolower(product),
    "land cover" = "LndCov",
    "land cover change" = "LndChg",
    "land cover confidence" = "LndCnf",
    "fractional impervious surface" = "FctImp",
    "impervious descriptor" = "ImpDsc",
    "spectral change day of year" = "SpcChg"
  )
  # ----------------------------------------------------------------------------
  # Check `years` and generate sequence.
  int_years <- amadeus:::sequence_years(year, min = 1985, max = 2024)
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus:::download_setup_dir(directory_to_save, zip = TRUE)
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Generate download URLs.
  chr_urls <- sprintf(
    paste0(
      "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/",
      "data-bundles/Annual_NLCD_%s_%d_CU_C1V1.zip"
    ),
    chr_collection,
    int_years
  )
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filenames <- sprintf(
    paste0(chr_dir_to_download, "Annual_NLCD_%s_%d_CU_C1V1.zip"),
    chr_collection,
    int_years
  )
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus:::download_preview(chr_urls, chr_filenames)))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  # http_version = 2L forces HTTP/1.1 to avoid nghttp2 PROTOCOL_ERRORs from
  # www.mrlc.gov, which drops HTTP/2 connections mid-transfer.
  download_result <- amadeus:::download_run_method(
    urls = chr_urls,
    destfiles = chr_filenames,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit,
    http_version = 2L
  )
  # ----------------------------------------------------------------------------
  # Unzip downloaded .zip files.
  amadeus:::download_unzip(
    file_name = chr_filenames,
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
#' Process land cover data
#' @description
#' The \code{process_nlcd()} function imports and cleans raw land cover data,
#' returning a single `SpatRaster` object.
#' @param path character giving nlcd data path
#' @param year numeric giving the year of NLCD data used
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @description Reads NLCD file of selected `year`.
#' @return a `SpatRaster` object
#' @author Eva Marques, Insang Song
#' @importFrom utils read.csv
#' @importFrom tools file_path_sans_ext
#' @importFrom terra rast metags
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' nlcd <- process_nlcd(
#'   path = "./data/",
#'   year = 2021
#' )
#' }
#' @export
process_nlcd <-
  function(
    path = NULL,
    year = 2021,
    extent = NULL,
    ...
  ) {
    # check inputs
    if (!is.character(path) || is.null(path)) {
      stop("path is not a character.")
    }
    if (!dir.exists(path)) {
      stop("path does not exist.")
    }
    if (!is.numeric(year)) {
      stop("year is not a numeric.")
    }
    product_codes <- c(
      "LndCov",
      "LndChg",
      "LndCnf",
      "FctImp",
      "ImpDsc",
      "SpcChg"
    )
    # open nlcd file corresponding to the year
    # Try new naming convention (with recursive search for subdirectories)
    nlcd_file <-
      list.files(
        path,
        pattern = paste0(
          "Annual_NLCD_(",
          paste(product_codes, collapse = "|"),
          ")_",
          year,
          "_.*\\.(tif|img)$"
        ),
        full.names = TRUE,
        recursive = TRUE, # ADD THIS - files may be in subdirectories
        ignore.case = TRUE # ADD THIS - for robustness
      )

    if (length(nlcd_file) == 0) {
      message("No NLCD files detected. Trying deprecated file names...")
      nlcd_file <-
        list.files(
          path,
          pattern = paste0("nlcd_", year, "_.*\\.(tif|img)$"),
          full.names = TRUE,
          recursive = TRUE, # ADD THIS
          ignore.case = TRUE # ADD THIS
        )
      if (length(nlcd_file) > 0) {
        # FIXED: was > 1, should be > 0
        message(
          paste0(
            "Deprecated file paths detected. Data still imported, but ",
            "see https://www.mrlc.gov/data/project/annual-nlcd for updated ",
            "NLCD documentation and availability."
          )
        )
      }
    }

    # check if name without extension is duplicated
    nlcd_file_base <- basename(nlcd_file)
    nlcd_file_base <- tools::file_path_sans_ext(nlcd_file_base)
    if (any(duplicated(nlcd_file_base))) {
      stop("Duplicated NLCD files are detected. Please remove duplicates.")
    }
    if (length(nlcd_file) == 0) {
      stop("NLCD data not available for this year.")
    }

    # NLCD C1V1 bug
    # `.aux.xml` metadata file was causing `NA` values to be read as `NaN`,
    # corrupting the factor/integer data values when used downstream.
    # File is hidden with preceding `._` for retention but exclusion in
    # metadata definitions.
    chr_aux_xml_path <- list.files(
      path,
      pattern = paste0(
        "Annual_NLCD_(",
        paste(product_codes, collapse = "|"),
        ")_",
        year,
        "_.*\\.aux\\.xml$" # FIXED: escaped the dot before xml
      ),
      full.names = FALSE,
      recursive = TRUE, # ADD THIS
      ignore.case = TRUE # ADD THIS
    )

    if (length(chr_aux_xml_path) > 0) {
      # FIXED: handle multiple files
      for (aux_file in chr_aux_xml_path) {
        chr_aux_xml_hide <- file.path(
          dirname(file.path(path, aux_file)),
          paste0("._", basename(aux_file))
        )
        chr_aux_xml_full <- file.path(path, aux_file)

        if (file.exists(chr_aux_xml_full) && !file.exists(chr_aux_xml_hide)) {
          message(paste0(
            "Hiding corrupt ",
            basename(aux_file),
            " metadata file."
          ))
          file.rename(chr_aux_xml_full, chr_aux_xml_hide)
        }
      }
    }

    nlcd <- terra::rast(nlcd_file, win = extent)
    nlcd_product <- unique(unlist(lapply(
      nlcd_file_base,
      function(x) {
        product_code <- regmatches(
          x,
          regexpr(
            paste(product_codes, collapse = "|"),
            x,
            ignore.case = TRUE
          )
        )
        product_codes[match(tolower(product_code), tolower(product_codes))]
      }
    )))
    nlcd_product <- nlcd_product[!is.na(nlcd_product)]
    nlcd_metadata <- paste0("Year=", year)
    if (length(nlcd_product) == 1L) {
      nlcd_metadata <- c(
        nlcd_metadata,
        paste0("Product=", nlcd_product)
      )
    }
    terra::metags(nlcd) <- nlcd_metadata
    return(nlcd)
  }

################################################################################
# {calculate} tier
################################################################################
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
#'   or `"terra"` (using [`terra::freq()`]). Ignored if `locs` are points.
#' @param radius numeric (non-negative) giving the
#' radius of buffer around points.
#' @param class_names character(1). Column naming scheme for categorical NLCD
#' products. Use `"code"` (default) for numeric values or `"mrlc"` for
#' standardized MRLC class names. The `"mrlc"` option is not available for
#' Land Cover Confidence, Fractional Impervious Surface, or Spectral Change
#' Day of Year because their values are not MRLC classes. For the Land Cover
#' Change product, four-digit change codes are named as
#' `"<from_class>_to_<to_class>"`; unchanged two-digit classes retain their
#' standard MRLC names.
#' @param drop logical(1). Default `FALSE`. For buffered outputs (`radius > 0`),
#'   retain NLCD class columns even when all values are 0 (`drop = FALSE`) or
#'   remove class columns that are all 0 across all locations (`drop = TRUE`).
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values may expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`. Only valid when
#' `mode = "exact"`.
#' See [`exactextractr::exact_extract`] for details.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
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
#' @importFrom terra rast project vect crs set.crs buffer
#' @importFrom sf st_union st_geometry
#' @importFrom terra intersect metags
#' @importFrom exactextractr exact_extract
#' @importFrom collapse rowbind
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_nlcd(
#'   from = nlcd, # derived from process_nlcd() example
#'   locs = loc,
#'   locs_id = "id",
#'   mode = "exact",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_nlcd <- function(
  from,
  locs,
  locs_id = "site_id",
  mode = c("exact", "terra"),
  radius = 1000,
  class_names = c("code", "mrlc"),
  drop = FALSE,
  weights = NULL,
  max_cells = 5e7,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  # check inputs
  mode <- match.arg(mode)
  class_names <- match.arg(class_names)
  if (!is.numeric(radius)) {
    stop("radius is not a numeric.")
  }
  if (radius < 0) {
    stop("radius has not a likely value.")
  }
  if (!is.logical(drop) || length(drop) != 1 || is.na(drop)) {
    stop("`drop` should be a single logical value (TRUE/FALSE).")
  }
  if (!methods::is(from, "SpatRaster")) {
    stop("from is not a SpatRaster.")
  }

  # currently only handles 1 year
  if (terra::nlyr(from) > 1) {
    stop(
      paste0(
        "`from` contains more than one data layer. Current version ",
        "only processes one year worth of NLCD data."
      )
    )
  }

  nlcd_product <- get_nlcd_product(from)
  products_without_classes <- c(
    LndCnf = "Land Cover Confidence",
    FctImp = "Fractional Impervious Surface",
    SpcChg = "Spectral Change Day of Year"
  )
  if (
    class_names == "mrlc" &&
      nlcd_product %in% names(products_without_classes)
  ) {
    stop(
      sprintf(
        paste0(
          "`class_names = \"mrlc\"` is not available for %s because its ",
          "values are not MRLC classes; use `class_names = \"code\"`."
        ),
        unname(products_without_classes[nlcd_product])
      )
    )
  }

  # prepare locations
  locs_prepared <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  locs_vector <- locs_prepared[[1]]
  locs_df <- locs_prepared[[2]]

  # detect new or deprecated file path stucture
  if (identical(names(from), "NLCD Land Cover Class")) {
    message(
      paste0(
        "Deprecated data format detected. Data still analyzed, but ",
        "see https://www.mrlc.gov/data/project/annual-nlcd for updated ",
        "NLCD documentation and availability."
      )
    )
  }
  nlcd_metadata <- terra::metags(from)
  year_index <- which(tolower(nlcd_metadata$name) == "year")
  if (length(year_index) == 0L) {
    year_index <- nrow(nlcd_metadata)
  }
  year <- as.integer(nlcd_metadata$value[year_index[1]])
  stopifnot(year %in% 1985:2024L)

  # select points within mainland US and reproject on nlcd crs if necessary
  data_vect_b <-
    terra::project(locs_vector, y = terra::crs(from))
  is_point_locs <- all(
    tolower(terra::geomtype(locs_vector)) %in% c("points", "point")
  )

  message(
    paste0(
      "Calculating NLCD Land Cover Class covariates for ",
      year,
      "..."
    )
  )

  if (radius <= 0 && is_point_locs) {
    new_data_vect <- suppressMessages(
      amadeus::calc_worker(
        dataset = "nlcd",
        from = from,
        locs_vector = data_vect_b,
        locs_df = locs_df,
        fun = "mean",
        variable = 1,
        time = NULL,
        time_type = "timeless",
        radius = 0,
        level = NULL,
        weights = weights
      )
    )
    if ("geometry" %in% names(new_data_vect)) {
      new_data_vect <- cbind(
        new_data_vect[, c(locs_id, "geometry"), drop = FALSE],
        as.integer(year),
        new_data_vect[,
          setdiff(names(new_data_vect), c(locs_id, "geometry")),
          drop = FALSE
        ]
      )
    } else {
      new_data_vect <- cbind(
        new_data_vect[, locs_id, drop = FALSE],
        as.integer(year),
        new_data_vect[, setdiff(names(new_data_vect), locs_id), drop = FALSE]
      )
    }
    value_col <- setdiff(names(new_data_vect), c(locs_id, "geometry", "time"))
    if (length(value_col) == 1L) {
      names(new_data_vect)[names(new_data_vect) == value_col] <- sprintf(
        "NLCD_VALUE_%05d",
        as.integer(radius)
      )
    }
  } else {
    # create circle buffers with buf_radius
    #bufs_pol <- terra::buffer(data_vect_b, width = radius)
    bufs_pol <- data_vect_b
    if (mode == "terra") {
      # terra mode
      # class_query <- "names"
      # extract land cover class in each buffer
      nlcd_at_bufs <- Map(
        function(i) {
          terra::freq(
            from,
            zones = bufs_pol[i, ],
            wide = TRUE
          )
        },
        seq_len(nrow(bufs_pol))
      )
      nlcd_at_bufs_fill <- amadeus::collapse_nlcd(
        data = nlcd_at_bufs,
        mode = mode,
        locs_id = locs_id
      )
      if (ncol(nlcd_at_bufs_fill) >= 2) {
        nlcd_at_bufs_fill <- nlcd_at_bufs_fill[, -seq_len(2), drop = FALSE]
      } else {
        nlcd_at_bufs_fill <- data.frame(row.names = seq_len(nrow(locs_df)))
      }
      if (ncol(nlcd_at_bufs_fill) > 0) {
        nlcd_cellcnt <- nlcd_at_bufs_fill[,
          seq_len(ncol(nlcd_at_bufs_fill)),
          drop = FALSE
        ]
        nlcd_denom <- rowSums(nlcd_cellcnt, na.rm = TRUE)
        nlcd_denom[!is.finite(nlcd_denom) | nlcd_denom == 0] <- 1
        nlcd_cellcnt <- nlcd_cellcnt / nlcd_denom
        nlcd_cellcnt[!is.finite(as.matrix(nlcd_cellcnt))] <- 0
        nlcd_at_bufs_fill[, seq_len(ncol(nlcd_at_bufs_fill))] <- nlcd_cellcnt
      }
    } else {
      # class_query <- "value"
      # ratio of each nlcd class per buffer
      bufs_polx <- bufs_pol[terra::ext(from), ] |>
        sf::st_as_sf()

      if (nrow(bufs_polx) == 0) {
        nlcd_at_bufs_fill <- data.frame(
          setNames(list(locs_df[[locs_id]]), locs_id)
        )
      } else {
        nlcd_at_bufs <- Map(
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
          },
          seq_len(nrow(bufs_polx))
        )
        nlcd_at_bufs_fill <- amadeus::collapse_nlcd(
          data = nlcd_at_bufs,
          mode = mode,
          locs = bufs_pol,
          locs_id = locs_id
        )
      }
    }

    if (mode == "exact") {
      nlcd_at_buf_names <- names(nlcd_at_bufs_fill)
      nlcd_val_cols <- grep("^frac_", nlcd_at_buf_names, value = TRUE)
      if (length(nlcd_val_cols) == 0) {
        nlcd_at_bufs_fill <- nlcd_at_bufs_fill[, locs_id, drop = FALSE]
      } else {
        nlcd_at_bufs_fill <- nlcd_at_bufs_fill[,
          c(locs_id, nlcd_val_cols),
          drop = FALSE
        ]
      }
      new_data_core <- merge(
        x = locs_df,
        y = nlcd_at_bufs_fill,
        by = locs_id,
        all.x = TRUE,
        sort = FALSE
      )
    } else {
      new_data_core <- cbind(locs_df, nlcd_at_bufs_fill)
    }

    value_cols <- setdiff(names(new_data_core), c(locs_id, "geometry"))
    if (length(value_cols) > 0) {
      new_data_core[, value_cols] <- lapply(
        new_data_core[, value_cols, drop = FALSE],
        function(x) {
          x[is.na(x)] <- 0
          x
        }
      )
      nlcd_codes <- vapply(
        value_cols,
        function(x) {
          x <- sub("^frac_", "", x)
          x <- sub("^X", "", x)
          x <- regmatches(x, regexpr("[0-9]+", x))
          ifelse(length(x) == 0 || !nzchar(x), "UNK", x)
        },
        character(1)
      )
      if (class_names == "mrlc") {
        nlcd_codes <- format_nlcd_mrlc_classes(
          nlcd_codes,
          product = nlcd_product
        )
      }
      names(new_data_core)[match(value_cols, names(new_data_core))] <- sprintf(
        "NLCD_%s_%05d",
        nlcd_codes,
        as.integer(radius)
      )
    }

    if ("geometry" %in% names(new_data_core)) {
      new_data_vect <- cbind(
        new_data_core[, c(locs_id, "geometry"), drop = FALSE],
        as.integer(year),
        new_data_core[,
          setdiff(names(new_data_core), c(locs_id, "geometry")),
          drop = FALSE
        ]
      )
    } else {
      new_data_vect <- cbind(
        new_data_core[, locs_id, drop = FALSE],
        as.integer(year),
        new_data_core[, setdiff(names(new_data_core), locs_id), drop = FALSE]
      )
    }
  }

  if (geom %in% c("sf", "terra")) {
    names(new_data_vect)[1:3] <- c(locs_id, "geometry", "time")
  } else {
    names(new_data_vect)[1:2] <- c(locs_id, "time")
  }
  if (drop && radius > 0) {
    fixed_cols <- c(locs_id, "time")
    if ("geometry" %in% names(new_data_vect)) {
      fixed_cols <- c(locs_id, "geometry", "time")
    }
    nlcd_cols <- grep(
      "^NLCD_.+_[0-9]{5}$",
      names(new_data_vect),
      value = TRUE
    )
    if (length(nlcd_cols) > 0) {
      keep_cols <- nlcd_cols[vapply(
        nlcd_cols,
        function(x) any(new_data_vect[[x]] > 0, na.rm = TRUE),
        logical(1)
      )]
      new_data_vect <- new_data_vect[, c(fixed_cols, keep_cols), drop = FALSE]
    }
  }
  new_data_vect$time <- as.integer(new_data_vect$time)
  new_data_return <- amadeus::calc_return_locs(
    covar = new_data_vect,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(new_data_return)
}

################################################################################
#' Collapse listed NLCD values while filling in NA for sites outside data.
#' @param data Buffered values from NLCD data.
#' @param mode "exact" or "terra"
#' @param locs extraction locations.
#' @param locs_id character(1). Name of unique identifier.
#' @keywords internal auxiliary
#' @importFrom collapse rowbind
#' @export
collapse_nlcd <- function(
  data,
  mode = c("terra", "exact"),
  locs = NULL,
  locs_id = "site_id"
) {
  # Add mode matching
  mode <- match.arg(mode)

  # Filter NULL values
  data_nonnull <- Filter(Negate(is.null), data)

  # Check for empty data
  if (length(data_nonnull) == 0) {
    warning("No non-null data provided to collapse_nlcd")
    return(data.frame())
  }

  # Combine data
  data_rbind <- collapse::rowbind(data_nonnull, fill = TRUE)

  # Check if result is empty
  if (is.null(data_rbind) || nrow(data_rbind) == 0) {
    warning("Row binding resulted in empty data frame")
    return(data.frame())
  }

  if (mode == "terra") {
    # Now safe to create NA row
    na_row <- data_rbind[1, , drop = FALSE]
    na_row[] <- NA

    # Replace all NULL elements with the NA row
    data_na <- lapply(data, function(x) if (is.null(x)) na_row else x)

    # Combine into a single data frame
    data_filled <- collapse::rowbind(data_na, fill = TRUE)
  } else {
    stopifnot(!is.null(locs))

    sites_wdata <- unlist(lapply(data, function(x) x[[locs_id]]))
    sites_missing <- setdiff(unlist(locs[[locs_id]]), sites_wdata)

    df_missing <- data.frame(setNames(list(sites_missing), locs_id))

    data_filled <- collapse::rowbind(data_rbind, df_missing, fill = TRUE)
  }

  return(data_filled)
}

################################################################################
#' Format NLCD class codes using MRLC class names
#' @param codes character. NLCD class codes. Four-digit Land Cover Change
#'   values are interpreted as concatenated from/to class codes.
#' @param product character(1). Annual NLCD product code.
#' @return A character vector of filesystem-safe MRLC class names.
#' @noRd
format_nlcd_mrlc_classes <- function(codes, product = "LndCov") {
  lookup_file <- switch(
    product,
    "ImpDsc" = "Impervious_Descriptor.csv",
    "nlcd_classes.csv"
  )
  nlcd_classes <- utils::read.csv(
    system.file("extdata", lookup_file, package = "amadeus"),
    stringsAsFactors = FALSE
  )
  class_lookup <- stats::setNames(nlcd_classes$names, nlcd_classes$value)

  formatted <- vapply(
    codes,
    function(code) {
      if (grepl("^[0-9]{4}$", code)) {
        from_name <- unname(class_lookup[substr(code, 1L, 2L)])
        to_name <- unname(class_lookup[substr(code, 3L, 4L)])
        if (!is.na(from_name) && !is.na(to_name)) {
          return(paste(from_name, "to", to_name, sep = "_"))
        }
      }

      class_name <- unname(class_lookup[code])
      if (is.na(class_name)) {
        paste0("Unknown_", code)
      } else {
        class_name
      }
    },
    character(1)
  )

  formatted <- gsub("[^[:alnum:]]+", "_", formatted)
  unname(gsub("(^_+|_+$)", "", formatted))
}

################################################################################
#' Identify an Annual NLCD product
#' @param from SpatRaster(1). An Annual NLCD raster.
#' @return A character(1) Annual NLCD product code.
#' @noRd
get_nlcd_product <- function(from) {
  product_codes <- c(
    "LndCov",
    "LndChg",
    "LndCnf",
    "FctImp",
    "ImpDsc",
    "SpcChg"
  )
  metadata <- terra::metags(from)
  product_index <- which(tolower(metadata$name) == "product")
  if (length(product_index) > 0L) {
    product <- metadata$value[product_index[1]]
    product_match <- match(tolower(product), tolower(product_codes))
    if (!is.na(product_match)) {
      return(product_codes[product_match])
    }
  }

  identifiers <- names(from)
  raster_sources <- tryCatch(
    terra::sources(from),
    error = function(e) character()
  )
  identifiers <- c(identifiers, as.character(raster_sources))
  matches <- unique(unlist(lapply(
    identifiers,
    function(x) {
      product <- regmatches(
        x,
        regexpr(
          paste(product_codes, collapse = "|"),
          x,
          ignore.case = TRUE
        )
      )
      product_codes[match(tolower(product), tolower(product_codes))]
    }
  )))
  matches <- matches[!is.na(matches)]
  if (length(matches) == 1L) {
    return(matches)
  }

  "LndCov"
}
################################################################################
