################################################################################
# {amadeus} functions for Emissions Database for Global Atmospheric Research
# (EDGAR).
################################################################################

################################################################################
# {download} tier
################################################################################
# 2. Emissions Database for Global Atmospheric Research (EDGAR)
# nolint start
#' Download EDGAR Emissions Data
#'
#' Constructs and optionally downloads EDGAR emissions data URLs based on
#' user-specified inputs including species, temporal resolution, emission
#' sectors, and file formats.
#'
#' @param species Character vector. One or more species to download.
#'  Supported values: "BC", "CO", "NH3", "NMVOC", "NOx", "OC", "PM10",
#'  "PM2.5", "SO2". Input is case-insensitive and supports "pm2.5" or "pm25".
#' @param version Character. EDGAR data version. Supported values: "8.1" for
#'  most recent version data or "8.1_voc" for VOC speciation data.
#' @param temp_res Character. Temporal resolution for specification with
#'  version 8.1. One of "yearly", "monthly", or "timeseries". temp_res is not
#'  needed for version=8.1_voc and will be ignored if specified.
#' @param sector_yearly Character vector or NULL. Emission sectors for yearly
#'  data. If NULL, totals will be used. Possible values include:
#'  "AGS", "AWB", "CHE", "ENE", "IND", "MNM", "NMM", "PRU_SOL", "RCO",
#'  "REF_TRF", "SWD_INC", "SWD_LDF", "TNR_Aviation_CDS", "TNR_Aviation_CRS",
#'  "TNR_Aviation_LTO", "TNR_Aviation_SPS", "TNR_Other",
#'  "TNR_Ship", "TRO", "WWT"
#' @param sector_monthly Character vector or NULL. Emission sectors for monthly
#'  data. If NULL, the function will use full-species files
#'  (not sector-specific).
#'  Supported values: "AGRICULTURE", "BUILDINGS", "FUEL_EXPLOITATION",
#' "IND_COMBUSTION", "IND_PROCESSES", "POWER_INDUSTRY", "TRANSPORT", "WASTE".
#' @param sector_voc Character vector or NULL. Emission sectors for VOC
#'   speciation
#'  data. If NULL, the function will use full-species files
#'  (not sector-specific).
#'  Supported values: "AGRICULTURE", "BUILDINGS", "FUEL_EXPLOITATION",
#' "IND_COMBUSTION", "IND_PROCESSES", "POWER_INDUSTRY", "TRANSPORT", "WASTE".
#' @param output Character. Output type. Supported values include "emi" for
#'  emissions and "flx" for fluxes.
#' @param format Character. File format to download. Typically "nc" (NetCDF)
#'  or "txt". Flux output and monthly outputs are only supported in .nc format
#' @param year_range Numeric vector of length 1, 2 or NULL. Year range, e.g.,
#' 2021, or c(2021, 2022). If NULL, uses all available years (1970-2022 for
#' yearly data, 2000-2022 for monthly and VOC speciation data)
#' @param voc Integer vector or NULL. Used for VOC speciation in version
#'  "8.1_voc". Accepts integers from 1 to 25. See:
#'  https://edgar.jrc.ec.europa.eu/dataset_ap81_VOC_spec#p3  for reference on
#'  speciation groups and VOC numbers.
#' @return A list of download URLs (character). Optionally downloads available
#'  files and warns about missing ones.
#' @param directory_to_save character(1). Directory to save data. Two
#'  sub-directories will be created for the downloaded zip files ("/zip_files")
#'  and the unzipped data files ("/data_files").
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#'  user acknowledges that the data downloaded using this function may be very
#'  large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#'  containing all download commands. By setting \code{TRUE} the function
#'  will download all of the requested data files.
#' @param remove_command logical(1).
#'  Remove (\code{TRUE}) or keep (\code{FALSE})
#'  the text file containing download commands. Default is FALSE.
#' @param unzip logical(1). Unzip zip files. Default is \code{TRUE}.
#' @param remove_zip logical(1). Remove zip file from directory_to_download.
#'  Default is \code{FALSE}.
#' @param hash logical(1). By setting \code{TRUE} the function will return
#'  an \code{rlang::hash_file()} hash character corresponding to the
#'  downloaded files. Default is \code{FALSE}.
#' @param show_progress logical(1). Show download progress.
#'   Default is \code{TRUE}.
#' @param max_tries integer(1). Maximum download retry attempts.
#'   Default is \code{20}.
#' @param rate_limit numeric(1). Minimum seconds between requests.
#'   Default is \code{2}.
#' @author Mariana Alifa Kassien
#' @return
#' * For \code{hash = FALSE}, NULL
#' * For \code{hash = TRUE}, an \code{rlang::hash_file} character.
#' * Zip and/or data files will be downloaded and stored in
#' \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#'
# nolint end
#' @examples
#' \dontrun{
#' download_edgar(
#' species = "CO",
#' acknowledgement = TRUE,
#' temp_res = "yearly",
#' sector_yearly = "ENE",
#' year_range = c(2021, 2022)
#' )
#' }
#' \dontrun{
#' download_edgar(
#' species = "PM2.5",
#' acknowledgement = TRUE,
#' temp_res = "monthly",
#' sector_monthly = c("TRANSPORT", "WASTE")
#' )
#' }
#' \dontrun{
#' download_edgar(
#' species = "SO2",
#' acknowledgement = TRUE,
#' temp_res = "timeseries"
#' )
#' }
#' @export
download_edgar <- function(
  species = c("BC", "CO", "NH3", "NMVOC", "NOx", "OC", "PM10", "PM2.5", "SO2"),
  version = "8.1",
  temp_res = NULL,
  sector_yearly = NULL,
  sector_monthly = NULL,
  sector_voc = NULL,
  format = "nc",
  output = "emi",
  year_range = NULL,
  voc = NULL,
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
) {
  # check for data download acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  # Handle deprecated parameters
  if (!isTRUE(download)) {
    warning(
      "Setting download=FALSE is deprecated.",
      " Downloads now use httr2 by default.\n",
      "To skip downloading, the function will return",
      " after discovering files.\n",
      call. = FALSE
    )
  }
  if (!isFALSE(remove_command)) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  # directory setup
  directory_original <- amadeus::download_sanitize_path(directory_to_save)
  directories <- amadeus::download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]

  durl <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/"

  # Normalize species input
  species <- toupper(species)
  species <- gsub("NOX", "NOx", species)

  # Check for invalid combinations
  if (any(output == "flx" & format == "txt")) {
    stop("Output 'flux' is only supported for format 'nc'.")
  }
  if (any(temp_res == "monthly" & format == "txt")) {
    stop("Monthly resolution is only supported for format 'nc'.")
  }

  if (!is.null(year_range)) {
    if (length(year_range) == 1) {
      yearsvec <- year_range
    } else if (length(year_range) == 2) {
      yearsvec <- seq(year_range[1], year_range[2])
    } else {
      stop("year_range must be of length 1 or 2")
    }
  }

  urls <- character()

  if (version == "8.1") {
    vers <- "v81_FT2022_AP_new/"
    vers_file <- "v8.1_FT2022_AP"

    grid_species <- data.frame(species = species, stringsAsFactors = FALSE)
    grid_species$folder <- gsub(
      "(?i)pm2\\.5|pm25",
      "PM2.5",
      grid_species$species,
      perl = TRUE
    )
    grid_species$file <- gsub(
      "(?i)pm2\\.5|pm25",
      "PM25",
      grid_species$species,
      perl = TRUE
    )

    if (temp_res == "timeseries") {
      urls <- paste0(durl, vers, "EDGAR_", grid_species$file, "_1970_2022.zip")
      urls <- ifelse(
        grid_species$file == "SO2",
        gsub("2022.zip$", "2022_v2.zip", urls),
        urls
      )
    } else if (temp_res == "yearly") {
      if (!is.null(sector_yearly)) {
        grid <- expand.grid(
          species_idx = seq_len(nrow(grid_species)),
          sector = sector_yearly,
          year = if (!is.null(year_range)) yearsvec else NA,
          stringsAsFactors = FALSE
        )

        urls <- if (!is.null(year_range)) {
          paste0(
            durl,
            vers,
            grid_species$folder[grid$species_idx],
            "/",
            grid$sector,
            "/",
            output,
            "_",
            format,
            "/",
            vers_file,
            "_",
            grid_species$species[grid$species_idx],
            "_",
            grid$year,
            "_",
            grid$sector,
            "_",
            output,
            "_",
            format,
            ".zip"
          )
        } else {
          paste0(
            durl,
            vers,
            grid_species$folder[grid$species_idx],
            "/",
            grid$sector,
            "/",
            grid$sector,
            "_",
            output,
            "_",
            format,
            ".zip"
          )
        }
      } else {
        grid <- expand.grid(
          species_idx = seq_len(nrow(grid_species)),
          year = if (!is.null(year_range)) yearsvec else NA,
          stringsAsFactors = FALSE
        )

        urls <- if (!is.null(year_range)) {
          paste0(
            durl,
            vers,
            grid_species$folder[grid$species_idx],
            "/TOTALS/",
            output,
            "_",
            format,
            "/",
            vers_file,
            "_",
            grid_species$species[grid$species_idx],
            "_",
            grid$year,
            "_TOTALS_",
            output,
            "_",
            format,
            ".zip"
          )
        } else {
          paste0(
            durl,
            vers,
            grid_species$folder,
            "/TOTALS/TOTALS_",
            output,
            "_",
            format,
            ".zip"
          )
        }
      }
    } else if (temp_res == "monthly") {
      if (!is.null(sector_monthly)) {
        grid <- expand.grid(
          species_idx = seq_len(nrow(grid_species)),
          sector = sector_monthly,
          stringsAsFactors = FALSE
        )
        urls <- paste0(
          durl,
          vers,
          "monthly/",
          grid_species$folder[grid$species_idx],
          "/bkl_",
          grid$sector,
          "/bkl_",
          grid$sector,
          "_",
          output,
          "_",
          format,
          ".zip"
        )
      } else {
        urls <- paste0(
          durl,
          vers,
          "monthly/EDGAR_",
          grid_species$file,
          "_m_2000_2022.zip"
        )
        urls <- ifelse(
          grid_species$file == "SO2",
          gsub("2022.zip$", "2022_v2.zip", urls),
          urls
        )
      }
    } else {
      stop("Unsupported temp_res value")
    }
  } else if (version == "8.1_voc") {
    vers <- "v81_FT2022_VOC_spec/"
    vers_file <- "v8.1_FT2022_VOC_spec"

    if (!is.null(sector_voc)) {
      grid <- expand.grid(
        voc = voc,
        sector = sector_voc,
        year = if (!is.null(year_range)) yearsvec else NA,
        stringsAsFactors = FALSE
      )

      urls <- if (!is.null(year_range)) {
        paste0(
          durl,
          vers,
          "voc",
          grid$voc,
          "/bkl_",
          grid$sector,
          "/",
          output,
          "_",
          format,
          "/",
          vers_file,
          "_voc",
          grid$voc,
          "_",
          grid$year,
          "_bkl_",
          grid$sector,
          "_",
          output,
          "_",
          format,
          ".zip"
        )
      } else {
        paste0(
          durl,
          vers,
          "voc",
          voc,
          "/bkl_",
          sector_voc,
          "/bkl_",
          sector_voc,
          "_",
          output,
          "_",
          format,
          ".zip"
        )
      }
    } else {
      urls <- paste0(durl, vers, "EDGAR_voc", voc, "_1970_2022.zip")
    }
  } else {
    stop("Unsupported version")
  }
  # Check constructed urls
  message("Constructed URL(s): ", paste(urls, collapse = "\n"))

  #### 5. build download file name
  download_label <- temp_res
  if (is.null(download_label) || length(download_label) == 0) {
    download_label <- if (version == "8.1_voc") "voc" else "edgar"
  }
  download_names <- paste0(
    directory_to_download,
    "edgar_",
    download_label,
    "_",
    basename(urls)
  )

  #### Exit early if download=FALSE (return unvalidated URL list)
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available for download.\n",
      length(urls)
    ))
    return(invisible(list(
      urls = urls,
      destfiles = download_names,
      n_files = length(urls)
    )))
  }

  # Validate and download
  download_urls <- c()
  download_names_valid <- c()
  missing_urls <- c()

  for (i in seq_along(urls)) {
    u <- urls[i]
    if (!(amadeus::check_url_status(u))) {
      missing_urls <- c(missing_urls, u)
    } else {
      download_urls <- c(download_urls, u)
      download_names_valid <- c(download_names_valid, download_names[i])
    }
  }
  # Stop function if no valid urls were created
  if (is.null(download_urls) || length(download_urls) == 0) {
    stop("No valid URLs were constructed.")
  }
  # Issue warning message for URLs not found
  if (length(missing_urls)) {
    warning(
      "Some URLs could not be accessed:",
      paste(missing_urls, collapse = "\n")
    )
  }

  #### Filter to files that need downloading
  needs_download <- !file.exists(download_names_valid) |
    file.size(download_names_valid) == 0
  download_urls_dl <- download_urls[needs_download]
  download_names_dl <- download_names_valid[needs_download]

  #### Download using httr2
  if (length(download_urls_dl) > 0) {
    amadeus::download_run_method(
      urls = download_urls_dl,
      destfiles = download_names_dl,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  }

  #### Unzip data
  sapply(
    download_names_valid,
    amadeus::download_unzip,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = download_names_valid
  )
  return(amadeus::download_hash(hash, directory_to_save))
}

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process EDGAR emissions data
#' @description
#' The \code{process_edgar()} function imports extracted EDGAR gridded emissions
#' files and returns a single `SpatRaster` object. Raster formats supported by
#' `terra::rast()` such as NetCDF (`.nc`, `.nc4`) and GeoTIFF (`.tif`,
#' `.tiff`) are supported.
#' @param path character. Directory containing extracted EDGAR raster files or
#'   one or more file paths.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster;
#'   if `NULL` (default), the entire raster is loaded.
#' @param ... Placeholders.
#' @note
#' `process_edgar()` currently supports gridded raster outputs from
#' `download_edgar()` such as the default `format = "nc"`. Plain-text EDGAR
#' downloads should be re-downloaded as raster outputs before processing.
#' @return a `SpatRaster` object
#' @author Mariana Alifa Kassien, Insang Song
#' @seealso [`download_edgar()`], [`calculate_edgar()`]
#' @importFrom terra rast nlyr time
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires data that is
#' ##       not included in the package.
#' \dontrun{
#' edgar <- process_edgar(
#'   path = "./data/edgar",
#'   extent = c(-85, -75, 33, 37)
#' )
#' }
#' @export
# nolint end
process_edgar <- function(
  path = NULL,
  extent = NULL,
  ...
) {
  amadeus::check_for_null_parameters(mget(ls()))

  if (length(path) == 1 && dir.exists(path)) {
    path <- list.files(
      path = amadeus::download_sanitize_path(path),
      recursive = TRUE,
      full.names = TRUE
    )
  }

  if (length(path) == 0) {
    stop("path does not contain files.")
  }

  raster_paths <- grep(
    "\\.(nc4?|tif|tiff|grd|img)$",
    path,
    ignore.case = TRUE,
    value = TRUE
  )

  if (length(raster_paths) == 0) {
    txt_paths <- grep("\\.txt$", path, ignore.case = TRUE, value = TRUE)
    if (length(txt_paths) > 0) {
      stop(
        "process_edgar() currently supports gridded raster files only. ",
        "Re-download EDGAR with format = \"nc\" or provide extracted ",
        "raster files.\n"
      )
    }
    stop("path does not contain supported EDGAR raster files.\n")
  }

  clean_name <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x
  }

  edgar_rasters <- lapply(
    raster_paths,
    function(pth) {
      data <- terra::rast(pth, win = extent)
      base_name <- clean_name(tools::file_path_sans_ext(basename(pth)))
      layer_names <- clean_name(names(data))

      if (
        terra::nlyr(data) == 1 ||
          any(layer_names == "") ||
          all(grepl("^lyr_?[0-9]+$", layer_names))
      ) {
        names(data) <- sprintf(
          "edgar_%s_%03d",
          base_name,
          seq_len(terra::nlyr(data))
        )
        if (terra::nlyr(data) == 1) {
          names(data) <- paste0("edgar_", base_name)
        }
      } else {
        names(data) <- paste0("edgar_", layer_names)
      }

      if (terra::nlyr(data) == 1) {
        year_gregexpr <- gregexpr(
          "(?<!\\d)\\d{4}(?!\\d)",
          basename(pth),
          perl = TRUE
        )[[1]]
        if (year_gregexpr[1] != -1) {
          year_match <- regmatches(
            basename(pth),
            gregexpr("(?<!\\d)\\d{4}(?!\\d)", basename(pth), perl = TRUE)
          )[[1]][1]
          terra::time(data) <- as.Date(sprintf("%s-01-01", year_match))
        }
      }

      data
    }
  )

  edgar <- do.call(c, c(edgar_rasters, warn = FALSE))
  names(edgar) <- make.unique(names(edgar), sep = "_")
  return(edgar)
}

################################################################################
# {calculate} tier
################################################################################
#' Calculate EDGAR covariates
#' @description
#' Extract EDGAR gridded emissions values at point locations. For
#' `radius = 0`, cell values are extracted directly. For `radius > 0`,
#' means are calculated over a circular buffer around each location.
#' @param from SpatRaster(1). Output from \code{process_edgar()}.
#' @param locs data.frame, character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file containing
#'   identifier for each unique coordinate location.
#' @param radius numeric(1). Circular buffer distance around site locations.
#'   Default is `0`.
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Mariana Alifa Kassien, Insang Song
#' @seealso [`process_edgar()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra extract crs
#' @importFrom sf st_as_sf st_buffer
#' @importFrom exactextractr exact_extract
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires data that is
#' ##       not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_edgar(
#'   from = edgar, # derived from process_edgar() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   geom = FALSE
#' )
#' }
#' @export
calculate_edgar <- function(
  from,
  locs,
  locs_id = "site_id",
  radius = 0,
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  if (!inherits(from, "SpatRaster")) {
    stop("`from` must be a SpatRaster object.")
  }
  if (!is.numeric(radius) || length(radius) != 1) {
    stop("`radius` must be numeric(1).")
  }

  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]

  is_polygon_locs <- inherits(sites_e, "SpatVector") &&
    !all(tolower(terra::geomtype(sites_e)) %in% c("points", "point"))
  weights_prepared <- amadeus::calc_prepare_weights(
    from = from[[1]],
    weights = weights
  )
  fun_extract <- amadeus::calc_weighted_fun(
    fun = "mean",
    weighted = !is.null(weights_prepared)
  )
  if (radius == 0 && !is_polygon_locs && is.null(weights_prepared)) {
    sites_extracted <- terra::extract(from, sites_e)
    sites_extracted <- sites_extracted[, -1, drop = FALSE]
  } else {
    if (inherits(sites_e, "SpatVector")) {
      sites_e <- sf::st_as_sf(sites_e)
    }
    extract_args <- c(
      list(
        x = from,
        y = sites_e,
        fun = fun_extract,
        force_df = TRUE,
        progress = FALSE
      ),
      list(...)
    )
    if (!is.null(weights_prepared)) {
      extract_args$weights <- weights_prepared
    }
    sites_extracted <- do.call(exactextractr::exact_extract, extract_args)
    exact_names <- names(sites_extracted)
    if (length(exact_names) == 1 && identical(exact_names, "mean")) {
      exact_names <- names(from)[1]
    } else {
      exact_names <- gsub("^mean\\.", "", exact_names)
    }
    names(sites_extracted) <- exact_names
  }

  names(sites_extracted) <- sprintf("%s_%d", names(sites_extracted), radius)
  sites_extracted[[locs_id]] <- sites_id[, 1]
  if (
    "geometry" %in% names(sites_id) && !"geometry" %in% names(sites_extracted)
  ) {
    sites_extracted$geometry <- sites_id$geometry
  }
  ordered_cols <- c(
    locs_id,
    if ("geometry" %in% names(sites_extracted)) "geometry",
    setdiff(names(sites_extracted), c(locs_id, "geometry"))
  )
  sites_extracted <- sites_extracted[, ordered_cols]

  posixt_out <- FALSE
  if (!is.null(.by_time)) {
    if (!"time" %in% names(sites_extracted)) {
      value_cols_now <- setdiff(names(sites_extracted), c(locs_id, "geometry"))
      if (length(value_cols_now) != 1L) {
        stop(
          "EDGAR `.by_time` summarization requires a single covariate column ",
          "or an existing `time` column.\n"
        )
      }
      edgar_time <- NA
      time_vals <- try(terra::time(from), silent = TRUE)
      if (!inherits(time_vals, "try-error") && length(time_vals) >= 1L) {
        edgar_time <- time_vals[1]
      }
      if (is.na(edgar_time)) {
        stop(
          "Could not derive EDGAR time for `.by_time` summarization. ",
          "Provide data with explicit time in layer metadata.\n"
        )
      }
      sites_extracted$time <- edgar_time
    }
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id
    )
    if ("time" %in% names(sites_extracted)) {
      sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
      posixt_out <- TRUE
    }
  }

  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = posixt_out,
    geom = geom,
    crs = terra::crs(from)
  )
  return(sites_return)
}
################################################################################
