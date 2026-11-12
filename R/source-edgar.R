################################################################################
# {amadeus} functions for Emissions Database for Global Atmospheric Research
# (EDGAR).
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download EDGAR emissions data
#' @description
#' Construct EDGAR v8.1 air-pollutant or VOC-speciation archive URLs and
#' download them using the shared validation, preview, and download helpers.
#' @param species character vector. Air pollutants to download for version
#'   `"8.1"`: `"BC"`, `"CO"`, `"NH3"`, `"NMVOC"`, `"NOx"`, `"OC"`,
#'   `"PM10"`, `"PM2.5"`, or `"SO2"`. Defaults to all listed species.
#'   Input is case-insensitive; `"PM25"` is also accepted. Ignored for VOC
#'   speciation, which uses `voc` instead.
#' @param version character(1). `"8.1"` (default) for air pollutants or
#'   `"8.1_voc"` for VOC speciation.
#' @param temp_res character(1) or NULL. Required for version `"8.1"`:
#'   `"yearly"`, `"monthly"`, or `"timeseries"`. Timeseries archives can
#'   contain tabular data; see Details for processing limitations.
#'   For version `"8.1_voc"`, this does not affect URL selection; if supplied,
#'   it is retained in the local archive-name prefix instead of `"voc"`.
#' @param sector_yearly character vector or NULL. Yearly sectors: `"AGS"`,
#'   `"AWB"`, `"CHE"`, `"ENE"`, `"IND"`, `"MNM"`, `"NMM"`, `"PRU_SOL"`,
#'   `"RCO"`, `"REF_TRF"`, `"SWD_INC"`, `"SWD_LDF"`, `"TNR_Aviation_CDS"`,
#'   `"TNR_Aviation_CRS"`, `"TNR_Aviation_LTO"`, `"TNR_Aviation_SPS"`,
#'   `"TNR_Other"`, `"TNR_Ship"`, `"TRO"`, `"WWT"`, or `"TOTALS"`.
#'   `NULL` selects totals. Used only for yearly version `"8.1"` data.
#' @param sector_monthly character vector or NULL. Monthly sectors:
#'   `"AGRICULTURE"`, `"BUILDINGS"`, `"FUEL_EXPLOITATION"`,
#'   `"IND_COMBUSTION"`, `"IND_PROCESSES"`, `"POWER_INDUSTRY"`,
#'   `"TRANSPORT"`, or `"WASTE"`. `NULL` selects full-species archives.
#'   Used only for monthly version `"8.1"` data.
#' @param sector_voc character vector or NULL. VOC sectors, using the same
#'   choices as `sector_monthly`. `NULL` selects full VOC-group archives.
#'   Used only for version `"8.1_voc"`.
#' @param format character(1). `"nc"` (default) or `"txt"`. Flux output and
#'   monthly version `"8.1"` data require `"nc"`. Selects an archive template
#'   where supported; it does not convert the downloaded files.
#' @param output character(1). `"emi"` (default) for emissions or `"flx"`
#'   for fluxes. Full-species monthly, timeseries, and full VOC-group archives
#'   use fixed provider filenames that do not vary with `output` or `format`.
#' @param year_range integer(1 or more) or NULL. One year, two endpoints, or
#'   a non-empty vector of years. Every year from the minimum through the
#'   maximum is selected, including gaps: `c(2018, 2023)` and
#'   `c(2018, 2020, 2023)` both select `2018:2023`. Years must be non-missing
#'   integer values from 1900 through the current calendar year. For yearly
#'   version `"8.1"` data or sector-specific VOC data, `NULL` (default)
#'   selects bundled archives instead of individual yearly archives.
#'   Monthly, timeseries, and full VOC-group archives always contain the
#'   provider's bundled years; `year_range` is validated when supplied but
#'   does not change their URLs or subset their contents.
#' @param voc integer vector, character vector, or NULL. VOC group numbers
#'   from 1 to 25, required for version `"8.1_voc"`. Numeric strings such as
#'   `"1"` are accepted. Ignored for version `"8.1"`.
#' @param directory_to_save character(1). Non-empty parent directory.
#'   Archives are saved in `zip_files/` and extracted into `data_files/`.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed. Archives and
#'   extracted data can require substantial storage.
#' @param download logical(1). Deprecated. `FALSE` warns and returns a preview
#'   after directory creation, before preflight checks or file downloads.
#'   Default is `TRUE`.
#' @param remove_command logical(1). Deprecated and ignored; `TRUE` warns.
#'   Default is `FALSE`.
#' @param unzip logical(1). Extract downloaded or existing archives into
#'   `data_files/`. Default is `TRUE`. When `FALSE`, archives remain in
#'   `zip_files/` without extraction.
#' @param remove_zip logical(1). Remove archives and their parent `zip_files/`
#'   directory recursively after the extraction loop finishes, including any
#'   other files in that directory. Requires `unzip = TRUE`. Default is `FALSE`.
#' @param hash logical(1). Return the combined MD5 hash of all files recursively
#'   under `data_files/`, including previously extracted files.
#'   With `unzip = FALSE`, this still hashes `data_files/`, not the archives.
#'   Default is `FALSE`; ignored when `download = FALSE`.
#' @param show_progress logical(1). Show per-file download progress. Default
#'   is `TRUE`; summary messages may still be printed when `FALSE`.
#' @param max_tries integer(1). Maximum attempts per file request, including
#'   the initial attempt. Must be at least 1. Default is 20.
#' @param rate_limit numeric(1). Non-negative request-throttling interval in
#'   seconds, passed to [download_run_method()]. Default is 2.
#' @details
#' Scalar selectors `version`, `format`, and `output` use [base::match.arg()],
#' allowing unambiguous partial matches. This also applies to `temp_res` for
#' version `"8.1"`; for VOC speciation, a supplied `temp_res` is used as-is
#' in the local archive-name prefix. Sector names require exact,
#' case-sensitive matches. Only the sector argument for the selected version
#' and temporal resolution is used; timeseries downloads use no sector argument.
#'
#' URL construction retains the v8.1 FT2022 archive templates, including
#' the PM2.5 folder/filename conventions and the SO2 `_v2` suffix on full
#' monthly and timeseries archives. Monthly full-species filenames cover
#' 2000--2022; timeseries and full VOC-group filenames cover 1970--2022.
#' Available remote files are not discovered by querying directory listings.
#' Year validation checks the input range, not whether EDGAR provides an
#' archive for each requested year or combination of arguments.
#'
#' Timeseries archives are not necessarily raster products. For example,
#' the NOx timeseries archive contains an Excel workbook (`.xlsx`) and a
#' readme, even when `format = "nc"`. [process_edgar()] currently accepts
#' raster files only, so this workbook cannot be passed through the
#' `process_edgar()` and `calculate_edgar()` workflow.
#'
#' Local archive names retain the `edgar_<temp_res>_` prefix, or `edgar_voc_`
#' when VOC speciation is requested with `temp_res = NULL`. Preview results
#' include all constructed URLs and destination paths, including existing
#' files. A preview creates the output directories but makes no network
#' requests and does not verify remote availability. No shell-command file
#' is created.
#'
#' Full downloads check the first URL with the shared preflight helper.
#' A failed preflight emits a warning and downloading is still attempted.
#' [download_run_method()] skips existing non-empty destination files and
#' records individual download failures in its summary. The preflight check
#' uses its own retry settings; file downloads use the shared default timeout.
#' Extraction and cleanup occur before returning, and extraction errors can
#' prevent a normal return. Summary counts describe downloads, not extraction.
#' @return
#' * With `download = FALSE`, an invisible list containing `urls`,
#'   `destfiles`, and `n_files`, the number of planned archive downloads.
#' * With `download = TRUE` and `hash = FALSE`, the invisible summary from
#'   [download_run_method()] with `success`, `failed`, and `skipped` counts.
#'   It always contains `failed_urls` and `failed_files` character vectors,
#'   both `character(0)` when no downloads failed, including when every file
#'   was skipped. These counts describe archives, not extracted files.
#' * With `download = TRUE` and `hash = TRUE`, a visible character(1) combined
#'   MD5 hash of all files under `data_files/`, including existing files.
#' @author Mariana Alifa Kassien
#' @seealso [download_data()], [download_run_method()], [process_edgar()]
#' @importFrom Rdpack reprompt
#' @examples
#' \dontrun{
#' download_edgar(
#'   species = "CO",
#'   temp_res = "yearly",
#'   sector_yearly = "ENE",
#'   year_range = c(2021, 2022),
#'   directory_to_save = file.path(tempdir(), "edgar"),
#'   acknowledgement = TRUE,
#'   download = FALSE
#' )
#' download_edgar(
#'   version = "8.1_voc",
#'   voc = 1:3,
#'   sector_voc = "AGRICULTURE",
#'   year_range = 2021,
#'   directory_to_save = file.path(tempdir(), "edgar_voc"),
#'   acknowledgement = TRUE,
#'   download = FALSE
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
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "edgar",
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
  # Check source-specific scalar arguments.
  chr_version <- match.arg(version, c("8.1", "8.1_voc"))
  chr_format <- match.arg(format, c("nc", "txt"))
  chr_output <- match.arg(output, c("emi", "flx"))
  checkmate::assert_string(temp_res, null.ok = chr_version == "8.1_voc")
  chr_temp_res <- if (chr_version == "8.1") {
    match.arg(temp_res, c("yearly", "monthly", "timeseries"))
  } else {
    temp_res
  }
  # ----------------------------------------------------------------------------
  # Normalize and check the selected species or VOC groups.
  if (chr_version == "8.1") {
    checkmate::assert_character(species, min.len = 1L, any.missing = FALSE)
    chr_species <- toupper(species)
    chr_species[chr_species == "NOX"] <- "NOx"
    checkmate::assert_subset(
      chr_species,
      c("BC", "CO", "NH3", "NMVOC", "NOx", "OC", "PM10", "PM2.5", "PM25", "SO2")
    )
  } else {
    int_voc <- if (is.character(voc)) suppressWarnings(as.numeric(voc)) else voc
    checkmate::assert_integerish(
      int_voc,
      lower = 1L,
      upper = 25L,
      min.len = 1L,
      any.missing = FALSE
    )
    int_voc <- as.integer(int_voc)
  }
  # ----------------------------------------------------------------------------
  # Select and check only the sectors used by this request.
  chr_sectors <- if (chr_version == "8.1_voc") {
    sector_voc
  } else {
    switch(chr_temp_res, yearly = sector_yearly, monthly = sector_monthly)
  }
  checkmate::assert_character(
    chr_sectors,
    min.len = 1L,
    any.missing = FALSE,
    null.ok = TRUE
  )
  if (!is.null(chr_sectors)) {
    chr_sector_choices <- if (
      chr_version == "8.1" && chr_temp_res == "yearly"
    ) {
      c(
        "AGS",
        "AWB",
        "CHE",
        "ENE",
        "IND",
        "MNM",
        "NMM",
        "PRU_SOL",
        "RCO",
        "REF_TRF",
        "SWD_INC",
        "SWD_LDF",
        "TNR_Aviation_CDS",
        "TNR_Aviation_CRS",
        "TNR_Aviation_LTO",
        "TNR_Aviation_SPS",
        "TNR_Other",
        "TNR_Ship",
        "TRO",
        "WWT",
        "TOTALS"
      )
    } else {
      c(
        "AGRICULTURE",
        "BUILDINGS",
        "FUEL_EXPLOITATION",
        "IND_COMBUSTION",
        "IND_PROCESSES",
        "POWER_INDUSTRY",
        "TRANSPORT",
        "WASTE"
      )
    }
    checkmate::assert_subset(chr_sectors, chr_sector_choices)
  }
  # ----------------------------------------------------------------------------
  # Reject unsupported output/format combinations.
  if (chr_output == "flx" && chr_format == "txt") {
    stop("Output 'flux' is only supported for format 'nc'.", call. = FALSE)
  }
  if (
    chr_version == "8.1" && chr_temp_res == "monthly" && chr_format == "txt"
  ) {
    stop("Monthly resolution is only supported for format 'nc'.", call. = FALSE)
  }
  # ----------------------------------------------------------------------------
  # Check years and generate a sequence; NULL retains bundled-archive selection.
  int_years <- if (is.null(year_range)) {
    NULL
  } else {
    amadeus:::sequence_years(year_range)
  }
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus:::download_setup_dir(directory_to_save, zip = TRUE)
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Define provider directories, filename prefixes, and the local archive label.
  chr_base <- paste0(
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/",
    if (chr_version == "8.1") "v81_FT2022_AP_new/" else "v81_FT2022_VOC_spec/"
  )
  chr_version_file <- if (chr_version == "8.1") {
    "v8.1_FT2022_AP"
  } else {
    "v8.1_FT2022_VOC_spec"
  }
  chr_label <- if (is.null(chr_temp_res)) "voc" else chr_temp_res
  # ----------------------------------------------------------------------------
  # Expand request combinations and select the matching archive templates.
  if (chr_version == "8.1" && chr_temp_res == "yearly") {
    # Yearly totals and individual sectors share the same directory pattern.
    df_download <- expand.grid(
      species = chr_species,
      sector = if (is.null(chr_sectors)) "TOTALS" else chr_sectors,
      year = if (is.null(int_years)) NA_integer_ else int_years,
      stringsAsFactors = FALSE
    )
    df_download$base <- paste0(
      chr_base,
      sub("PM25", "PM2.5", df_download$species, fixed = TRUE),
      "/",
      df_download$sector,
      "/"
    )
    if (is.null(int_years)) {
      df_download$file <- paste0(
        df_download$sector,
        "_",
        chr_output,
        "_",
        chr_format,
        ".zip"
      )
    } else {
      df_download$base <- paste0(
        df_download$base,
        chr_output,
        "_",
        chr_format,
        "/"
      )
      df_download$file <- paste0(
        chr_version_file,
        "_",
        df_download$species,
        "_",
        df_download$year,
        "_",
        df_download$sector,
        "_",
        chr_output,
        "_",
        chr_format,
        ".zip"
      )
    }
  } else if (
    chr_version == "8.1" && chr_temp_res == "monthly" && !is.null(chr_sectors)
  ) {
    # Monthly sector archives bundle years; year_range does not change the URLs.
    df_download <- expand.grid(
      species = chr_species,
      sector = chr_sectors,
      stringsAsFactors = FALSE
    )
    df_download$base <- paste0(
      chr_base,
      "monthly/",
      sub("PM25", "PM2.5", df_download$species, fixed = TRUE),
      "/bkl_",
      df_download$sector,
      "/"
    )
    df_download$file <- paste0(
      "bkl_",
      df_download$sector,
      "_",
      chr_output,
      "_",
      chr_format,
      ".zip"
    )
  } else if (chr_version == "8.1") {
    # Full-species monthly and timeseries archives use fixed provider filenames.
    df_download <- expand.grid(species = chr_species, stringsAsFactors = FALSE)
    df_download$base <- paste0(
      chr_base,
      if (chr_temp_res == "monthly") "monthly/" else ""
    )
    df_download$file <- paste0(
      "EDGAR_",
      sub("PM2.5", "PM25", df_download$species, fixed = TRUE),
      if (chr_temp_res == "monthly") "_m_2000_2022" else "_1970_2022",
      ifelse(df_download$species == "SO2", "_v2", ""),
      ".zip"
    )
  } else if (!is.null(chr_sectors)) {
    # Expand every VOC/sector/year combination, including bundled sector files.
    df_download <- expand.grid(
      voc = int_voc,
      sector = chr_sectors,
      year = if (is.null(int_years)) NA_integer_ else int_years,
      stringsAsFactors = FALSE
    )
    df_download$base <- paste0(
      chr_base,
      "voc",
      df_download$voc,
      "/bkl_",
      df_download$sector,
      "/"
    )
    if (is.null(int_years)) {
      df_download$file <- paste0(
        "bkl_",
        df_download$sector,
        "_",
        chr_output,
        "_",
        chr_format,
        ".zip"
      )
    } else {
      df_download$base <- paste0(
        df_download$base,
        chr_output,
        "_",
        chr_format,
        "/"
      )
      df_download$file <- paste0(
        chr_version_file,
        "_voc",
        df_download$voc,
        "_",
        df_download$year,
        "_bkl_",
        df_download$sector,
        "_",
        chr_output,
        "_",
        chr_format,
        ".zip"
      )
    }
  } else {
    # Full VOC-group archives already contain the complete provider year range.
    df_download <- expand.grid(voc = int_voc, stringsAsFactors = FALSE)
    df_download$base <- chr_base
    df_download$file <- paste0("EDGAR_voc", df_download$voc, "_1970_2022.zip")
  }
  # ----------------------------------------------------------------------------
  # Generate download URLs and local archive filenames from the same rows.
  df_download$url <- paste0(df_download$base, df_download$file)
  df_download$destfile <- paste0(
    chr_dir_to_download,
    "edgar_",
    chr_label,
    "_",
    df_download$file
  )
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus:::download_preview(
      df_download$url,
      df_download$destfile
    )))
  }
  # ----------------------------------------------------------------------------
  # Check first URL for validity.
  amadeus:::check_preflight(df_download$url[1])
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus:::download_run_method(
    urls = df_download$url,
    destfiles = df_download$destfile,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Unzip downloaded or existing archives.
  amadeus:::download_unzip(
    file_name = df_download$destfile,
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
  amadeus:::check_for_null_parameters(mget(ls()))

  if (length(path) == 1 && dir.exists(path)) {
    path <- list.files(
      path = amadeus:::download_sanitize_path(path),
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
