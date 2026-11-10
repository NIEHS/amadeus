################################################################################
# {amadeus} functions for NOAA Hazard Mapping System (HMS).
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download wildfire smoke data
#' @description
#' The \code{download_hms()} function accesses and downloads
#' wildfire smoke plume coverage data from NOAA's Hazard Mapping System Fire
#' and Smoke Product.
#' @note HMS data does not require authentication.
#' @param data_format character(1). "Shapefile" or "KML".
#' @param date character(1 or 2). Date range "YYYY-MM-DD" format
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param unzip logical(1). Unzip zip files (default TRUE).
#' @param remove_zip logical(1). Remove zip files after unzipping (default
#' FALSE).
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song
#' @return With `download = FALSE`, an invisible preview list containing
#' `urls`, `destfiles`, and `n_files`. With `download = TRUE` and
#' `hash = FALSE`, an invisible summary from [download_run_method()] containing
#' `success`, `failed`, `skipped`, `failed_urls`, and `failed_files`. The two
#' failure vectors are `character(0)` when no downloads failed, including when
#' every file was skipped. With `download = TRUE` and `hash = TRUE`, a visible
#' character scalar containing the combined MD5 hash of the output directory.
#' @importFrom utils head tail
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_HMSabout}{amadeus}
#' @examples
#' \dontrun{
#' download_hms(
#'   data_format = "Shapefile",
#'   date = "2024-01-01",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_hms <- function(
  data_format = "Shapefile",
  date = c("2018-01-01", "2018-01-01"),
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
    dataset_name = "hms",
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
  chr_format <- tolower(data_format)
  checkmate::assert_choice(chr_format, c("shapefile", "kml"))
  # ----------------------------------------------------------------------------
  # Check `date` and generate sequence.
  chr_dates <- amadeus:::sequence_dates(date, hyphen = FALSE)
  if (as.Date(chr_dates[1], format = "%Y%m%d") < as.Date("2005-08-05")) {
    stop("NOAA HMS wildfire smoke data begins at August 05, 2005.")
  }
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus:::download_setup_dir(directory_to_save, zip = TRUE)
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Define file suffix and download directory.
  if (chr_format == "shapefile") {
    chr_format <- "Shapefile"
    chr_suffix <- ".zip"
  } else {
    chr_format <- "KML"
    chr_suffix <- ".kml"
    chr_dir_to_download <- chr_dir_to_save
  }
  # ----------------------------------------------------------------------------
  # Generate download URLs.
  chr_urls <- sprintf(
    paste0(
      "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/",
      chr_format,
      "/%s/%s/hms_smoke%s",
      chr_suffix
    ),
    substr(chr_dates, 1, 4),
    substr(chr_dates, 5, 6),
    chr_dates
  )
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filenames <- sprintf(
    paste0(
      chr_dir_to_download,
      "hms_smoke_",
      chr_format,
      "_%s",
      chr_suffix
    ),
    chr_dates
  )
  # ----------------------------------------------------------------------------
  # Check first URL for validity.
  amadeus:::check_preflight(chr_urls[1])
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
  # `unzip` and `remove` parameters if data_format = "Shapefile".
  if (chr_format == "Shapefile") {
    vunzip <- Vectorize(amadeus:::download_unzip)
    vunzip(
      file_name = chr_filenames,
      directory_to_unzip = chr_dir_to_save,
      unzip = unzip,
      remove = remove_zip
    )
  }
  # ----------------------------------------------------------------------------
  # Return results or hash.
  amadeus:::download_return(download_result, hash, chr_dir_to_save)
}

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process wildfire smoke data
#' @description
#' The \code{process_hms()} function imports and cleans raw wildfire smoke
#' plume coverage data, returning a single `SpatVector` object.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param path character(1). Directory with downloaded NOAA HMS data files.
#' @param extent numeric(4) or SpatExtent giving the extent of the output
#'   if `NULL` (default), the entire data is returned
#' @param ... Placeholders.
#' @note
#' \code{process_hms()} will return a character object if there are no wildfire
#' smoke plumes present for the selected dates and density. The returned
#' character will contain the density value and the sequence of dates
#' for which no wildfire smoke plumes were detected (see "Examples").
#' If multiple density polygons overlap, the function will return the
#' highest density value.
#' @examples
#' hms <- process_hms(
#'   date = c("2018-12-30", "2019-01-01"),
#'   path = "../tests/testdata/hms/"
#' )
# nolint end
#' @author Mitchell Manware
#' @return a `SpatVector` or character object
#' @importFrom terra vect
#' @importFrom terra aggregate
#' @importFrom terra subset
#' @importFrom stats na.omit
#' @export
process_hms <- function(
  date = "2018-01-01",
  path = NULL,
  extent = NULL,
  ...
) {
  #### directory setup
  path <- amadeus:::download_sanitize_path(path)
  #### check for variable
  amadeus:::check_for_null_parameters(mget(ls()))
  #### check dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### identify file paths
  paths <- list.files(
    path,
    pattern = "hms_smoke*.*.shp$",
    full.names = TRUE
  )
  paths <- paths[grep(
    ".shp",
    paths
  )]
  #### identify dates based on user input
  dates_of_interest <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### dates of interest with hyphen for return in 0 polygon case
  dates_no_polygon <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = FALSE
  )
  #### subset file paths to only dates of interest
  data_paths <- unique(
    grep(
      paste(
        dates_of_interest,
        collapse = "|"
      ),
      paths,
      value = TRUE
    )
  )
  #### process data
  data_return <- terra::vect()
  for (d in seq_along(data_paths)) {
    data_date <- terra::vect(data_paths[d], extent = extent)
    data_date_p <- terra::project(
      data_date,
      "EPSG:4326"
    )
    #### subset to density of interest
    data_density <- data_date_p

    #### absent polygons (ie. December 31, 2018)
    if (nrow(data_density) == 0) {
      message(paste0(
        " smoke plume polygons absent for date ",
        as.Date(
          dates_of_interest[d],
          format = "%Y%m%d"
        ),
        ". Returning empty SpatVector.\n"
      ))
      next
    } else {
      date <- unique(
        as.Date(
          substr(data_density$Start, 1, 7),
          format = "%Y%j"
        )
      )
      ##### patch to correct multiple dates in satellite Start/End data
      # If two dates, overwrite based on date derived from the generated
      # date sequence
      date <- if (length(date) == 1) {
        date
      } else {
        idx_date <- match(dates_of_interest[d], gsub("-", "", date))
        date[idx_date]
      }
      message(paste0(
        "Cleaning smoke data for date ",
        date,
        "..."
      ))
      #### zero buffer to avoid self intersection
      data_0_buffer <- terra::buffer(
        data_density,
        width = 0
      )
      # add dummy polygons
      poly_dummy <-
        "POLYGON((-180 -86,-179.99 -86,-179.99 -85.99,-180 -85.99,-180 -86))"
      poly_dummy <- terra::vect(poly_dummy)
      poly_dummy <- rbind(poly_dummy, poly_dummy, poly_dummy)
      poly_dummy <- terra::set.crs(poly_dummy, terra::crs(data_0_buffer))
      poly_dummy$Density <- c("Heavy", "Medium", "Light")
      poly_dummy$.hmsdummy <- 1
      data_0_buffer_a <- rbind(data_0_buffer, poly_dummy)
      data_0_buffer_a$.hmsdummy <-
        ifelse(
          is.nan(data_0_buffer_a$.hmsdummy),
          NA,
          data_0_buffer_a$.hmsdummy
        )
      #### aggregate polygons
      data_aggregate <- terra::aggregate(
        data_0_buffer_a,
        by = "Density",
        dissolve = TRUE
      )

      # Density index sorting to Heavy-Medium-Light
      sort_index <- data_aggregate$Density
      sort_index <-
        match(
          c("Heavy", "Medium", "Light"),
          sort_index
        )
      sort_index <- stats::na.omit(sort_index)
      data_aggregate <- data_aggregate[sort_index, ]

      # union polygons. Heavy-Medium-Light are 1, 2, 3, respectively.
      data_aggregate <- terra::union(data_aggregate)
      data_aggregate$indx <-
        as.numeric(
          paste0(
            data_aggregate$id_1,
            data_aggregate$id_2,
            data_aggregate$id_3
          )
        )
      # cut integers into three classes, assign labels
      data_aggregate$Density <-
        cut(
          data_aggregate$indx,
          c(0, 10, 100, 1000),
          labels = c("Light", "Medium", "Heavy"),
          right = FALSE
        )
      # reaggregate to dissolve polygons
      data_aggregate <- terra::aggregate(data_aggregate, by = "Density")
      data_aggregate$Date <- date
      #### factorize
      #### select "Density" and "Date"
      data_aggregate <- data_aggregate[, c("Density", "Date")]
      #### merge with other data
      data_return <- rbind(data_return, data_aggregate)
      reext <- terra::ext(c(-180, 180, -65, 85))
      data_return <- terra::crop(data_return, reext)
    }
  }
  #### if no polygons
  if (nrow(data_return) == 0) {
    message(paste0(
      "Smoke plume polygons absent from ",
      as.Date(
        dates_of_interest[1],
        format = "%Y%m%d"
      ),
      " to ",
      as.Date(
        dates_of_interest[length(dates_of_interest)],
        format = "%Y%m%d"
      ),
      ". Returning vector of dates.\n"
    ))
    no_polygon_return <- c(as.character(dates_no_polygon))
    return(no_polygon_return)
  } else if (nrow(data_return) > 0) {
    message(paste0(
      "Returning daily smoke data from ",
      as.Date(
        dates_of_interest[1],
        format = "%Y%m%d"
      ),
      " to ",
      as.Date(
        dates_of_interest[length(dates_of_interest)],
        format = "%Y%m%d"
      ),
      ".\n"
    ))
    #### return SpatVector
    return(data_return)
  }
}

################################################################################
# {calculate} tier
################################################################################
#' Calculate wildfire smoke covariates
#' @description
#' Extract wildfire smoke plume values at point or buffered locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, date, and either binary
#' indicators (`frac = FALSE`) or fractional overlap values (`frac = TRUE`) for
#' wildfire smoke plume density inherited from \code{from}.
#' @param from SpatVector(1). Output of \code{process_hms()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param .by_time NULL or character(1). Optional time grouping key used
#' when \code{.by_time} is provided. When supplied, HMS indicators are
#' summarized by \code{sum} (smoke-day counts) for `frac = FALSE`, or
#' \code{mean} for `frac = TRUE`.
#' @param frac logical(1). Default `FALSE`. If `FALSE`, return binary 0/1 smoke
#' indicators by density class. If `TRUE`, return fractional overlap by density
#' class.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @seealso [process_hms()]
#' @author Mitchell Manware
#' @return a data.frame or SpatVector object. When \code{.by_time} is provided,
#'   rows are aggregated using \code{calc_summarize_by()}.
#' @importFrom terra vect as.data.frame time extract crs
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr all_of
#' @importFrom stats setNames
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_hms(
#'   from = hms, # derived from process_hms() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   geom = FALSE
#' )
#' }
#' @export
calculate_hms <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  weights = NULL,
  .by_time = NULL,
  frac = FALSE,
  geom = FALSE,
  ...
) {
  #### check for null parameters (.by_time is optional)
  params_check <- mget(ls())
  params_check[c(".by_time", "weights")] <- NULL
  amadeus:::check_for_null_parameters(params_check)
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
    stop("`frac` should be a single logical value (TRUE/FALSE).")
  }
  #### from == character indicates no wildfire smoke plumes are present
  #### return 0 for all densities, locs and dates
  if (is.character(from)) {
    amadeus::check_geom(geom)
    message(paste0(
      "Inherited list of dates due to absent smoke plume polygons.\n"
    ))
    zero_value <- if (isTRUE(frac)) 0 else 0L
    skip_df <- data.frame(
      as.POSIXlt(from),
      zero_value,
      zero_value,
      zero_value
    )
    colnames(skip_df) <- c(
      "time",
      paste0("light_", sprintf("%05d", radius)),
      paste0("medium_", sprintf("%05d", radius)),
      paste0("heavy_", sprintf("%05d", radius))
    )
    # fixed: locs is replicated per the length of from
    skip_merge <-
      Reduce(
        rbind,
        Map(
          function(x) {
            cbind(locs, skip_df[rep(x, nrow(locs)), ])
          },
          seq_len(nrow(skip_df))
        )
      )

    if (!is.null(.by_time)) {
      hms_fun_summary <- if (isTRUE(frac)) "mean" else "sum"
      skip_merge <- amadeus::calc_summarize_by(
        covar = skip_merge,
        .by_time = .by_time,
        fun_summary = hms_fun_summary,
        locs_id = locs_id
      )
      did_summarize <- TRUE
    } else {
      did_summarize <- FALSE
    }
    if (did_summarize && "time" %in% names(skip_merge)) {
      skip_merge$time <- as.POSIXct(skip_merge$time, tz = "UTC")
    }
    skip_return <- amadeus::calc_return_locs(
      skip_merge,
      POSIXt = TRUE,
      geom = geom,
      crs = "EPSG:4326"
    )
    return(skip_return)
  }
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]

  #### generate date sequence for missing polygon patch
  date_sequence <- amadeus::generate_date_sequence(
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

  ### split date_sequence by 30 days
  date_sequence_split <-
    split(date_sequence, ceiling(seq_along(date_sequence) / 30))
  return_list <- vector("list", length(date_sequence_split))

  ### extract layer data at sites
  for (i in seq_along(date_sequence_split)) {
    message(paste0(
      "Calculating smoke intensity covariates for date range: ",
      date_sequence_split[[i]][1],
      " to ",
      date_sequence_split[[i]][length(date_sequence_split[[i]])]
    ))

    ### Full SPT
    data_template <- expand.grid(
      id = sites_id[[locs_id]],
      time = date_sequence_split[[i]]
    )
    data_template <- stats::setNames(data_template, c(locs_id, "time"))
    from_sub <- from[from$Date %in% date_sequence_split[[i]], ]

    is_point_locs <- all(
      tolower(terra::geomtype(sites_e)) %in% c("points", "point")
    )
    if (nrow(from_sub) == 0) {
      sites_extracted_layer <- data.frame(
        setNames(list(character(0)), locs_id),
        Date = character(0),
        Density = character(0),
        base_value = numeric(0)
      )
    } else if (radius == 0 && is_point_locs) {
      sites_extracted_layer <- terra::extract(from_sub, sites_e)
      sites_extracted_layer$id.y <-
        unlist(sites_e[[locs_id]])[sites_extracted_layer$id.y]
      names(sites_extracted_layer)[
        names(sites_extracted_layer) == "id.y"
      ] <- locs_id
      sites_extracted_layer$base_value <- 1
    } else {
      intersections <- terra::intersect(sites_e, from_sub)
      if (nrow(intersections) > 0) {
        inter_area <- terra::expanse(intersections)
        sites_extracted_layer <- terra::as.data.frame(intersections)
        if (isTRUE(frac)) {
          site_area <- terra::expanse(sites_e)
          site_lookup <- setNames(site_area, as.character(sites_e[[locs_id]]))
          denom <- as.numeric(
            site_lookup[as.character(sites_extracted_layer[[locs_id]])]
          )
          denom[!is.finite(denom) | denom <= 0] <- NA_real_
          sites_extracted_layer$base_value <- inter_area / denom
          sites_extracted_layer$base_value[
            !is.finite(sites_extracted_layer$base_value)
          ] <- 0
        } else {
          sites_extracted_layer$base_value <- 1
        }
      } else {
        sites_extracted_layer <- data.frame(
          setNames(list(character(0)), locs_id),
          Date = character(0),
          Density = character(0),
          base_value = numeric(0)
        )
      }
    }

    # remove unmatched extraction placeholders before aggregating
    if (nrow(sites_extracted_layer) > 0) {
      sites_extracted_layer <- sites_extracted_layer[
        !is.na(sites_extracted_layer$Date) &
          !is.na(sites_extracted_layer$Density),
        ,
        drop = FALSE
      ]
    }
    # remove duplicates and aggregate by site/date/density
    if (nrow(sites_extracted_layer) > 0) {
      sites_extracted_layer <- unique(
        sites_extracted_layer[, c(locs_id, "Date", "Density", "base_value")]
      )
      sites_extracted_layer <- stats::aggregate(
        base_value ~ .,
        data = sites_extracted_layer,
        FUN = sum
      )
      if (!isTRUE(frac)) {
        sites_extracted_layer$base_value <- as.integer(
          sites_extracted_layer$base_value > 0
        )
      } else {
        sites_extracted_layer$base_value <- pmin(
          sites_extracted_layer$base_value,
          1
        )
      }
    }

    #### merge with site_id and date
    sites_extracted_layer <-
      tidyr::pivot_wider(
        data = sites_extracted_layer,
        names_from = "Density",
        values_from = "base_value",
        id_cols = dplyr::all_of(c(locs_id, "Date")),
        values_fill = list(base_value = 0)
      )

    # Fill in missing columns
    levels_acceptable <- c("Light", "Medium", "Heavy")
    # Detect missing columns
    col_tofill <-
      setdiff(levels_acceptable, names(sites_extracted_layer))
    # Fill zeros
    if (length(col_tofill) > 0) {
      sites_extracted_layer[col_tofill] <- if (isTRUE(frac)) 0 else 0L
    }
    col_order <- c(locs_id, "Date", levels_acceptable)
    sites_extracted_layer <- sites_extracted_layer[, col_order]
    sites_extracted_layer <-
      stats::setNames(
        sites_extracted_layer,
        c(locs_id, "time", levels_acceptable)
      )

    binary_colname <-
      paste0(tolower(levels_acceptable), "_", sprintf("%05d", radius))
    # sites_extracted_layer$time <- as.Date(sites_extracted_layer$time)
    sites_extracted_layer <-
      stats::setNames(
        sites_extracted_layer,
        c(locs_id, "time", binary_colname)
      )

    # Filling NAs to 0 (explicit integer)
    # sites_extracted_layer[is.na(sites_extracted_layer)] <- 0L

    # Join full space-time pairs with extracted data
    site_extracted <- merge(
      data_template,
      sites_extracted_layer,
      by = c(locs_id, "time"),
      all.x = TRUE
    )
    # append list with the extracted data.frame
    return_list[[i]] <- site_extracted
  }

  ### Merge data.frame in list
  sites_extracted <- do.call(rbind, return_list)

  #### define column names
  colname_common <- c(locs_id, "time", binary_colname)
  if (geom %in% c("sf", "terra")) {
    sites_extracted <-
      merge(sites_extracted, sites_id, by = locs_id)
    sites_extracted <-
      stats::setNames(
        sites_extracted,
        c(colname_common, "geometry")
      )
  } else {
    sites_extracted <-
      stats::setNames(
        sites_extracted,
        colname_common
      )
  }
  # Filling NAs to 0 for smoke columns
  for (smoke_col in binary_colname) {
    sites_extracted[[smoke_col]][is.na(sites_extracted[[smoke_col]])] <-
      if (isTRUE(frac)) 0 else 0L
  }

  if (!is.null(.by_time)) {
    hms_fun_summary <- if (isTRUE(frac)) "mean" else "sum"
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = hms_fun_summary,
      locs_id = locs_id
    )
    did_summarize <- TRUE
  } else {
    did_summarize <- FALSE
  }

  # Messaging
  timevals <- sites_extracted[["time"]]
  intensities <- sites_extracted[, binary_colname, drop = FALSE]
  intensities <- apply(intensities, 1, sum)
  time_allzero <- unique(timevals[intensities == 0])
  time_allzero_c <- paste(time_allzero, collapse = "\n")
  message(paste0(
    "No intersecting smoke plume polygons for date(s):\n",
    time_allzero_c
  ))

  #### date to POSIXct
  if ("time" %in% names(sites_extracted)) {
    sites_extracted$time <- as.POSIXct(sites_extracted$time)
  }
  #### order by date
  sites_extracted_ordered <- as.data.frame(
    sites_extracted[order(sites_extracted$time), ]
  )
  message("Returning smoke intensity covariates.")
  sites_extracted_ordered <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_extracted_ordered)
}
################################################################################
