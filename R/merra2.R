# nolint start
#' Download meteorological and atmospheric data
#' @description
#' The \code{download_merra2()} function accesses and downloads various
#' meteorological and atmospheric collections from [NASA's Modern-Era
#' Retrospective analysis for Research and Applications, Version 2
#' (MERRA-2) model](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/), and the
#' daily corrected Global Fire Weather Index (FWI) product derived from MERRA-2
#' weather inputs.
#' @note Due to NASA data access policies, standard MERRA-2 GES DISC downloads
#' require a valid NASA Earthdata token for authentication. Use
#' \code{setup_nasa_token()} for setup. The `"fwi"` collection is hosted on the
#' public GlobalFWI portal and does not require Earthdata authentication.
#' @param collection character(1). MERRA-2 data collection file name, or
#'   `"fwi"` for the daily corrected Global Fire Weather Index product
#'   (`MERRA2.CORRECTED`).
#' @param nasa_earth_data_token character(1) or NULL. NASA EarthData
#'   authentication token.
#' @param date character(1 or 2). length of 10. Date or start/end dates
#'   for downloading data.
#' Format "YYYY-MM-DD" (ex. January 1, 2018 = `"2018-01-01"`).
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param hash logical(1). By setting \code{TRUE} the function will return
#' an \code{rlang::hash_file()} hash character corresponding to the
#' downloaded files. Default is \code{FALSE}.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song, Kyle Messier
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @examples
#' \dontrun{
#' download_merra2(
#'   collection = "inst1_2d_int_Nx",
#'   date = "2024-01-01",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
# nolint end
download_merra2 <- function(
  collection = c(
    "inst1_2d_asm_Nx",
    "inst1_2d_int_Nx",
    "inst1_2d_lfo_Nx",
    "inst3_3d_asm_Np",
    "inst3_3d_aer_Nv",
    "inst3_3d_asm_Nv",
    "inst3_3d_chm_Nv",
    "inst3_3d_gas_Nv",
    "inst3_2d_gas_Nx",
    "inst6_3d_ana_Np",
    "inst6_3d_ana_Nv",
    "statD_2d_slv_Nx",
    "tavg1_2d_adg_Nx",
    "tavg1_2d_aer_Nx",
    "tavg1_2d_chm_Nx",
    "tavg1_2d_csp_Nx",
    "tavg1_2d_flx_Nx",
    "tavg1_2d_int_Nx",
    "tavg1_2d_lfo_Nx",
    "tavg1_2d_lnd_Nx",
    "tavg1_2d_ocn_Nx",
    "tavg1_2d_rad_Nx",
    "tavg1_2d_slv_Nx",
    "tavg3_3d_mst_Ne",
    "tavg3_3d_trb_Ne",
    "tavg3_3d_nav_Ne",
    "tavg3_3d_cld_Np",
    "tavg3_3d_mst_Np",
    "tavg3_3d_rad_Np",
    "tavg3_3d_tdt_Np",
    "tavg3_3d_trb_Np",
    "tavg3_3d_udt_Np",
    "tavg3_3d_odt_Np",
    "tavg3_3d_qdt_Np",
    "tavg3_3d_asm_Nv",
    "tavg3_3d_cld_Nv",
    "tavg3_3d_mst_Nv",
    "tavg3_3d_rad_Nv",
    "tavg3_2d_glc_Nx",
    "fwi"
  ),
  nasa_earth_data_token = NULL,
  date = c("2018-01-01", "2018-01-01"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
) {
  #### 1. Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### 2. Directory setup (can be done early)
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### 3. Check dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]

  #### 3a. Handle deprecated parameters BEFORE token check
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

  #### 4. Check and retrieve NASA token for standard MERRA-2 collections only
  standard_collection <- collection != "fwi"
  if (any(standard_collection)) {
    nasa_earth_data_token <- amadeus::get_token(
      token = nasa_earth_data_token,
      env_var = "NASA_EARTHDATA_TOKEN"
    )
  }

  #### 5. Now check for null parameters
  parameters <- mget(ls())
  if (!any(standard_collection)) {
    parameters$nasa_earth_data_token <- ""
  }
  parameters <- parameters
  amadeus::check_for_null_parameters(parameters)

  #### 7. Check if collection is recognized
  identifiers <- c(
    "inst1_2d_asm_Nx M2I1NXASM 10.5067/3Z173KIE2TPD",
    "inst1_2d_int_Nx M2I1NXINT 10.5067/G0U6NGQ3BLE0",
    "inst1_2d_lfo_Nx M2I1NXLFO 10.5067/RCMZA6TL70BG",
    "inst3_3d_asm_Np M2I3NPASM 10.5067/QBZ6MG944HW0",
    "inst3_3d_aer_Nv M2I3NVAER 10.5067/LTVB4GPCOTK2",
    "inst3_3d_asm_Nv M2I3NVASM 10.5067/WWQSXQ8IVFW8",
    "inst3_3d_chm_Nv M2I3NVCHM 10.5067/HO9OVZWF3KW2",
    "inst3_3d_gas_Nv M2I3NVGAS 10.5067/96BUID8HGGX5",
    "inst3_2d_gas_Nx M2I3NXGAS 10.5067/HNGA0EWW0R09",
    "inst6_3d_ana_Np M2I6NPANA 10.5067/A7S6XP56VZWS",
    "inst6_3d_ana_Nv M2I6NVANA 10.5067/IUUF4WB9FT4W",
    "statD_2d_slv_Nx M2SDNXSLV 10.5067/9SC1VNTWGWV3",
    "tavg1_2d_adg_Nx M2T1NXADG 10.5067/HM00OHQBHKTP",
    "tavg1_2d_aer_Nx M2T1NXAER 10.5067/KLICLTZ8EM9D",
    "tavg1_2d_chm_Nx M2T1NXCHM 10.5067/3RQ5YS674DGQ",
    "tavg1_2d_csp_Nx M2T1NXCSP 10.5067/H0VVAD8F6MX5",
    "tavg1_2d_flx_Nx M2T1NXFLX 10.5067/7MCPBJ41Y0K6",
    "tavg1_2d_int_Nx M2T1NXINT 10.5067/Q5GVUVUIVGO7",
    "tavg1_2d_lfo_Nx M2T1NXLFO 10.5067/L0T5GEG1NYFA",
    "tavg1_2d_lnd_Nx M2T1NXLND 10.5067/RKPHT8KC1Y1T",
    "tavg1_2d_ocn_Nx M2T1NXOCN 10.5067/Y67YQ1L3ZZ4R",
    "tavg1_2d_rad_Nx M2T1NXRAD 10.5067/Q9QMY5PBNV1T",
    "tavg1_2d_slv_Nx M2T1NXSLV 10.5067/VJAFPLI1CSIV",
    "tavg3_3d_mst_Ne M2T3NEMST 10.5067/JRUZ3SJ3ZJ72",
    "tavg3_3d_trb_Ne M2T3NETRB 10.5067/4I7ZI35QRH8K",
    "tavg3_3d_nav_Ne M2T3NENAV 10.5067/N5WAKNS1UYQN",
    "tavg3_3d_cld_Np M2T3NPCLD 10.5067/TX10URJSKT53",
    "tavg3_3d_mst_Np M2T3NPMST 10.5067/0TUFO90Q2PMS",
    "tavg3_3d_rad_Np M2T3NPRAD 10.5067/3UGE8WQXZAOK",
    "tavg3_3d_tdt_Np M2T3NPTDT 10.5067/9NCR9DDDOPFI",
    "tavg3_3d_trb_Np M2T3NPTRB 10.5067/ZRRJPGWL8AVL",
    "tavg3_3d_udt_Np M2T3NPUDT 10.5067/CWV0G3PPPWFW",
    "tavg3_3d_odt_Np M2T3NPODT 10.5067/S0LYTK57786Z",
    "tavg3_3d_qdt_Np M2T3NPQDT 10.5067/A9KWADY78YHQ",
    "tavg3_3d_asm_Nv M2T3NVASM 10.5067/SUOQESM06LPK",
    "tavg3_3d_cld_Nv M2T3NVCLD 10.5067/F9353J0FAHIH",
    "tavg3_3d_mst_Nv M2T3NVMST 10.5067/ZXTJ28TQR1TR",
    "tavg3_3d_rad_Nv M2T3NVRAD 10.5067/7GFQKO1T43RW",
    "tavg3_2d_glc_Nx M2T3NXGLC 10.5067/9ETB4TT5J6US"
  )
  identifiers <- lapply(identifiers, strsplit, split = " ")
  identifiers <- lapply(identifiers, function(x) matrix(x[[1]], nrow = 1))
  identifiers <- do.call(rbind, identifiers)
  identifiers_df <- as.data.frame(identifiers)
  colnames(identifiers_df) <- c("collection_id", "esdt_name", "DOI")

  if (!all(collection[standard_collection] %in% identifiers_df$collection_id)) {
    message(identifiers_df)
    stop(paste0(
      "Requested collection is not recognized.\n",
      "Please refer to the table above to find a proper collection.\n"
    ))
  }

  #### 8. Define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )

  #### 9. Define year + month sequence
  yearmonth_sequence <- unique(substr(date_sequence, 1, 6))

  #### 10. Collect all URLs and destination files
  all_urls <- character()
  all_destfiles <- character()

  for (c in seq_along(collection)) {
    collection_loop <- collection[c]

    if (collection_loop == "fwi") {
      base <- paste0(
        "https://portal.nccs.nasa.gov/datashare/GlobalFWI/v2.0/",
        "fwiCalcs.MERRA2/Default/MERRA2.CORRECTED/"
      )
      download_folder <- paste0(directory_to_save, collection_loop)
      if (!dir.exists(download_folder)) {
        dir.create(download_folder, recursive = TRUE)
      }
      for (d in seq_along(date_sequence)) {
        date_loop <- date_sequence[d]
        year <- substr(date_loop, 1, 4)
        file_name <- paste0(
          "FWI.MERRA2.CORRECTED.Daily.Default.",
          date_loop,
          ".nc"
        )
        download_url <- paste0(base, year, "/", file_name)
        download_name <- paste0(download_folder, "/", file_name)
        if (amadeus::check_destfile(download_name)) {
          all_urls <- c(all_urls, download_url)
          all_destfiles <- c(all_destfiles, download_name)
        }
      }
      next
    }

    #### Define ESDT name
    identifiers_df_requested <- subset(
      identifiers_df,
      subset = identifiers_df$collection_id == collection_loop
    )
    esdt_name <- identifiers_df_requested[, 2]

    #### Define URL base (goldsmr4 vs goldsmr5)
    esdt_name_4 <- c(
      "M2I1NXASM",
      "M2I1NXINT",
      "M2I1NXLFO",
      "M2I3NXGAS",
      "M2SDNXSLV",
      "M2T1NXADG",
      "M2T1NXAER",
      "M2T1NXCHM",
      "M2T1NXCSP",
      "M2T1NXFLX",
      "M2T1NXINT",
      "M2T1NXLFO",
      "M2T1NXLND",
      "M2T1NXOCN",
      "M2T1NXRAD",
      "M2T1NXSLV",
      "M2T3NXGLC"
    )
    esdt_name_5 <- c(
      "M2I3NPASM",
      "M2I3NVAER",
      "M2I3NVASM",
      "M2I3NVCHM",
      "M2I3NVGAS",
      "M2I6NPANA",
      "M2I6NVANA",
      "M2T3NEMST",
      "M2T3NENAV",
      "M2T3NETRB",
      "M2T3NPCLD",
      "M2T3NPMST",
      "M2T3NPODT",
      "M2T3NPQDT",
      "M2T3NPRAD",
      "M2T3NPTDT",
      "M2T3NPTRB",
      "M2T3NPUDT",
      "M2T3NVASM",
      "M2T3NVCLD",
      "M2T3NVMST",
      "M2T3NVRAD"
    )

    if (esdt_name %in% esdt_name_4) {
      base <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/"
    } else if (esdt_name %in% esdt_name_5) {
      base <- "https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2/"
    }
    #### Get file listings using httr2
    for (y in seq_along(yearmonth_sequence)) {
      year <- substr(yearmonth_sequence[y], 1, 4)
      month <- substr(yearmonth_sequence[y], 5, 6)

      base_url <- paste0(
        base,
        esdt_name,
        ".5.12.4/",
        year,
        "/",
        month,
        "/"
      )
      # Validate first URL only
      if (c == 1 && y == 1) {
        if (!amadeus::check_url_status(base_url)) {
          stop(paste0(
            "Invalid date returns HTTP code 404. ",
            "Check `date` parameter.\n"
          ))
        }
      }

      # Get directory listing using httr2
      tryCatch(
        {
          resp <- httr2::request(base_url) |>
            httr2::req_perform()

          html_content <- httr2::resp_body_string(resp)

          # Extract .nc4 files from HTML
          nc4_pattern <- 'href="([^"]*\\.nc4)"'
          nc4_matches <- gregexpr(nc4_pattern, html_content, perl = TRUE)
          list_urls_month <- regmatches(html_content, nc4_matches)[[1]]
          list_urls_month <- gsub('href="|"', "", list_urls_month)

          # Also get .xml files
          xml_pattern <- 'href="([^"]*\\.nc4\\.xml)"'
          xml_matches <- gregexpr(xml_pattern, html_content, perl = TRUE)
          list_xml_month <- regmatches(html_content, xml_matches)[[1]]
          list_xml_month <- gsub('href="|"', "", list_xml_month)

          # Filter by date sequence and remove duplicate links from HTML listing
          list_urls_date_sequence <- unique(list_urls_month[
            substr(basename(list_urls_month), 28, 35) %in% date_sequence
          ])
          list_xml_date_sequence <- unique(list_xml_month[
            substr(basename(list_xml_month), 28, 35) %in% date_sequence
          ])

          # Create download folder
          download_folder <- paste0(directory_to_save, collection_loop)
          if (!dir.exists(download_folder)) {
            dir.create(download_folder, recursive = TRUE)
          }

          # Create metadata folder
          download_folder_metadata <- paste0(download_folder, "/metadata/")
          if (!dir.exists(download_folder_metadata)) {
            dir.create(download_folder_metadata, recursive = TRUE)
          }

          # Build URLs and destination files for data
          for (f in list_urls_date_sequence) {
            download_url <- paste0(base_url, f)
            download_name <- paste0(download_folder, "/", f)

            if (amadeus::check_destfile(download_name)) {
              all_urls <- c(all_urls, download_url)
              all_destfiles <- c(all_destfiles, download_name)
            }
          }

          # Build URLs and destination files for metadata
          for (f in list_xml_date_sequence) {
            download_url_metadata <- paste0(base_url, f)
            download_name_metadata <- paste0(download_folder_metadata, f)

            if (amadeus::check_destfile(download_name_metadata)) {
              all_urls <- c(all_urls, download_url_metadata)
              all_destfiles <- c(all_destfiles, download_name_metadata)
            }
          }
        },
        error = function(e) {
          warning(
            sprintf(
              "Failed to get directory listing for %s: %s\n",
              base_url,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
    }
  }

  #### 11. Exit early if download=FALSE
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available for download.\n",
      length(all_urls)
    ))
    return(invisible(list(
      urls = all_urls,
      destfiles = all_destfiles,
      n_files = length(all_urls)
    )))
  }

  #### 12. Download files using httr2
  if (length(all_urls) > 0) {
    download_result <- amadeus::download_run_method(
      urls = all_urls,
      destfiles = all_destfiles,
      token = nasa_earth_data_token, # Now passing the NASA token!
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    message("All files already exist. Nothing to download.\n")
    download_result <- list(
      success = 0,
      failed = 0,
      skipped = length(all_destfiles)
    )
  }

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

