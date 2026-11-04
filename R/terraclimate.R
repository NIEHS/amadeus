#' Download TerraClimate data
#' @description
#' The \code{download_terraclimate} function accesses and downloads climate
#' and water balance data from the University of California Merced
#' Climatology Lab's TerraClimate dataset.
#' @note TerraClimate data does not require authentication.
#' @param variables character. Variable(s) name(s).
#' @param year integer(1 or 2). Year or start/end years for downloading data.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_abatzoglou2018terraclimate}{amadeus}
#' @examples
#' \dontrun{
#' download_terraclimate(
#'   variables = "ppt",
#'   year = 2023,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_terraclimate <- function(
  variables = NULL,
  year = c(2018, 2022),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

  #### Check years
  year <- if (length(year) == 1) rep(year, 2) else year
  stopifnot(length(year) == 2)
  year <- year[order(year)]

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

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

  #### Define years sequence
  if (any(nchar(year[1]) != 4, nchar(year[2]) != 4)) {
    stop("years should be 4-digit integers.\n")
  }
  years <- seq(year[1], year[2], 1)

  #### Define variables
  variables_list <- amadeus::process_variable_codes(
    variables = variables,
    source = "terraclimate"
  )

  #### Define URL base
  base <- paste0(
    "https://climate.northwestknowledge.net/TERRACLIMATE-DATA/",
    "TerraClimate_"
  )

  #### Collect all URLs and destination files
  all_urls <- character()
  all_destfiles <- character()

  for (v in seq_along(variables_list)) {
    variable <- variables_list[v]
    folder <- paste0(directory_to_save, variable, "/")

    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }

    for (y in seq_along(years)) {
      year_l <- years[y]
      url <- paste0(base, variable, "_", year_l, ".nc")

      # Validate first URL only
      if (v == 1 && y == 1) {
        if (!amadeus::check_url_status(url)) {
          warning(
            paste0(
              "Preflight URL check failed; attempting direct download ",
              "for year validation.\n"
            ),
            call. = FALSE
          )
        }
      }

      destfile <- paste0(
        directory_to_save,
        variable,
        "/",
        variable,
        "_",
        year_l,
        ".nc"
      )

      if (amadeus::check_destfile(destfile)) {
        all_urls <- c(all_urls, url)
        all_destfiles <- c(all_destfiles, destfile)
      }
    }
  }

  #### Exit early if download=FALSE
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

  #### Download files using httr2
  download_result <- amadeus::download_run_method(
    urls = all_urls,
    destfiles = all_destfiles,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

