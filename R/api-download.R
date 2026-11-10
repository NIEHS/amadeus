################################################################################
# Primary {download_data} function.
################################################################################

################################################################################
#' Download raw data wrapper function
# nolint start
#' @description
#' The \code{download_data()} function accesses and downloads atmospheric,
#' meteorological, and environmental data from various open-access data
#' sources.
# nolint end
#' @param dataset_name character(1). Dataset name or alias from the list below.
#'   Matching ignores case and accepts unambiguous partial matches. The default
#'   selects `"aqs"`.
#' @param directory_to_save character(1). Directory to save / unzip
#'  (if zip files are downloaded) data.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param hash logical(1). Return the source function's combined MD5 directory
#'   hash instead of its download summary. Default is `FALSE`. The directory
#'   included in the hash depends on the source and can include existing files.
#'   Ignored when `download = FALSE` is passed through `...`.
#' @param nasa_earth_data_token character(1) or NULL. NASA EarthData
#' authentication token. Required for NASA EarthData datasets: `"geos"`,
#' `"merra2"`, `"modis"`, `"sedac_groads"` / `"groads"`, and
#' `"sedac_population"` / `"population"`. Can be a token string, a path to a
#' file containing the token, or \code{NULL} to read from the
#' \code{NASA_EARTHDATA_TOKEN} environment variable. Ignored for datasets that
#' do not use NASA EarthData authentication.
#' @param rate_limit numeric(1). Non-negative request-throttling interval in
#'   seconds, passed to the source function. Default is 2. This controls the
#'   average request rate rather than a strict minimum gap between requests.
#' @param ... Source-specific selections and download controls, such as `date`,
#'   `year`, `timescale`, `version`, `download`, `show_progress`, `max_tries`,
#'   `unzip`, and `remove_zip`. See the selected source function for its
#'    arguments.
#' @details
#' This wrapper selects a downloader and forwards arguments. The source function
#' performs validation, directory setup, discovery, downloading, and extraction.
#' Previews and directory layouts follow that function's behavior.
#'
#' The `"drought"` alias calls [download_drought()] with its default SPEI source
#' unless `source` is supplied. The `"spei"`, `"eddi"`, and `"usdm"` aliases
#' call [download_spei()], [download_eddi()], and [download_usdm()] directly.
#' Supply the selected function's required arguments through `...`. The `source`
#' argument selects a drought source only when `dataset_name = "drought"`.
#' @note
#' - All download function names are in \code{download_*} formats
#' @author Insang Song
#' @seealso
#' For details of each download function per dataset,
#' Please refer to:
#' * \code{\link{download_aqs}}: `"aqs"`, `"AQS"`
#' * \code{\link{download_ecoregion}}: `"ecoregions"`, `"ecoregion"`
#' * \code{\link{download_geos}}: `"geos"`
#' * \code{\link{download_goes}}: `"goes"`, `"goes_adp"`, `"GOES"`
#' * \code{\link{download_gmted}}: `"gmted"`, `"GMTED"`
#' * \code{\link{download_koppen_geiger}}: `"koppen"`, `"koppengeiger"`
#' * \code{\link{download_merra2}}: "merra2", `"merra"`, `"MERRA"`, `"MERRA2"`
#' * \code{\link{download_narr}}: `"narr"`
#' * \code{\link{download_nlcd}}: `"nlcd"`, `"NLCD"`
#' * \code{\link{download_hms}}: `"noaa"`, `"smoke"`, `"hms"`
#' * \code{\link{download_groads}}: `"sedac_groads"`, `"groads"`
#' * \code{\link{download_population}}: `"sedac_population"`,
#'   `"population"`
#' * \code{\link{download_modis}}: `"modis"`, `"MODIS"`
#' * \code{\link{download_tri}}: `"tri"`, `"TRI"`
#' * \code{\link{download_nei}}: `"nei"`, `"NEI"`
#' * \code{\link{download_gridmet}}: `"gridMET"`, `"gridmet"`
#' * \code{\link{download_terraclimate}}: `"TerraClimate"`, `"terraclimate"`
#' * \code{\link{download_huc}}: `"huc"`
#' * \code{\link{download_cropscape}}: `"cropscape"`, `"cdl"`
#' * \code{\link{download_prism}}: `"prism"`
#' * \code{\link{download_edgar}}: `"edgar"`
#' * \code{\link{download_improve}}: `"improve"`, `"IMPROVE"`
#' * \code{\link{download_drought}}: `"drought"`
#' * \code{\link{download_spei}}: `"spei"`
#' * \code{\link{download_eddi}}: `"eddi"`
#' * \code{\link{download_usdm}}: `"usdm"`
#' @return
#' The selected downloader's return value, with its visibility preserved:
#' * With `download = FALSE`, an invisible preview list containing `urls`,
#'   `destfiles`, and `n_files`.
#' * With downloading enabled and `hash = FALSE`, an invisible summary
#'   containing `success`, `failed`, `skipped`, `failed_urls`, and
#'   `failed_files`. The two failure vectors are `character(0)` when no
#'   downloads failed, including when every file was skipped.
#' * With downloading enabled and `hash = TRUE`, a visible character scalar
#'   containing the combined MD5 hash of the source's output directory.
#' @examples
#' \dontrun{
#' download_data(
#'   dataset_name = "narr",
#'   variables = "weasd",
#'   year = 2023,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = FALSE # Deprecated; may still contact the source.
#' )
#' }
#' @export
download_data <- function(
  dataset_name = c(
    "aqs",
    "ecoregion",
    "ecoregions",
    "geos",
    "goes",
    "goes_adp",
    "gmted",
    "koppen",
    "koppengeiger",
    "merra2",
    "merra",
    "modis",
    "narr",
    "nlcd",
    "noaa",
    "sedac_groads",
    "sedac_population",
    "groads",
    "population",
    "hms",
    "smoke",
    "tri",
    "nei",
    "gridmet",
    "terraclimate",
    "huc",
    "cropscape",
    "cdl",
    "prism",
    "edgar",
    "improve",
    "drought",
    "spei",
    "eddi",
    "usdm"
  ),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  hash = FALSE,
  nasa_earth_data_token = NULL,
  rate_limit = 2,
  ...
) {
  # ----------------------------------------------------------------------------
  # Normalize the dataset name, select one source or alias.
  dataset_name <- tolower(dataset_name)
  chr_dataset <- match.arg(dataset_name)
  # ----------------------------------------------------------------------------
  # Select download function based on called source/alias.
  fun_download <- switch(
    chr_dataset,
    aqs = amadeus::download_aqs,
    ecoregion = amadeus::download_ecoregion,
    ecoregions = amadeus::download_ecoregion,
    geos = amadeus::download_geos,
    goes = amadeus::download_goes,
    goes_adp = amadeus::download_goes,
    gmted = amadeus::download_gmted,
    koppen = amadeus::download_koppen_geiger,
    koppengeiger = amadeus::download_koppen_geiger,
    merra2 = amadeus::download_merra2,
    merra = amadeus::download_merra2,
    narr = amadeus::download_narr,
    nlcd = amadeus::download_nlcd,
    noaa = amadeus::download_hms,
    smoke = amadeus::download_hms,
    hms = amadeus::download_hms,
    sedac_groads = amadeus::download_groads,
    groads = amadeus::download_groads,
    sedac_population = amadeus::download_population,
    population = amadeus::download_population,
    modis = amadeus::download_modis,
    tri = amadeus::download_tri,
    nei = amadeus::download_nei,
    gridmet = amadeus::download_gridmet,
    terraclimate = amadeus::download_terraclimate,
    huc = amadeus::download_huc,
    cropscape = amadeus::download_cropscape,
    cdl = amadeus::download_cropscape,
    prism = amadeus::download_prism,
    edgar = amadeus::download_edgar,
    improve = amadeus::download_improve,
    drought = amadeus::download_drought,
    spei = amadeus::download_spei,
    eddi = amadeus::download_eddi,
    usdm = amadeus::download_usdm
  )
  # ----------------------------------------------------------------------------
  # Forward shared controls together with the source-specific arguments.
  list_args <- c(
    list(
      directory_to_save = directory_to_save,
      acknowledgement = acknowledgement,
      hash = hash,
      rate_limit = rate_limit
    ),
    list(...)
  )
  # ----------------------------------------------------------------------------
  # Pass authentication only to downloaders that declare the token parameter.
  if ("nasa_earth_data_token" %in% names(formals(fun_download))) {
    list_args$nasa_earth_data_token <- nasa_earth_data_token
  }
  # ----------------------------------------------------------------------------
  # Preserve source-function result and visibility; argument context on errors.
  tryCatch(
    do.call(fun_download, list_args),
    error = function(e) {
      stop(
        paste0(
          conditionMessage(e),
          "\n",
          paste0(deparse(args(fun_download)), collapse = "\n"),
          "\n",
          "Please refer to the argument list and ",
          "the error message above to rectify the error.\n"
        ),
        call. = FALSE
      )
    }
  )
}
