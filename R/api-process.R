################################################################################
# Primary {process_covariates} function.
################################################################################

################################################################################
# nolint start
#' Process raw data wrapper function
#' @description
#' This function processes raw data files which have
#' been downloaded by [`download_data`]. `process_covariates` and
#' the underlying source-specific processing functions have been designed to
#' operate on the raw data files. To avoid errors, \strong{do not edit the raw
#' data files before passing to `process_covariates`}.
#' @param covariate character(1). Covariate type.
#' @param path character(1). Directory or file path to raw data
#' depending on `covariate` value.
#' @param ... Arguments passed to each raw data processing function.
#' @seealso
#' * \code{\link{process_modis_swath}}: "modis_swath"
#' * \code{\link{process_modis_merge}}: "modis_merge"
#' * \code{\link{process_blackmarble}}: "blackmarble"
#' * \code{\link{process_koppen_geiger}}: "koppen-geiger", "koeppen-geiger", "koppen"
#' * \code{\link{process_ecoregion}}: "ecoregion", "ecoregions"
#' * \code{\link{process_nlcd}}: "nlcd", "NLCD"
#' * \code{\link{process_tri}}: "tri", "TRI"
#' * \code{\link{process_nei}}: "nei", "NEI"
#' * \code{\link{process_geos}}: "geos", "GEOS"
#' * \code{\link{process_goes}}: "goes", "goes_adp", "GOES"
#' * \code{\link{process_gmted}}: "gmted", "GMTED"
#' * \code{\link{process_aqs}}: "aqs", "AQS"
#' * \code{\link{process_edgar}}: "edgar"
#' * \code{\link{process_improve}}: "improve", "IMPROVE"
#' * \code{\link{process_hms}}: "hms", "smoke", "HMS"
#' * \code{\link{process_narr}}: "narr", "NARR"
#' * \code{\link{process_groads}}: "sedac_groads", "roads", "groads"
#' * \code{\link{process_population}}: "sedac_population", "population"
#' * \code{\link{process_merra2}}: "merra", "merra2", "MERRA2"
#' * \code{\link{process_gridmet}}: "gridmet", "gridMET"
#' * \code{\link{process_terraclimate}}: "terraclimate", "TerraClimate"
#' * \code{\link{process_huc}}: "huc", "HUC"
#' * \code{\link{process_cropscape}}: "cropscape", "cdl"
#' * \code{\link{process_prism}}: "prism", "PRISM"
#' * \code{\link{process_drought}}: "drought", "spei", "eddi", "usdm"
#' @return `SpatVector`, `SpatRaster`, `sf`, `data.table`, or `character` depending on
#' covariate type and selections.
#' @author Insang Song
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' process_covariates(
#'   covariate = "narr",
#'   date = c("2018-01-01", "2018-01-10"),
#'   variable = "weasd",
#'   path = system.file("extdata", "examples", "narr", "weasd")
#' )
#' }
#' @export
# nolint end
process_covariates <-
  function(
    covariate = c(
      "modis_swath",
      "modis_merge",
      "mcd14ml",
      "koppen-geiger",
      "blackmarble",
      "koeppen-geiger",
      "koppen",
      "koeppen",
      "geos",
      "goes",
      "goes_adp",
      "GOES",
      "dummies",
      "gmted",
      "aqs",
      "hms",
      "smoke",
      "sedac_population",
      "population",
      "sedac_groads",
      "groads",
      "roads",
      "nlcd",
      "tri",
      "narr",
      "nei",
      "ecoregions",
      "ecoregion",
      "merra",
      "merra2",
      "gridmet",
      "terraclimate",
      "huc",
      "cropscape",
      "cdl",
      "prism",
      "edgar",
      "improve",
      "IMPROVE",
      "drought",
      "spei",
      "eddi",
      "usdm"
    ),
    path = NULL,
    ...
  ) {
    covariate <- tolower(covariate)
    covariate <- match.arg(covariate)
    if (startsWith(covariate, "ko")) {
      covariate <- "koppen"
    }

    # select function to run
    what_to_run <- switch(
      covariate,
      modis_merge = process_modis_merge,
      modis_swath = process_modis_swath,
      mcd14ml = process_mcd14ml,
      blackmarble = process_blackmarble,
      ecoregion = process_ecoregion,
      ecoregions = process_ecoregion,
      koppen = process_koppen_geiger,
      narr = process_narr,
      nlcd = process_nlcd,
      smoke = process_hms,
      hms = process_hms,
      sedac_groads = process_groads,
      roads = process_groads,
      groads = process_groads,
      sedac_population = process_population,
      population = process_population,
      nei = process_nei,
      tri = process_tri,
      geos = process_geos,
      goes = process_goes,
      goes_adp = process_goes,
      gmted = process_gmted,
      aqs = process_aqs,
      merra = process_merra2,
      merra2 = process_merra2,
      gridmet = process_gridmet,
      terraclimate = process_terraclimate,
      huc = process_huc,
      cropscape = process_cropscape,
      cdl = process_cropscape,
      prism = process_prism,
      edgar = process_edgar,
      improve = process_improve,
      drought = process_drought,
      spei = function(path, ...) {
        process_drought(path = path, source = "spei", ...)
      },
      eddi = function(path, ...) {
        process_drought(path = path, source = "eddi", ...)
      },
      usdm = function(path, ...) {
        process_drought(path = path, source = "usdm", ...)
      }
    )

    res_covariate <-
      tryCatch(
        {
          what_to_run(
            path = path,
            ...
          )
        },
        error = function(e) {
          stop(
            paste0(
              e,
              "\n",
              paste0(deparse(args(what_to_run)), collapse = "\n"),
              "\n",
              "Please refer to the argument list and ",
              "the error message above to rectify the error.\n"
            )
          )
        }
      )

    return(res_covariate)
  }
