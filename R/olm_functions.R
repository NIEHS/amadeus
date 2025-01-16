# nocov start
# nolint start
#' Download OpenLandMap data
#' @description
#' Accesses and downloads OpenLandMap data from the [OpenLandMap website](https://opengeohub.org/about-openlandmap/).
#' @param product character(1). Available collection name in OpenLandMap
#' STAC Catalog. [list_stac_files] with `id_only = TRUE` to see available collections.
#' * "no2_s5p.l3.trop.tmwm"
#' * "no2_s5p.l3.trop.tmwm.ltm"
#' * "log.oc_iso.10694"
#' * "evi_mod13q1.stl.trend.logit.ols.beta"
#' * "land.cover_esacci.lc.l4"
#' * "evi_mod13q1.tmwm.inpaint"
#' * "dtm.bareearth_ensemble"
#' * "fapar_essd.lstm"
#' * "fapar_essd.lstm.p95.beta"
#' * "pot.fapar_fapar.p95.eml.m"
#' * "pot.fapar_fapar.p95.eml"
#' * "snow.cover_esa.modis"
#' * "snow.cover_esa.modis.ltm"
#' * "wilderness_li2022.human.footprint"
#' * "wv_mcd19a2v061.seasconv"
#' * "wv_mcd19a2v061.seasconv.m_p50"
#' * "wv_mcd19a2v061.seasconv.m_p25"
#' * "wv_mcd19a2v061.seasconv.m_p75"
#' * "wv_mcd19a2v061.seasconv.m_std"
#' * "wv_mcd19a2v061.seasconv.m.yearly"
#' * "bulkdens.fineearth_usda.4a1h"
#' * "geom_merit.dem"
#' * "fapar_proba.v"
#' * "forest.cover_esacci.ifl"
#' * "grtgroup_usda.soiltax"
#' * "land.cover_copernicus"
#' * "organic.carbon.stock_msa.kgm2"
#' * "organic.carbon_usda.6a1c"
#' * "pft.landcover_esa.cci.lc"
#' * "precipitation_sm2rain.ltm"
#' * "ph.h2o_usda.4c1a2a"
#' * "pop.count_ghs.jrc"
#' * "sand.wfraction_usda.3a1a1a"
#' * "lc_mcd12q1v061.p1"
#' * "texture.class_usda.tt"
#' * "water.occurrence_jrc.surfacewater"
#' * "watercontent.33kPa_usda.4b1c"
#' * "dsm_glo30"
#' * "lc_glad.glcluc"
#' * "lc_glad.glcluc.change"
#' * "landuse.cropland_hyde"
#' * "landuse.pasture_hyde"
#' * "land.use.land.cover_hilda.plus"
#' * "lst_mod11a2.daytime.trend.logit.ols.beta"
#' * "lst_mod11a2.nighttime.trend.logit.ols.beta"
#' * "lst_mod11a2.daytime.annual"
#' * "lst_mod11a2.nighttime.annual"
#' * "lst_mod11a2.daytime"
#' * "lst_mod11a2.nighttime"
#' * "landform_usgs.ecotapestry"
#' * "lithology_usgs.ecotapestry"
#' * "grtgroup_usda.soiltax.hapludalfs"
#' * "biome.type_biome00k"
#' * "biomes_biome6k.tropical.evergreen.broadleaf.forest"
#' * "biomes_biome6k.tropical.evergreen.broadleaf.forest.rcp26"
#' * "biomes_biome6k.tropical.evergreen.broadleaf.forest.rcp45"
#' * "biomes_biome6k.tropical.evergreen.broadleaf.forest.rcp85"
#' * "biomes_biome6k.tropical.savanna"
#' * "biomes_biome6k.tropical.savanna.rcp26"
#' * "biomes_biome6k.tropical.savanna.rcp45"
#' * "biomes_biome6k.tropical.savanna.rcp85"
#' * "lc_glc.fcs30d"
#' * "nightlights.average_viirs.v21"
#' * "nightlights.difference_viirs.v21"
#' * "l2a.gedi"
#' * "fluxnet"
#' * "gbov"
#' * "geowiki.lc"
#' * "geowiki.forest.loss"
#' * "veg.plot"
#' * "obis"
#' * "fapar.eml"
#' @param format character(1). File format to query. Default is "tif".
#' It could be used as a pattern search for the file names.
#' @param directory_to_save character(1). Directory to download files.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Insang Song
#' @note `extdata/openlandmap_assets.rds` contains the available assets in OpenLandMap.
#' Users may want to check the available assets to download data directly.
#' For developers: JSON files should be found at STAC catalog of OpenLandMap when updated.
#' @return NULL; GeoTIFF (.tif) files will be stored in
#' \code{directory_to_save}.
#' @seealso [list_stac_files]
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_hengl2023openlandmap}{amadeus}
#' @examples
#' \dontrun{
#' download_olm(
#'   product = "no2_s5p.l3.trop.tmwm",
#'   format = "tif",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
# nolint end
download_olm <- function(
  product = NULL,
  format = "tif",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)

  #### 3. define measurement data paths

  download_urls <-
    list_stac_files(
      which = product,
      format = format,
      id_only = FALSE
    )
  url_filenames <- strsplit(download_urls, "/", fixed = TRUE)[[1]]
  url_filenames <- sapply(url_filenames, \(x) x[length(x)])

  download_names <- paste0(directory_to_save, url_filenames)
  #### 4. build download command
  download_commands <-
    paste0("wget -e robots=off -np ",
           download_urls,
           " -O ",
           download_names,
           "\n")

  #### 5. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "OLM_queried_",
    product,
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )
  download_sink(commands_txt)
  #### 6. concatenate and print download commands to "..._curl_commands.txt"
  writeLines(download_commands)
  #### 7. finish "..._curl_commands.txt" file
  sink()
  #### 9. download data
  download_run(
    download = download,
    commands_txt = commands_txt,
    remove = remove_command
  )
  message("Requests were processed.\n")
}


#' Retrieve file links from SpatioTemporal Assets Catalog (STAC)
#' @description
#' Read file links from SpatioTemporal Assets Catalog (STAC) JSON file.
#' @note
#' Retrieving URLs may take a while depending on the spatial tiling,
#' temporal resolution, and other assets. Users are encouraged to use
#' `which` parameter to select a specific collection.
#' @param stac_json character(1). Full path of STAC JSON file.
#' @param format character(1). Format of target files. Default is "tif".
#' @param which numeric/character. Index or name of collection to retrieve.
#' @param id_only logical(1). Return collection IDs only.
#' @return character vector of file links.
#' @examples
#' \dontrun{
#' read_stac_json()
#' }
#' @export
#' @keywords auxiliary
#' @author Insang Song
#' @importFrom rstac read_collections
#' @importFrom rstac read_items
#' @importFrom rstac assets_url
#' @importFrom rstac read_stac
#' @importFrom rstac links
list_stac_files <-
  function(
    stac_json =
    "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json",
    format = "tif",
    which = NULL,
    id_only = FALSE
  ) {
    allcollections <-
      rstac::read_stac(
        stac_json
      ) |>
      rstac:::read_collections.catalog()
    collection <- allcollections$collections
    collection_ids <- sapply(collection, \(x) x$id)
    if (id_only) {
      return(collection_ids)
    }

    message(
      sprintf(
        "Names of collections include: %s",
        paste(
          collection_ids, collapse = "\n"
        )
      )
    )

    which <- if (!is.numeric(which)) grep(which, collection_ids) else which
    collection <- if (is.null(which)) collection else collection[which]

    collection_assets <-
      lapply(
        collection,
        \(x) rstac::assets_url(rstac:::read_items.doc_collection(x))
      )
    collection_assets <- unlist(collection_assets)
    list_assets <- grep(sprintf("%s$", format), collection_assets, value = TRUE)

    return(list_assets)
  }


#' Process OpenLandMap data
#' @param path character giving OpenLandMap data path
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @return a `SpatRaster` object
#' @author Insang Song
#' @importFrom terra rast
#' @examples
#' \dontrun{
#' olm <- process_olm(
#'   path = paste0(
#'     "./data/no2_s5p.l3.trop.tmwm.p50_p90_2km_a_",
#'     "20180501_20221130_go_epsg.4326_v20221219.tif"
#'  )
#' )
#' }
#' @export
process_olm <-
  function(
    path = NULL,
    extent = NULL,
    ...
  ) {
    # check inputs
    if (!is.character(path) || is.null(path)) {
      stop("path is not a character.")
    }
    olm <- terra::rast(path, win = extent)
    return(olm)
  }
# nocov end
