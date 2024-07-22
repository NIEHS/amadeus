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
#' - [`process_modis_swath`]: `"modis_swath"`
#' - [`process_modis_merge`]: `"modis_merge"`
#' - [`process_blackmarble`]: `"blackmarble"`
#' - [`process_koppen_geiger`]: `"koppen-geiger"`, `"koeppen-geiger"`, `"koppen"`
#' - [`process_ecoregion`]: `"ecoregion"`, `"ecoregions"`
#' - [`process_nlcd`]: `"nlcd"`
#' - [`process_tri`]: `"tri"`
#' - [`process_nei`]: `"nei"`
#' - [`process_geos`]: `"geos"`
#' - [`process_gmted`]: `"gmted"`
#' - [`process_aqs`]: `"aqs"`
#' - [`process_hms`]: `"hms"`, `"smoke"`
#' - [`process_narr`]: `"narr"`
#' - [`process_sedac_groads`]: `"sedac_groads"`, `"roads"`, `"groads"`
#' - [`process_sedac_population`]: `"sedac_population"`, `"population"`
#' - [`process_merra2`]: `"merra"`, `"merra2"`
#' - [`process_gridmet`]: `"gridmet"`, `"gridMET`"
#' - [`process_terraclimate`]: `"terraclimate"`, `"TerraClimate"`
#' - [`process_huc`]: `"huc"`
#' - [`process_cropscape`]: `"cropscape"`, `"cdl"`
#' - [`process_prism`]: `"prism"`
#' - [`process_olm`]: `"olm"`, `"openlandmap"`
#' @returns `SpatVector`, `SpatRaster`, `sf`, or `character` depending on
#' covariate type and selections.
#' @author Insang Song
#' @examples
#' \dontrun{
#' narr <- process_covariates(
#'   covariate = "narr",
#'   date = c("2023-01-01", "2023-01-10"),
#'   variable = "weasd",
#'   path = "./data/weasd"
#' )
#' }
#' @export
# nolint end
process_covariates <-
  function(
    covariate = c("modis_swath", "modis_merge",
                  "koppen-geiger",
                  "blackmarble",
                  "koeppen-geiger", "koppen", "koeppen",
                  "geos", "dummies", "gmted",
                  "hms", "smoke",
                  "sedac_population", "population",
                  "sedac_groads", "groads", "roads",
                  "nlcd", "tri", "narr", "nei",
                  "ecoregions", "ecoregion",
                  "merra", "merra2", "gridmet", "terraclimate",
                  "huc", "cropscape", "cdl", "prism", "olm", "openlandmap"),
    path = NULL,
    ...
  ) {
    covariate <- tolower(covariate)
    covariate <- match.arg(covariate)
    if (startsWith(covariate, "ko")) {
      covariate <- "koppen"
    }

    # select function to run
    what_to_run <- switch(covariate,
      modis_merge = process_modis_merge,
      modis_swath = process_modis_swath,
      blackmarble = process_blackmarble,
      ecoregion = process_ecoregion,
      ecoregions = process_ecoregion,
      koppen = process_koppen_geiger,
      narr = process_narr,
      nlcd = process_nlcd,
      smoke = process_hms,
      hms = process_hms,
      sedac_groads = process_sedac_groads,
      roads = process_sedac_groads,
      groads = process_sedac_groads,
      sedac_population = process_sedac_population,
      population = process_sedac_population,
      nei = process_nei,
      tri = process_tri,
      geos = process_geos,
      gmted = process_gmted,
      merra = process_merra2,
      merra2 = process_merra2,
      gridmet = process_gridmet,
      terraclimate = process_terraclimate,
      huc = process_huc,
      cropscape = process_cropscape,
      cdl = process_cropscape,
      prism = process_prism,
      olm = process_olm,
      openlandmap = process_olm
    )

    res_covariate <-
      tryCatch({
        what_to_run(
          path = path,
          ...
        )
      }, error = function(e) {
        print(e)
        print(args(what_to_run))
        stop(
          paste0(
            "Please refer to the argument list and the error message above to ",
            "rectify the error.\n"
          )
        )
      })

    return(res_covariate)
  }

# nolint start
#' Process MODIS sub-datasets
#' @description
#' Selected MODIS sinusoidal grid product subdataset name selector.
#' Four presets are supported. `custom_sel` supersedes
#' presets of `product` values.
#' @param product character(1). Product code.
#' @param custom_sel character(1). Custom filter.
#' If this value is not NULL, preset filter is
#' overridden.
#' @param ... Placeholders.
#' @note
#' Preset product codes and associated variables include
#' * "MOD11A1" - Land surface temperature (LST)
#' * "MOD13A2" - Normalized Difference Vegetation Index (NDVI)
#' * "MOD09GA" - Surface reflectance, and
#' * "MCD19A2" - Aerosol optical depth (AOD).
#'
#' For a full list of available
#' MODIS product codes, see the "Short Name" column at
#' [NASA LP DAAC Search Data Catalog](https://lpdaac.usgs.gov/product_search/?collections=Combined+MODIS&collections=Terra+MODIS&collections=Aqua+MODIS&view=list).
#' When utilizing a product code from this "Short Name" column, \strong{do
#' not include} the version number following the period. For example, if "Short
#' Name" = MCD12C1.006, then `product = "MCD12C1"`.
# nolint end
#' @author Insang Song
#' @returns A character object that conforms to the regular
#' expression. Details of regular expression in R can be found in [regexp].
#' @seealso [calc_modis_par]
#' @examples
#' process_modis_sds(product = "MOD09GA")
#' @export
# previously modis_prefilter_sds
process_modis_sds <-
  function(
    product = c("MOD11A1", "MOD13A2", "MOD09GA", "MCD19A2"),
    custom_sel = NULL,
    ...
  ) {
    if (!is.null(custom_sel)) {
      modis_sds <- custom_sel
    } else {
      product <- match.arg(product)
      modis_sds <-
        switch(product,
          MOD11A1 = "(LST_)",
          MOD13A2 = "(NDVI)",
          MOD09GA = "(sur_refl_b0)",
          MCD19A2 = "(Optical_Depth)"
        )
      if (product == "MCD19A2") {
        message(
          sprintf(
            "For MCD19A2, use %s for 5km resolution sub-datasets.\n",
            "(cos|RelAZ|Angle)"
          )
        )
      }
    }
    return(modis_sds)
  }


#' Process MODIS layers
#' @description
#' Aggregate layers in a sub-dataset in sinusoidal MODIS products.
#' @param path character(1). Full path to MODIS HDF4/HDF5 file.
#' Direct sub-dataset access is supported, for example,
#' HDF4_EOS:EOS_GRID:\{filename\}:\{base_grid_information\}:\{sub-dataset\}
#' @param subdataset character(1). Exact or regular expression filter of
#' sub-dataset. See [process_modis_sds] for details.
#' @param fun_agg character(1). Function name to aggregate layers.
#' Should be acceptable to [terra::tapp].
#' @param ... Placeholders.
#' @returns a `SpatRaster` object
#' @author Insang Song
#' @seealso [terra::tapp], [terra::rast], [terra::describe]
#' @description Some MODIS products consist of multi-layer subdatasets.
#' This function aggregates multiple layers into single layer `SpatRaster.`
#' `fun_agg` is applied at overlapping cells.
#' @note HDF values are read as original without scaling.
#' Users should consult MODIS product documentation to apply proper
#' scaling factor for post-hoc adjustment. If users have no preliminary
#' information about MODIS sub-datasets, consider running
#' `terra::describe(__filename__, sds = TRUE)` to navigate the full
#' list of sub-datasets in the input file then consult the documentation
#' of MODIS product.
#' @importFrom terra describe
#' @importFrom terra rast
#' @importFrom terra nlyr
#' @importFrom terra tapp
#' @importFrom terra is.rotated
#' @examples
#' \dontrun{
#' mod09ga_flatten <- process_flatten_sds(
#'   path =
#'     list.files("./data", pattern = "MOD09GA.", full.names = TRUE)[1],
#'   subdataset = process_modis_sds("MOD09GA"),
#'   fun_agg = "mean"
#' )
#' }
#' @export
# previously modis_aggregate_sds
process_flatten_sds <-
  function(
    path = NULL,
    subdataset = NULL,
    fun_agg = "mean",
    ...
  ) {
    # if curvilinear, halt
    status_curv <-
      suppressWarnings(terra::is.rotated(terra::rast(path)))
    if (any(status_curv)) {
      stop("The raster is curvilinear. Please rectify or warp
the input then flatten it manually.")
    }

    # describe provides subdataset information
    if (!any(grepl(":", path))) {
      # we use var to get detailed information in subdatasets
      sds_desc <- terra::describe(path, sds = TRUE)
      index_sds <- grep(subdataset, sds_desc$var)
      sds_desc <- sds_desc[index_sds, c("name", "var", "nlyr")]
      # raw is TRUE to ignore scaling factor.
      sds_read <- terra::rast(path, subds = index_sds, raw = TRUE)
      sds_nsds <- nrow(sds_desc)
      sds_nlyr <- sds_desc$nlyr
      sds_varn <- sds_desc$var
    } else {
      sds_read <- terra::rast(path, raw = TRUE)
      sds_nsds <- 1L
      sds_nlyr <- terra::nlyr(sds_read)
      sds_varn <- names(sds_read)
    }
    if (all(sds_nlyr == 1L)) {
      sds_agg <- sds_read
    } else {
      sds_aggindex <- rep(seq_len(sds_nsds), times = sds_nlyr)
      # if there are multiple layers in a subdataset,
      # aggregate overlapping pixel values
      sds_agg <-
        terra::tapp(sds_read,
                    index = sds_aggindex,
                    fun = fun_agg,
                    na.rm = TRUE)
    }
    # restore names
    names(sds_agg) <- sds_varn
    gc()
    return(sds_agg)
  }


# nolint start
#' Process MODIS .hdf files
#' @description
#' Get mosaicked or merged raster from multiple MODIS hdf files.
#' @param path character. Full list of hdf file paths.
#'  preferably a recursive search result from [`base::list.files`].
#' @param date character(1). date to query. Should be in
#' `"YYYY-MM-DD"` format.
#' @param subdataset character(1). subdataset names to extract.
#' Should conform to regular expression. See [`base::regex`] for details.
#' Default is `NULL`, which will result in errors. Users should specify
#' which subdatasets will be imported.
#' @param fun_agg Function name or custom function to aggregate overlapping
#' cell values. See `fun` description in [`terra::tapp`] for details.
#' @param ... For internal use.
#' @note Curvilinear products (i.e., swaths) will not be accepted.
#' MODIS products downloaded by functions in `amadeus`,
#' [MODISTools](https://cran.r-project.org/web/packages/MODISTools/index.html),
#' and [luna](https://github.com/rspatial/luna) are accepted.
#' @seealso [`download_data`]
#' @author Insang Song
#' @returns a `SpatRaster` object
#' @examples
#' \dontrun{
#' mod09ga_merge <- process_modis_merge(
#'   path =
#'     list.files("./data", pattern = "MOD09GA.", full.names = TRUE),
#'   date = "2024-01-01",
#'   subdataset = "sur_refl_b01_1",
#'   fun_agg = "mean"
#' )
#' }
#' @export
# nolint end
# previously modis_get_vrt
process_modis_merge <- function(
    path = NULL,
    date = NULL,
    subdataset = NULL,
    fun_agg = "mean",
    ...) {

  if (!is.character(path)) {
    stop("Argument path should be a list of hdf files (character).\n")
  }
  if (!(is.character(fun_agg) || is.function(fun_agg))) {
    stop("Argument fun_agg should be a function or name of a function
         that is accepted in terra::tapp.\n")
  }
  # date format check
  is_date_proper(instr = date)

  # interpret date
  today <- as.character(date)
  dayjul <- strftime(today, "%Y%j")
  ftarget <- grep(sprintf("A%s", dayjul), path, value = TRUE)

  # get layer information
  layer_target <-
    lapply(ftarget,
           function(x) {
             process_flatten_sds(
               x,
               subdataset = subdataset,
               fun_agg = fun_agg
             )
           })
  # Merge multiple rasters into one
  # do.call(f, l) is equivalent to f(l[[1]], ... , l[[length(l)]])
  if (length(path) > 1) {
    result_merged <- do.call(terra::merge, layer_target)
    gc()
  } else {
    result_merged <- layer_target[[1]]
  }
  return(result_merged)
}


# nolint start
#' Process Black Marble corners
#' @description
#' Tile corner generator for Black Marble products.
#' @param hrange integer(2). Both should be in 0-35.
#' @param vrange integer(2). Both should be in 0-17.
#' @description Black Marble products are in HDF5 format and are read without
#' georeference with typical R geospatial packages.
#' This function generates a `data.frame` of corner coordinates for assignment.
#' @returns `data.frame` with xmin, xmax, ymin, and ymax fields
#' @author Insang Song
#' @references
#' - [Wang, Z. (2022). Black Marble User Guide (Version 1.3). NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)
#' @examples
#' process_blackmarble_corners(hrange = c(1, 2), vrange = c(1, 2))
#' @export
# nolint end
process_blackmarble_corners <-
  function(
    hrange = c(5, 11),
    vrange = c(3, 6)
  ) {
    # should be in range
    if (!all(hrange %in% seq(0, 35)) || !all(vrange %in% seq(0, 17))) {
      stop("hrange or vrange are out of range.")
    }
    # in case range is put in reverse order
    hrange <- sort(hrange)
    vrange <- sort(vrange)

    hseq <- seq(hrange[1], hrange[2])
    vseq <- seq(vrange[1], vrange[2])

    tile_df <-
      expand.grid(
        vaddr = sprintf("v%02d", vseq),
        haddr = sprintf("h%02d", hseq)
      )
    hrangec <- -180 + (hseq * 10L)
    vrangec <- 90 - (vseq * 10L)

    hlen <- abs(diff(hrange)) + 1
    vlen <- abs(diff(vrange)) + 1

    tile_df$tile <- paste0(tile_df$haddr, tile_df$vaddr)
    tile_df <- data.frame(tile = tile_df$tile)
    #return(tile_df)
    tile_df$xmin <- rep(hrangec, each = vlen)
    tile_df$xmax <- tile_df$xmin + 10L
    tile_df$ymin <- rep(vrangec, hlen) - 10L
    tile_df$ymax <- tile_df$ymin + 10L

    return(tile_df)
  }


# nolint start
#' Assign VIIRS Black Marble products corner coordinates to retrieve a merged raster
#' @description This function will return a `SpatRaster` object with
#' georeferenced h5 files of Black Marble product. Referencing corner coordinates
#' are necessary as the original h5 data do not include such information.
#' @param path character. Full paths of h5 files.
#' @param date character(1). Date to query.
#' @param tile_df data.frame. Contains four corner coordinates in fields named
#' `c("xmin", "xmax", "ymin", "ymax")`.
#' See [`process_blackmarble_corners`] to generate a valid object for this argument.
#' @param subdataset integer(1). Subdataset number to process.
#' Default is 3L.
#' @param crs character(1). terra::crs compatible CRS.
#' Default is `"EPSG:4326"`
#' @param ... For internal use.
#' @returns a `SpatRaster` object
#' @author Insang Song
#' @seealso
#' * [`terra::describe`]
#' * [`terra::merge`]
#' * [`process_blackmarble_corners`]
#' @references
#' - [Wang, Z. (2022). Black Marble User Guide (Version 1.3). NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)
#' @importFrom terra rast
#' @importFrom terra ext
#' @importFrom terra crs
#' @importFrom terra merge
#' @examples
#' \dontrun{
#' vnp46a2 <- process_blackmarble(
#'   path =
#'     list.files("./data", pattern = "VNP46A2.", full.names = TRUE),
#'   date = "2024-01-01",
#'   tile_df =
#'     process_blackmarble_corners(hrange = c(8, 10), vrange = c(4, 5))
#'   subdataset = 3L,
#'   crs = "EPSG:4326"
#' )
#' }
#' @export
# previously modis_preprocess_vnp46
# nolint end
process_blackmarble <- function(
  path = NULL,
  date = NULL,
  tile_df = process_blackmarble_corners(),
  subdataset = 3L,
  crs = "EPSG:4326",
  ...
) {
  is_date_proper(instr = date)
  # interpret date from paths
  datejul <- strftime(as.Date(date), format = "%Y%j")
  stdtile <- tile_df$tile

  filepaths_today <- grep(sprintf("A%s", datejul), path, value = TRUE)
  # today's filenames
  filepaths_today <-
    grep(
      paste0(
        "(", paste(stdtile, collapse = "|"), ")"
      ),
      filepaths_today, value = TRUE
    )

  filepaths_today_tiles <-
    regmatches(filepaths_today,
               regexpr("h([0-2][0-9]|[3][0-6])v([0-1][0-9])", filepaths_today))

  vnp_today <- unname(split(filepaths_today, filepaths_today))
  filepaths_today_tiles_list <-
    unname(split(filepaths_today_tiles, filepaths_today_tiles))
  # for filenames,
  # assign corner coordinates then merge
  # Subdataset 3 is BRDF-corrected nighttime light
  vnp_assigned <-
    mapply(function(vnp, tile_in) {
      vnp_ <- terra::rast(vnp, subds = subdataset)
      tile_ext <- tile_df[tile_df$tile == tile_in, -1]

      terra::crs(vnp_) <- terra::crs(crs)
      terra::ext(vnp_) <- unlist(tile_ext)
      return(vnp_)
    }, vnp_today, filepaths_today_tiles_list, SIMPLIFY = FALSE)
  if (length(filepaths_today) > 1) {
    vnp_all <- do.call(terra::merge, vnp_assigned)
  } else {
    vnp_all <- vnp_assigned[[1]]
  }
  vnp_all[vnp_all == 65535L] <- NaN
  vnp_all[is.nan(vnp_all)] <- NA
  return(vnp_all)
}

#' Warp MODIS swath data into rectilinear grid raster
#' @description Swath data is a type of MODIS data,
#' where curvilinear points are stored with varying resolution depending on
#' the relative position of the sensor axis. As this type of data
#' typically does not work well with planar spatial data, users
#' should warp or rectify this data into a rectilinear raster.
#' Main procedure is done with [`stars::st_warp`], in which users are able to
#' customize the threshold to fill potential gaps that appear where
#' the target resolution is finer than the local resolution of curvilinear
#' grid points.
#' @param path File path of MODIS swath with exact sub-dataset specification.
#' @param cellsize numeric(1). Cell size (spatial resolution) of
#' output rectilinear grid raster.
#' @param threshold numeric(1). Maximum distance to fill gaps if occur.
#' @param crs integer(1)/character(1). Coordinate system definition.
#' Should be compatible with EPSG codes or WKT2.
#' See [`terra::crs`] and [`sf::st_crs`] / [EPSG](https://www.epsg.io)
#' @param ... For internal use.
#' @note This function handles one file at a time.
#' @returns a `stars` object
#' @author Insang Song
#' @seealso [`terra::rectify`]
#' @importFrom stars st_warp
#' @importFrom stars read_stars
#' @examples
#' \dontrun{
#' mod06l2_warp <- process_modis_warp(
#'   path = paste0(
#'     "HDF4_EOS:EOS_SWATH:",
#'     list.files(
#'       "./data/mod06l2",
#'       full.names = TRUE,
#'       pattern = ".hdf"
#'     )[1],
#'     ":mod06:Cloud_Fraction"
#'   ),
#'   cellsize = 0.1,
#'   threshold = 0.4,
#'   crs = 4326
#' )
#' }
#' @export
# previously modis_warp_stars
process_modis_warp <-
  function(
    path = NULL,
    cellsize = 0.1,
    threshold = cellsize * 4,
    crs = 4326,
    ...
  ) {
    options(sf_use_s2 = FALSE)
    ras <- stars::read_stars(path)
    rtd <-
      stars::st_warp(
        ras,
        crs = crs,
        segments = 500,
        cellsize = cellsize,
        threshold = threshold
      )
    return(rtd)
  }

# nolint start
#' Mosaic MODIS swaths
#' @description This function will return a `SpatRaster` object with
#' values of selected subdatasets. Swath data include curvilinear
#' grids, which require warping/rectifying the original curvilinear grids
#' into rectilinear grids. The function internally warps each of inputs
#' then mosaic the warped images into one large `SpatRaster` object.
#' Users need to select a subdataset to process. The full path looks like
#' `"HDF4_EOS:EOS_SWATH:{file_path}:mod06:subdataset"`, where file_path is
#' the full path to the hdf file.
#' @param path character. Full paths of hdf files.
#' @param date character(1). Date to query.
#' @param subdataset character. Subdatasets to process.
#' __Unlike other preprocessing functions, this argument should specify
#' the exact subdataset name.__ For example, when using MOD06_L2 product,
#' one may specify `c("Cloud_Fraction", "Cloud_Optical_Thickness")`,
#' etc. The subdataset names can be found in `terra::describe()` output.
#' @param suffix character(1). Should be formatted `:{product}:`,
#' e.g., `:mod06:`
#' @param resolution numeric(1). Resolution of output raster.
#' Unit is degree (decimal degree in WGS84).
#' @param ... For internal use.
#' @seealso
#' * [`process_modis_warp()`], [`stars::read_stars()`], [`stars::st_warp()`]
#' * [GDAL HDF4 driver documentation](https://gdal.org/drivers/raster/hdf4.html)
#' * [`terra::describe()`]: to list the full subdataset list with `sds = TRUE`
#' * [`terra::sprc()`], [`terra::rast()`]
#' @returns
#' * a `SpatRaster` object (crs = `"EPSG:4326"`): if `path` is a single file with
#' full specification of subdataset.
#' * a `SpatRaster` object (crs = `"EPSG:4326"`): if `path` is a list of files. In this case, the returned object will have the maximal extent of multiple warped layers
#' @author Insang Song
#' @importFrom terra rast
#' @importFrom terra crop
#' @importFrom terra ext
#' @importFrom terra mosaic
#' @importFrom stars st_mosaic
#' @importFrom terra values
#' @importFrom terra sprc
#' @examples
#' \dontrun{
#' mod06l2_swath <- process_modis_swath(
#'   path = list.files(
#'     "./data/mod06l2",
#'     full.names = TRUE,
#'     pattern = ".hdf"
#'   ),
#'   date = "2024-01-01",
#'   subdataset = "Cloud_Fraction",
#'   suffix = ":mod06:",
#'   resolution = 0.05
#' )
#' }
#' @export
# nolint end
# previously modis_mosaic_mod06
process_modis_swath <-
  function(
    path = NULL,
    date = NULL,
    subdataset = NULL,
    suffix = ":mod06:",
    resolution = 0.05,
    ...
  ) {
    # check date format
    is_date_proper(instr = date)
    header <- "HDF4_EOS:EOS_SWATH:"
    ras_mod06 <- vector("list", length = length(subdataset))
    datejul <- strftime(date, format = "%Y%j")
    ## FIXME: this part may result in underperformance.
    ##        Find a way to optimize this part.
    paths_today <- grep(sprintf("A%s", datejul), path, value = TRUE)

    # if two or more paths are put in,
    # these are read into a list then mosaicked
    for (element in seq_along(subdataset)) {
      target_text <-
        sprintf("%s%s%s%s", header, paths_today, suffix, subdataset[element])
      # rectified stars objects to SpatRaster
      mod06_element <- split(target_text, target_text) |>
        lapply(process_modis_warp, cellsize = resolution)
      # Remove all NA layers to avoid erroneous values
      mod06_element_nas <-
        sapply(
          mod06_element,
          function(x) {
            xvals <- x[[subdataset[element]]]
            all(is.na(xvals)) || all(is.nan(xvals))
          }
        )
      mod06_element <-
        mod06_element[!mod06_element_nas & !is.null(mod06_element_nas)]

      # prepare a fail-safe alternative return
      # It will be used again later.
      alt <- terra::rast(
        xmin = -128,
        xmax = -64,
        ymin = 20,
        ymax = 52,
        resolution = resolution
      )
      # initialize values with NA
      alt[] <- NA
      alt_dim <- dim(alt)
      alt[1, 1] <- 0
      alt[1, alt_dim[2]] <- 0
      alt[alt_dim[1], 1] <- 0
      alt[alt_dim[1], alt_dim[2]] <- 0
      terra::crs(alt) <- "EPSG:4326"

      if (is.null(mod06_element) || length(mod06_element) == 0) {
        message("All layers are NA or NaN.")
        mod06_element_mosaic <- terra::deepcopy(alt)
      } else {
        # mosaick the warped SpatRasters into one
        mod06_element_mosaic <-
          do.call(stars::st_mosaic, mod06_element) |>
          terra::rast()
        # assigning variable name
        mod06_element_mosaic <-
          terra::crop(mod06_element_mosaic, terra::ext(alt))
      }
      names(mod06_element_mosaic) <- subdataset[element]
      ras_mod06[[element]] <- mod06_element_mosaic
    }
    # SpatRasterCollection can accommodate multiple SpatRasters
    # with different extents (most flexible kind)
    mod06_sprc <- terra::sprc(ras_mod06)
    # post-hoc: stack multiple layers with different extent
    # into one SpatRaster
    # 1. mosaic all layers into one
    mod06_mosaic <- terra::mosaic(mod06_sprc, fun = "median")
    # 2. Assign NAs to prepare "etching"; NA will result in NaNs
    terra::values(mod06_mosaic) <- NA
    # 3. Looping main "etching"; each element is put first
    mod06_etched <-
      sapply(mod06_sprc, terra::mosaic, y = mod06_mosaic, fun = "first")
    # 4. stack
    mod06_return <- do.call(c, mod06_etched)
    return(mod06_return)
  }


#' Process climate classification data
#' @description
#' The \code{process_koppen_geiger()} function imports and cleans raw climate
#' classification data, returning a single `SpatRaster` object.
#' @param path character(1). Path to Koppen-Geiger
#'  climate zone raster file
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @returns a `SpatRaster` object
#' @author Insang Song
#' @importFrom terra rast
#' @examples
#' \dontrun{
#' kg <- process_koppen_geiger(
#'   path = "./data/koppen_geiger_data.tif"
#' )
#' }
#' @export
process_koppen_geiger <-
  function(
    path = NULL,
    extent = NULL,
    ...
  ) {
    # import data
    kg_rast <- terra::rast(path, win = extent)
    # identify time period
    period <- strsplit(
      names(kg_rast),
      "_"
    )[[1]][4]
    if (period == "present") {
      terra::metags(kg_rast) <- c(year = "1980 - 2016")
    } else {
      terra::metags(kg_rast) <- c(year = "2071 - 2100")
    }
    return(kg_rast)
  }


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
#' @returns a `SpatRaster` object
#' @author Eva Marques, Insang Song
#' @importFrom utils read.csv
#' @importFrom tools file_path_sans_ext
#' @importFrom terra rast metags
#' @examples
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
    # open nlcd file corresponding to the year
    nlcd_file <-
      list.files(
        path,
        pattern = paste0("nlcd_", year, "_.*.(tif|img)$"),
        full.names = TRUE
      )
    # check if name without extension is duplicated
    nlcd_file_base <- basename(nlcd_file)
    nlcd_file_base <- tools::file_path_sans_ext(nlcd_file_base)
    if (any(duplicated(nlcd_file_base))) {
      stop("Duplicated NLCD files are detected. Please remove duplicates.")
    }
    if (length(nlcd_file) == 0) {
      stop("NLCD data not available for this year.")
    }
    nlcd <- terra::rast(nlcd_file, win = extent)
    terra::metags(nlcd) <- c(year = year)
    return(nlcd)
  }


#' Process ecoregion data
#' @description
#' The [`process_ecoregion`] function imports and cleans raw ecoregion
#' data, returning a `SpatVector` object.
#' @param path character(1). Path to Ecoregion Shapefiles
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note The function will fix Tukey's bridge in Portland, ME.
#' This fix will ensure that the EPA air quality monitoring sites
#' will be located within the ecoregion.
#' @author Insang Song
#' @returns a `SpatVector` object
#' @importFrom terra vect
#' @importFrom sf st_read st_crs st_as_sfc st_transform st_intersects st_union
#' @importFrom data.table year
#' @examples
#' \dontrun{
#' ecoregion <- process_ecoregion(
#'   path = "./data/epa_ecoregion.gpkg"
#' )
#' }
#' @export
process_ecoregion <-
  function(
    path = NULL,
    extent = NULL,
    ...
  ) {
    ecoreg <- sf::st_read(path)
    # fix Tukey's bridge in Portland, ME
    # nolint start
    poly_tukey <-
      "POLYGON ((-70.258 43.68, -70.2555 43.68, -70.255 43.6733, -70.2576 43.6732, -70.258 43.68))"
    poly_tukey <- sf::st_as_sfc(poly_tukey, crs = "EPSG:4326")
    poly_tukey <- sf::st_transform(poly_tukey, sf::st_crs(ecoreg))

    # nolint end
    ecoreg <- ecoreg[, grepl("^(L2_KEY|L3_KEY)", names(ecoreg))]
    ecoreg_edit_idx <- sf::st_intersects(ecoreg, poly_tukey, sparse = FALSE)
    ecoreg_edit_idx <- vapply(ecoreg_edit_idx, function(x) any(x), logical(1))
    if (!all(ecoreg_edit_idx == 0)) {
      ecoreg_else <- ecoreg[!ecoreg_edit_idx, ]
      ecoreg_edit <- sf::st_union(ecoreg[ecoreg_edit_idx, ], poly_tukey)
      ecoreg <- rbind(ecoreg_else, ecoreg_edit)
    }
    ecoreg$time <- paste0(
      "1997 - ", data.table::year(Sys.time())
    )
    ecoreg <- terra::vect(ecoreg)
    if (!is.null(extent)) {
      ecoreg_crop <- terra::crop(ecoreg, extent)
      return(ecoreg_crop)
    } else {
      return(ecoreg)
    }
  }


# nolint start
#' Process toxic release data
#' @description
#' This function imports and cleans raw toxic release data,
#' returning a single `SpatVector` (points) object for the selected `year`.
#' @param path character(1). Path to the directory with TRI CSV files
#' @param year integer(1). Single year to select.
#' @param variables integer. Column index of TRI data.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @author Insang Song, Mariana Kassien
#' @returns a `SpatVector` object (points) in `year`
#' `year` is stored in a field named `"year"`.
#' @note Visit [TRI Data and Tools](https://www.epa.gov/toxics-release-inventory-tri-program/tri-data-and-tools)
#' to view the available years and variables.
#' @references
#' https://www.epa.gov/toxics-release-inventory-tri-program/tri-data-and-tools
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
#' \dontrun{
#' tri <- process_tri(
#'   path = "./data",
#'   year = 2020,
#'   variables = c(1, 13, 12, 14, 20, 34, 36, 47, 48, 49)
#' )
#' }
# nolint end
#' @export
process_tri <- function(
  path = NULL,
  year = 2018,
  variables = c(1, 13, 12, 14, 20, 34, 36, 47, 48, 49),
  extent = NULL,
  ...
) {

  csvs_tri_from <-
    list.files(path = path, pattern = "*.csv$", full.names = TRUE)
  csvs_tri <- lapply(csvs_tri_from, read.csv)
  col_sel <- variables
  csvs_tri <- data.table::rbindlist(csvs_tri)
  dt_tri <- csvs_tri[, col_sel, with = FALSE]

  # column name readjustment
  tri_cns <- colnames(dt_tri)
  tri_cns <- sub(".*?\\.\\.", "", tri_cns)
  tri_cns <- sub("^[^A-Za-z]*", "", tri_cns)
  tri_cns <- gsub("\\.", "_", tri_cns)
  dt_tri <- stats::setNames(dt_tri, tri_cns)
  dt_tri <- dt_tri[dt_tri$YEAR == year, ]

  # depending on the way the chemicals are summarized
  # Unit is kilogram
  # nolint start
  YEAR <- NULL
  LONGITUDE <- NULL
  LATITUDE <- NULL
  TRI_CHEMICAL_COMPOUND_ID <- NULL
   
  dt_tri_x <-
    dt_tri |>
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_AIR"),
        ~ifelse(UNIT_OF_MEASURE == "Pounds", . * (453.592 / 1e3), . / 1e3)
      )
    ) |>
    dplyr::group_by(YEAR, LONGITUDE, LATITUDE, TRI_CHEMICAL_COMPOUND_ID) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::ends_with("_AIR"),
        ~sum(., na.rm = TRUE)
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      values_from = c("FUGITIVE_AIR", "STACK_AIR"),
      names_from = "TRI_CHEMICAL_COMPOUND_ID",
      names_sep = "_"
    ) |>
    dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE))
  names(dt_tri_x) <- sub(" ", "_", names(dt_tri_x))

  spvect_tri <-
    terra::vect(dt_tri_x,
                geom = c("LONGITUDE", "LATITUDE"),
                crs = "EPSG:4269", # all are NAD83
                keepgeom = TRUE)
  attr(spvect_tri, "tri_year") <- year
  if(!is.null(extent)) {
    tri_final <- apply_extent(spvect_tri, extent)
    return(tri_final)
  } else {
    return(spvect_tri)
  }
}
# nolint end


# nolint start
#' Process road emissions data
#' @description
#' The \code{process_nei()} function imports and cleans raw road emissions data,
#' returning a single `SpatVector` object.
#' @param path character(1). Directory with NEI csv files.
#' @param county `SpatVector`/`sf`. County boundaries.
#' @param year integer(1) Year to use. Currently only 2017 or 2020
#' is accepted.
#' @param ... Placeholders.
#' @returns a `SpatVector` object
#' @author Insang Song
#' @note Base files for `county` argument can be downloaded directly from
#' [U.S. Census Bureau](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
#' or by using `tigris` package. This function does not reproject census boundaries.
#' Users should be aware of the coordinate system of census boundary data for
#' other analyses.
#' @description NEI data comprises multiple csv files where emissions of
#' 50+ pollutants are recorded at county level. With raw data files,
#' this function will join a combined table of NEI data and county
#' boundary, then perform a spatial join to target locations.
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom methods is
#' @importFrom data.table .SD
#' @importFrom data.table fread
#' @importFrom data.table rbindlist
#' @examples
#' \dontrun{
#' nei <- process_nei(
#'   path = "./data",
#'   county = system.file("gpkg/nc.gpkg", package = "sf"),
#'   year = 2017
#' )
#' }
#' @export
# nolint end
process_nei <- function(
  path = NULL,
  county = NULL,
  year = c(2017, 2020),
  ...
) {
  if (is.null(county)) {
    stop("county argument is required.")
  }
  if (!methods::is(county, "SpatVector")) {
    county <- try(terra::vect(county))
    if (inherits(county, "try-error")) {
      stop("county is unable to be converted to SpatVector.\n")
    }
  }
  if (!"GEOID" %in% names(county)) {
    stop("county should include a field named \"GEOID\"")
  }
  if (!as.character(year) %in% c("2017", "2020")) {
    stop("year should be one of 2017 or 2020.\n")
  }
  # Concatenate NEI csv files
  csvs_nei <-
    list.files(
      path = path,
      pattern = "*.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
  csvs_nei <- grep(year, csvs_nei, value = TRUE)
  if (is.null(csvs_nei) || length(csvs_nei) == 0) {
    stop("No files found for the year. The file names should include the year")
  }
  csvs_nei <- lapply(csvs_nei, data.table::fread)
  csvs_nei <- data.table::rbindlist(csvs_nei)
  # column name readjustment
  target_nm <- c("fips code", "total emissions", "emissions uom")
  # not grep-ping at once for flexibility
  target_cns <- sapply(target_nm, function(x) grep(x, colnames(csvs_nei)))
  colnames(csvs_nei)[target_cns] <-
    c("geoid", "emissions_total", "unit_measurement")

  # unify unit of measurement
  # TON here is short tonne, which is 2000 lbs.
  csvs_nei$emissions_total_ton <-
    ifelse(
      csvs_nei$unit_measurement == "TON",
      csvs_nei$emissions_total,
      csvs_nei$emissions_total / 2000
    )
  emissions_total_ton <- NULL
  geoid <- NULL
  # yearabbr <- substr(year, 3, 4)
  csvs_nei$geoid <- sprintf("%05d", as.integer(csvs_nei$geoid))
  csvs_nei <-
    csvs_nei[, list(
      TRF_NEINP_0_00000 = sum(emissions_total_ton, na.rm = TRUE)
    ),
    by = geoid]
  csvs_nei$time <- as.integer(year)

  # read county vector
  cnty_geoid_guess <- grep("GEOID", names(county))
  names(county)[cnty_geoid_guess] <- "geoid"
  county$geoid <- sprintf("%05d", as.integer(county$geoid))
  cnty_vect <- merge(county, as.data.frame(csvs_nei), by = "geoid")
  cnty_vect <- cnty_vect[, c("geoid", "time", "TRF_NEINP_0_00000")]
  return(cnty_vect)

}

# nolint start
#' Process U.S. EPA AQS daily CSV data
#' @description
#' The \code{process_aqs()} function cleans and imports raw air quality
#' monitoring sites from pre-generated daily CSV files, returning a single
#' `SpatVector` or `sf` object.
#' `date` is used to filter the raw data read from csv files.
#' Filtered rows are then processed according to `mode` argument.
#' Some sites report multiple measurements per day with and without
#' [exceptional events](https://www.epa.gov/sites/default/files/2016-10/documents/exceptional_events.pdf)
#' the internal procedure of this function keeps "Included" if there
#' are multiple event types per site-time.
#' @param path character(1). Directory path to daily measurement data.
#' @param date character(2). Start and end date.
#'  Should be in `"YYYY-MM-DD"` format and sorted.
#' @param mode character(1). One of
#'   * "date-location" (all dates * all locations)
#'   * "available-data" (date-location pairs with available data)
#'   * "location" (unique locations).
#' @param data_field character(1). Data field to extract.
#' @param return_format character(1). `"terra"` or `"sf"` or `"data.table"`.
#' @param extent numeric(4). Spatial extent of the resulting object.
#'   The order should be `c(xmin, xmax, ymin, ymax)`.
#'   The coordinate system should be WGS84 (EPSG:4326).
#' @param ... Placeholders.
#' @seealso
#' * [`download_aqs()`]
#' * [EPA, n.d., _AQS Parameter Codes_](
#'   https://aqs.epa.gov/aqsweb/documents/codetables/parameters.csv)
#' @returns a SpatVector, sf, or data.table object depending on the `return_format`
#' @importFrom data.table as.data.table
#' @importFrom utils read.csv
#' @importFrom terra vect project
#' @importFrom sf st_as_sf
#' @importFrom dplyr group_by ungroup filter mutate select distinct
#' @note Choose `date` and `mode` values with caution.
#' The function may return a massive data.table depending on the time range,
#' resulting in a long processing time or even a crash if data is too large
#' for your computing environment to process.
#' @examples
#' \dontrun{
#' aqs <- process_aqs(
#'   path = "./data/aqs_daily_example.csv",
#'   date = c("2022-12-01", "2023-01-31"),
#'   mode = "full",
#'   return_format = "terra"
#' )
#' }
#' @export
process_aqs <-
  function(
    path = NULL,
    date = c("2018-01-01", "2022-12-31"),
    mode = c("date-location", "available-data", "location"),
    data_field = "Arithmetic.Mean",
    return_format = c("terra", "sf", "data.table"),
    extent = NULL,
    ...
  ) {
    mode <- match.arg(mode)
    return_format <- match.arg(return_format)
    if (!is.null(date)) {
      date <- try(as.Date(date))
      if (inherits(date, "try-error")) {
        stop("date has invalid format(s). Please check the values.")
      }
      if (length(date) != 2) {
        stop("date should be a character vector of length 2.")
      }
    } else {
      stop("date should be defined.")
    }
    if (length(path) == 1 && dir.exists(path)) {
      path <- list.files(
        path = path,
        pattern = "*.csv$",
        full.names = TRUE
      )
    }
    
    if (length(path) == 0) {
      stop("path does not contain csv files.")
    }
    pathfiles <- lapply(path, read.csv)

    sites <- data.table::rbindlist(pathfiles, fill = TRUE)

    ## get unique sites
    sites$site_id <-
      sprintf("%02d%03d%04d%05d",
              as.integer(sites$State.Code),
              as.integer(sites$County.Code),
              as.integer(sites$Site.Num),
              as.integer(sites$Parameter.Code))

    site_id <- NULL
    Datum <- NULL
    POC <- NULL
    Date.Local <- NULL
    Sample.Duration <- NULL

    date_start <- as.Date(date[1])
    date_end <- as.Date(date[2])
    date_sequence <- seq(date_start, date_end, "day")
    date_sequence <- as.character(date_sequence)

    # select relevant fields only
    sites <- sites |>
      dplyr::as_tibble() |>
      dplyr::filter(as.character(Date.Local) %in% date_sequence) |>
      dplyr::filter(startsWith(Sample.Duration, "24")) |>
      dplyr::group_by(site_id) |>
      dplyr::filter(POC == min(POC)) |>
      dplyr::mutate(time = Date.Local) |>
      dplyr::ungroup()
    col_sel <- c("site_id", "Longitude", "Latitude", "Datum")
    if (mode != "available-data") {
      sites_v <- unique(sites[, col_sel])
    } else {
      col_sel <- append(col_sel, "Event.Type")
      col_sel <- append(col_sel, "time")
      col_sel <- append(col_sel, data_field)
      sites_v <- sites |>
        dplyr::select(dplyr::all_of(col_sel)) |>
        dplyr::distinct()
      # excluding site-time with multiple event types
      # sites_vdup will be "subtracted" from the original sites_v
      sites_vdup <- sites_v |>
        dplyr::group_by(site_id, time) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::filter(Event.Type == "Excluded") |>
        dplyr::ungroup()
      sites_v <-
        dplyr::anti_join(
          sites_v, sites_vdup,
          by = c("site_id", "time", "Event.Type")
        )
    }
    names(sites_v)[2:3] <- c("lon", "lat")
    sites_v <- data.table::as.data.table(sites_v)

    # subset mainland
    sites_v <- sites_v[!grepl("^(02|15|72|78|6|80)", site_id), ]

    # NAD83 to WGS84
    sites_v_nad <-
      sites_v[sites_v$Datum == "NAD83", ]
    sites_v_nad <-
      terra::vect(
        sites_v_nad,
        keepgeom = TRUE,
        crs = "EPSG:4269"
      )
    sites_v_nad <- terra::project(sites_v_nad, "EPSG:4326")
    # postprocessing: combine WGS84 and new WGS84 records
    sites_v_nad <- as.data.frame(sites_v_nad)
    sites_v_wgs <- sites_v[sites_v$Datum == "WGS84"]
    final_sites <- data.table::rbindlist(
      list(sites_v_wgs, sites_v_nad), fill = TRUE)
    final_sites <-
      final_sites[, grep("Datum", names(final_sites), invert = TRUE), with = FALSE]

    if (mode == "date-location") {
      final_sites <-
        split(date_sequence, date_sequence) |>
        lapply(function(x) {
          fs_time <- final_sites
          fs_time$time <- x
          return(fs_time)
        })
      final_sites <- data.table::rbindlist(final_sites, fill = TRUE)
    }
    if (mode == "available-data") {
      final_sites <- unique(final_sites)
    }

    final_sites <-
      switch(
        return_format,
        terra =
        terra::vect(
          final_sites,
          keepgeom = TRUE,
          crs = "EPSG:4326"
        ),
        sf =
        sf::st_as_sf(
          final_sites,
          remove = FALSE,
          dim = "XY",
          coords = c("lon", "lat"),
          crs = "EPSG:4326"
        ),
        data.table = final_sites
      )
    if (!is.null(extent)) {
      if (return_format == "data.table") {
        warning("Extent is not applicable for data.table. Returning data.table...\n")
        return(final_sites)
      }
      final_sites <- apply_extent(final_sites, extent)
    }

    return(final_sites)
  }


#' Process population density data
#' @description
#' The \code{process_secac_population()} function imports and cleans raw
#' population density data, returning a single `SpatRaster` object.
#' @param path character(1). Path to GeoTIFF (.tif) or netCDF (.nc) file.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @examples
#' \dontrun{
#' pop <- process_sedac_population(
#'   path = "./data/sedac_population_example.tif"
#' )
#' }
#' @export
# nolint end
process_sedac_population <- function(
    path = NULL,
    extent = NULL,
    ...) {
  if (substr(path, nchar(path) - 2, nchar(path)) == ".nc") {
    message(
      paste0(
        "netCDF functionality for SEDAC data is under construction.\n"
      )
    )
    return()
  }
  #### check for variable
  check_for_null_parameters(mget(ls()))
  #### import data
  data <- terra::rast(path, win = extent)
  #### identify names
  names_raw <- names(data)
  #### create new names
  for (r in seq_along(names_raw)) {
    split1 <- strsplit(
      names_raw[r],
      "_rev11_",
    )[[1]][[2]]
    split2 <- strsplit(
      split1,
      "_"
    )[[1]]
    names(data[[r]]) <- paste0(
      "gpw_v4_population_",
      split2[1],
      "_",
      split2[2],
      "_",
      split2[3]
    )
    message(paste0(
      "Cleaning ",
      process_sedac_codes(
        paste0(
          split2[2],
          "_",
          split2[3]
        ),
        invert = TRUE
      ),
      " population data for year ",
      split2[1],
      "...\n"
    ))
    #### year
    terra::metags(data) <- c(year = split2[1])
  }
  return(data)
}


# nolint start
#' Process roads data
#' @description
#' The \code{process_sedac_groads()} function imports and cleans raw road data,
#' returning a single `SpatVector` object.
#' @param path character(1). Path to geodatabase or shapefiles.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note U.S. context. The returned `SpatVector` object contains a
#' `$description` column to represent the temporal range covered by the
#' dataset. For more information, see <https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1/metadata>.
#' @author Insang Song
#' @returns a `SpatVector` object
#' @importFrom terra vect
#' @examples
#' \dontrun{
#' groads <- process_sedac_groads(
#'   path = "./data/groads_example.shp"
#' )
#' }
#' @export
# nolint end
process_sedac_groads <- function(
    path = NULL,
    extent = NULL,
    ...) {
  #### check for variable
  check_for_null_parameters(mget(ls()))
  if (!grepl("(shp|gdb)$", path)) {
    stop("Input is not in expected format.\n")
  }
  #### import data
  data <- terra::vect(path, extent = extent)
  #### time period
  data$description <- "1980 - 2010"
  return(data)
}


# nolint start
#' Process wildfire smoke data
#' @description
#' The \code{process_hms()} function imports and cleans raw wildfire smoke
#' plume coverage data, returning a single `SpatVector` object. 
#' @param date character(2). length of 10 each.
#' Start/end date of downloaded data.
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
#' @export
process_hms <- function(
    date = c("2018-01-01", "2018-01-01"),
    path = NULL,
    extent = NULL,
    ...) {
  #### directory setup
  path <- download_sanitize_path(path)
  #### check for variable
  check_for_null_parameters(mget(ls()))
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
  dates_of_interest <- generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### dates of interest with hyphen for return in 0 polygon case
  dates_no_polygon <- generate_date_sequence(
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
      data_missing <- data_density
      data_missing$Density <- ""
      data_missing$Date <- ""
      data_return <- rbind(data_return, data_missing)
    } else {
      date <- as.Date(
        substr(data_density$Start[1], 1, 7),
        format = "%Y%j"
      )
      message(paste0(
        "Cleaning smoke data for date ",
        date[1],
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
      sort_index <- na.omit(sort_index)
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

#' Process elevation data
#' @description
#' The \code{process_gmted()} function imports and cleans raw elevation data,
#' returning a single `SpatRaster` object.
#' @param variable vector(1). Vector containing the GMTED statistic first and
#' the resolution second. (Example: variable = c("Breakline Emphasis",
#' "7.5 arc-seconds")).
#' * Statistic options: "Breakline Emphasis", "Systematic Subsample",
#'   "Median Statistic", "Minimum Statistic", "Mean Statistic",
#'   "Maximum Statistic", "Standard Deviation Statistic"
#' * Resolution options: "30 arc-seconds", "15 arc-seconds", "7.5 arc-seconds"
#' @param path character(1). Directory with downloaded GMTED  "*_grd"
#' folder containing .adf files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @author Mitchell Manware
#' @note
#' `SpatRaster` layer name indicates selected variable and resolution, and year
#' of release (2010).
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra varnames
#' @examples
#' \dontrun{
#' gmted <- process_gmted(
#'   variable = c("Breakline Emphasis", "7.5 arc-seconds"),
#'   path = "./data/be75_grd"
#' )
#' }
#' @export
process_gmted <- function(
    variable = NULL,
    path = NULL,
    extent = NULL,
    ...) {
  #### directory setup
  path <- download_sanitize_path(path)
  #### check for variable
  check_for_null_parameters(mget(ls()))
  #### check for length of variable
  if (!(length(variable) == 2)) {
    stop(
      paste0(
        "Please provide a vector with the statistic and resolution.\n"
      )
    )
  }
  #### identify statistic and resolution
  statistic <- variable[1]
  statistic_code <- process_gmted_codes(
    statistic,
    statistic = TRUE,
    invert = FALSE
  )
  resolution <- variable[2]
  resolution_code <- process_gmted_codes(
    resolution,
    resolution = TRUE,
    invert = FALSE
  )
  message(paste0(
    "Cleaning ",
    statistic,
    " data at ",
    resolution,
    " resolution.\n"
  ))
  #### identify file path
  paths <- list.files(
    path,
    full.names = TRUE,
    recursive = TRUE
  )

  #### select only the folder containing data
  data_paths <- unique(
    grep(
      paste0(
        statistic_code,
        resolution_code,
        "_grd",
        collapse = "|"
      ),
      paths,
      value = TRUE
    )
  )
  data_path <- data_paths[grep("(_grd$|w001001.adf)", data_paths)]
  #### import data
  data <- terra::rast(data_path, win = extent)
  #### layer name
  names(data) <- paste0(
    "elevation_",
    gsub(
      "_grd",
      "",
      names(data)
    ),
    "_2010"
  )
  #### varnames
  terra::varnames(data) <- paste0(
    "Elevation: ",
    statistic,
    " (",
    resolution,
    ")"
  )
  #### year
  terra::metags(data) <-
    c(year = 2010L)
  #### set coordinate reference system
  return(data)
}

# nolint start
#' Process meteorological data
#' @description
#' The \code{process_narr()} function imports and cleans raw meteorological
#' data, returning a single `SpatRaster` object.
#' @param date character(2). length of 10 each.
#' Start/end date of downloaded data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). Variable name acronym. See [List of Variables in NARR Files](https://ftp.cpc.ncep.noaa.gov/NARR/fixed/merged_land_AWIP32corrected.pdf)
#' for variable names and acronym codes.
#' @param path character(1). Directory with downloaded netCDF (.nc) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable acronym,
#' pressure level, and date.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra subset
#' @importFrom stringi stri_pad
#' @examples
#' \dontrun{
#' narr <- process_narr(
#'   date = c("2022-01-01", "2022-01-10"),
#'   variable = "weasd",
#'   path = "./data/weasd"
#' )
#' }
#' @export
# nolint end
process_narr <- function(
    date = c("2023-09-01", "2023-09-01"),
    variable = NULL,
    path = NULL,
    extent = NULL,
    ...) {
  #### directory setup
  path <- download_sanitize_path(path)
  #### check for variable
  check_for_null_parameters(mget(ls()))
  #### identify file paths
  data_paths <- list.files(
    path,
    pattern = variable,
    recursive = TRUE,
    full.names = TRUE
  )
  data_paths <- grep(
    sprintf("%s*.*.nc", variable),
    data_paths,
    value = TRUE
  )
  #### define date sequence
  date_sequence <- generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### path ncar
  path_ncar <- nchar(
    strsplit(
      data_paths[1],
      paste0(
        variable,
        "."
      )
    )[[1]][3]
  )
  if (path_ncar == 9) {
    #### define year-months of interest
    ym_of_interest <- unique(
      substr(
        date_sequence,
        1,
        6
      )
    )
  } else if (path_ncar == 7) {
    #### define years of interest
    ym_of_interest <- unique(
      substr(
        date_sequence,
        1,
        4
      )
    )
  }
  #### subset file paths to only dates of interest
  data_paths_ym <- unique(
    grep(
      paste(
        ym_of_interest,
        collapse = "|"
      ),
      data_paths,
      value = TRUE
    )
  )
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths_ym)) {
    #### import data
    data_year <- terra::rast(data_paths_ym[p], win = extent)
    message(paste0(
      "Cleaning ",
      variable,
      " data for ",
      month.name[
        as.numeric(
          substr(
            gsub(
              "-",
              "",
              terra::time(data_year)[1]
            ),
            5,
            6
          )
        )
      ],
      ", ",
      substr(
        gsub(
          "-",
          "",
          terra::time(data_year)[1]
        ),
        1,
        4
      ),
      "...\n"
    ))
    #### check for mono or pressure levels
    if (grepl("level", names(data_year)[1])) {
      #### pressure levels data
      message(paste0("Detected pressure levels data...\n"))
      days <- sapply(
        strsplit(
          names(data_year),
          "_"
        ),
        function(x) x[3]
      )
      terra::time(data_year) <- as.Date(
        paste0(
          substr(
            terra::time(data_year),
            1,
            4
          ),
          substr(
            gsub(
              "-",
              "",
              terra::time(data_year)
            ),
            5,
            6
          ),
          days
        ),
        format = "%Y%m%d"
      )
      names(data_year) <- paste0(
        variable,
        "_",
        sapply(
          strsplit(
            names(data_year),
            "_"
          ),
          function(x) x[2]
        ),
        "_",
        gsub(
          "-",
          "",
          terra::time(data_year)
        )
      )
    } else {
      #### mono level data
      message(paste0("Detected monolevel data...\n"))
      names(data_year) <- paste0(
        variable,
        "_",
        gsub(
          "-",
          "",
          terra::time(data_year)
        )
      )
    }
    data_full <- c(
      data_full,
      data_year,
      warn = FALSE
    )
  }
  #### subset years to dates of interest
  data_return <- terra::subset(
    data_full,
    which(
      substr(
        names(data_full),
        nchar(names(data_full)) - 7,
        nchar(names(data_full))
      ) %in% date_sequence
    )
  )
  message(paste0(
    "Returning daily ",
    variable,
    " data from ",
    as.Date(
      date_sequence[1],
      format = "%Y%m%d"
    ),
    " to ",
    as.Date(
      date_sequence[length(date_sequence)],
      format = "%Y%m%d"
    ),
    ".\n"
  ))
  #### return SpatRaster
  return(data_return)
}

#' Process atmospheric composition data
#' @description
#' The \code{process_geos()} function imports and cleans raw atmospheric
#' composition data, returning a single `SpatRaster` object.
#' @param date character(2). length of 10. Format "YYYY-MM-DD".
#' @param variable character(1). GEOS-CF variable name(s).
#' @param path character(1). Directory with downloaded netCDF (.nc4) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable,
#' pressure level, date, and hour.
#' @author Mitchell Manware
#' @return a `SpatRaster` object;
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra varnames
#' @importFrom terra crs
#' @importFrom terra subset
#' @examples
#' \dontrun{
#' geos <- process_geos(
#'   date = c("2024-01-01", "2024-01-10"),
#'   variable = "O3",
#'   path = "./data/aqc_tavg_1hr_g1440x721_v1"
#' )
#' }
#' @export
process_geos <-
  function(date = c("2018-01-01", "2018-01-01"),
           variable = NULL,
           path = NULL,
           extent = NULL,
           ...) {
    #### directory setup
    path <- download_sanitize_path(path)
    #### check for variable
    check_for_null_parameters(mget(ls()))
    #### identify file paths
    paths <- list.files(
      path,
      pattern = "GEOS-CF.v01.rpl",
      full.names = TRUE
    )
    paths <- paths[grep(
      ".nc4",
      paths
    )]
    #### identify dates based on user input
    dates_of_interest <- generate_date_sequence(
      date[1],
      date[2],
      sub_hyphen = TRUE
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
    #### identify collection
    collection <- process_collection(
      data_paths[1],
      source = "geos",
      collection = TRUE
    )
    message(
      paste0(
        "Identified collection ",
        collection,
        ".\n"
      )
    )
    #### initiate for loop
    data_return <- terra::rast()
    for (p in seq_along(data_paths)) {
      #### import .nc4 data
      data_raw <- terra::rast(data_paths[p])
      data_datetime <- process_collection(
        data_paths[p],
        source = "geos",
        datetime = TRUE
      )
      message(paste0(
        "Cleaning ",
        variable,
        " data for ",
        ISOdate(
          year = substr(data_datetime, 1, 4),
          month = substr(data_datetime, 5, 6),
          day = substr(data_datetime, 7, 8),
          hour = substr(
            data_datetime,
            9,
            10
          ),
          min = substr(
            data_datetime,
            11,
            12
          ),
          sec = 00,
          tz = "UTC"
        ),
        "...\n"
      ))
      #### subset to user-selected variable
      data_variable <- terra::subset(
        data_raw,
        subset = grep(
          variable,
          names(data_raw)
        )
      )
      #### define variable time
      terra::time(data_variable) <- rep(
        ISOdate(
          year = substr(data_datetime, 1, 4),
          month = substr(data_datetime, 5, 6),
          day = substr(data_datetime, 7, 8),
          hour = substr(
            data_datetime,
            9,
            10
          ),
          min = substr(
            data_datetime,
            11,
            12
          ),
          sec = 00,
          tz = "UTC"
        ),
        terra::nlyr(data_variable)
      )
      #### define variable name based on date and time
      names(data_variable) <- paste0(
        names(data_variable),
        "_",
        gsub(
          ":", "",
          gsub(
            "-", "",
            gsub(" ", "_", terra::time(data_variable))
          )
        )
      )
      if (substr(data_datetime, 9, 12) == "0000") {
        names(data_variable) <- paste0(
          names(data_variable),
          "_000000"
        )
      }
      #### combine data with same date
      data_return <- c(
        data_return,
        data_variable,
        warn = FALSE
      )
    }
    #### set coordinate reference system
    terra::crs(data_return) <- "EPSG:4326"
    message(paste0(
      "Returning hourly ",
      variable,
      " data from ",
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
    #### return SpatRaster
    return(data_return)
  }

#' Process meteorological and atmospheric data
#' @description
#' The \code{process_merra2()} function imports and cleans raw atmospheric
#' composition data, returning a single `SpatRaster` object.
#' @param date character(2). length of 10. Format "YYYY-MM-DD".
#' @param variable character(1). MERRA2 variable name(s).
#' @param path character(1). Directory with downloaded netCDF (.nc4) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable,
#' pressure level, date, and hour. Pressure level values utilized for layer
#' names are taken directly from raw data and are not edited to retain
#' pressure level information.
#' @author Mitchell Manware
#' @return a `SpatRaster` object;
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra names
#' @importFrom terra crs
#' @importFrom terra subset
#' @examples
#' \dontrun{
#' merra2 <- process_merra2(
#'   date = c("2024-01-01", "2024-01-10"),
#'   variable = "CPT",
#'   path = "./data/inst1_2d_int_Nx"
#' )
#' }
#' @export
process_merra2 <-
  function(date = c("2018-01-01", "2018-01-01"),
           variable = NULL,
           path = NULL,
           extent = NULL,
           ...) {
    #### directory setup
    path <- download_sanitize_path(path)
    #### check for variable
    check_for_null_parameters(mget(ls()))
    #### identify file paths
    paths <- list.files(
      path,
      pattern = "MERRA2_400",
      full.names = TRUE
    )
    paths <- paths[grep(
      ".nc4",
      paths
    )]
    #### identify dates based on user input
    dates_of_interest <- generate_date_sequence(
      date[1],
      date[2],
      sub_hyphen = TRUE
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
    #### identify collection
    collection <- process_collection(
      data_paths[1],
      source = "merra2",
      collection = TRUE
    )
    message(
      paste0(
        "Identified collection ",
        collection,
        ".\n"
      )
    )
    #### initiate for loop
    data_return <- terra::rast()
    for (p in seq_along(data_paths)) {
      #### import .nc4 data
      data_raw <- terra::rast(data_paths[p], win = extent)
      data_date <- process_collection(
        data_paths[p],
        source = "merra2",
        date = TRUE
      )
      message(
        paste0(
          "Cleaning ",
          variable,
          " data for ",
          as.Date(
            data_date,
            format = "%Y%m%d"
          ),
          "...\n"
        )
      )
      #### subset to user-selected variable
      data_variable <- terra::subset(
        data_raw,
        subset = grep(
          variable,
          names(data_raw)
        )
      )
      #### identify time step
      times <- process_merra2_time(
        collection = collection,
        from = data_variable
      )
      #### identify unique pressure levels
      levels <-
        unique(
          grep(
            "lev=",
            unlist(
              strsplit(names(data_variable), "_")
            ),
            value = TRUE
          )
        )
      #### empty `levels` if 2 dimensional data
      if (length(levels) == 0) {
        levels <- ""
      }
      #### merge levels and times
      leveltimes <- merge(levels, times)
      #### set layer names
      names(data_variable) <- gsub(
        "__",
        "_",
        paste0(
          variable,
          "_",
          leveltimes[, 1],
          "_",
          data_date,
          "_",
          leveltimes[, 2]
        )
      )
      #### set layer times
      terra::time(data_variable) <- ISOdatetime(
        year = substr(data_date, 1, 4),
        month = substr(data_date, 5, 6),
        day = substr(data_date, 7, 8),
        hour = substr(leveltimes[, 2], 1, 2),
        min = substr(leveltimes[, 2], 3, 4),
        sec = substr(leveltimes[, 2], 5, 6),
        tz = "UTC"
      )
      data_return <- c(
        data_return,
        data_variable,
        warn = FALSE
      )
    }
    terra::crs(data_return) <- "EPSG:4267"
    message(paste0(
      "Returning hourly ",
      variable,
      " data from ",
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
    #### return SpatRaster
    return(data_return)
  }


# nolint start
#' Process gridMET data
#' @description
#' The \code{process_gridmet()} function imports and cleans raw gridded surface meteorological
#' data, returning a single `SpatRaster` object.
#' @param date character(2). length of 10 each.
#' Start/end date of downloaded data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). Variable name or acronym code. See [gridMET Generate Wget File](https://www.climatologylab.org/wget-gridmet.html)
#' for variable names and acronym codes. (Note: variable "Burning Index" has code "bi" and variable
#' "Energy Release Component" has code "erc").
#' @param path character(1). Directory with downloaded netCDF (.nc) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable acronym,
#' and date.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra subset
#' @examples
#' \dontrun{
#' gridmet <- process_gridmet(
#'   date = c("2023-01-01", "2023-01-10"),
#'   variable = "Precipitation",
#'   path = "./data/pr"
#' )
#' }
#' @export
# nolint end
process_gridmet <- function(
    date = c("2023-09-01", "2023-09-01"),
    variable = NULL,
    path = NULL,
    extent = NULL,
    ...) {
  #### directory setup
  path <- download_sanitize_path(path)
  #### check for variable
  check_for_null_parameters(mget(ls()))
  variable_checked <- process_variable_codes(
    variables = variable,
    source = "gridmet"
  )
  variable_checked_long <- process_gridmet_codes(
    variable_checked,
    invert = TRUE
  )
  #### identify file paths
  data_paths <- list.files(
    path,
    pattern = variable_checked,
    full.names = TRUE
  )
  data_paths <- data_paths[grep(
    ".nc",
    data_paths
  )]
  #### define date sequence
  date_sequence <- generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### years of interest
  yoi <- unique(
    substr(
      date_sequence,
      1, 4
    )
  )
  #### subset file paths to only dates of interest
  data_paths <- unique(
    grep(
      paste(
        yoi,
        collapse = "|"
      ),
      data_paths,
      value = TRUE
    )
  )
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths)) {
    #### import data
    data_year <- terra::rast(data_paths[p], win = extent)
    message(paste0(
      "Cleaning daily ",
      variable_checked,
      " data for year ",
      gsub(
        ".nc",
        "",
        strsplit(
          data_paths[p],
          paste0(
            variable_checked,
            "_"
          )
        )[[1]][2]
      ),
      "...\n"
    ))
    time_numeric <- sapply(
      strsplit(
        names(data_year),
        "="
      ),
      function(x) as.numeric(x[2]) - 25567
    )
    terra::time(data_year) <- as.Date(time_numeric)
    names(data_year) <- paste0(
      variable_checked,
      "_",
      gsub(
        "-",
        "",
        terra::time(data_year)
      )
    )
    terra::varnames(data_year) <- variable_checked
    terra::longnames(data_year) <- variable_checked_long
    data_full <- c(
      data_full,
      data_year,
      warn = FALSE
    )
  }
  #### subset years to dates of interest
  data_return <- terra::subset(
    data_full,
    which(
      substr(
        names(data_full),
        nchar(names(data_full)) - 7,
        nchar(names(data_full))
      ) %in% date_sequence
    )
  )
  message(paste0(
    "Returning daily ",
    variable_checked_long,
    " data from ",
    as.Date(
      date_sequence[1],
      format = "%Y%m%d"
    ),
    " to ",
    as.Date(
      date_sequence[length(date_sequence)],
      format = "%Y%m%d"
    ),
    ".\n"
  ))
  #### return SpatRaster
  return(data_return)
}

# nolint start
#' Process TerraClimate data
#' @description
#' The \code{process_terraclimate()} function imports and cleans climate and water balance
#' data, returning a single `SpatRaster` object.
#' @param date character(2). length of 10 each.
#' Start/end date of downloaded data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). Variable name or acronym code. See [TerraClimate Direct Downloads](https://climate.northwestknowledge.net/TERRACLIMATE/index_directDownloads.php)
#' for variable names and acronym codes.
#' @param path character(1). Directory with downloaded netCDF (.nc) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable acronym, year,
#' and month.
#' @note
#' TerraClimate data has monthly temporal resolution, so the first day of each month
#' is used as a placeholder temporal value.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra subset
#' @examples
#' \dontrun{
#' terraclimate <- process_terraclimate(
#'   date = c("2023-01-01", "2023-01-10"),
#'   variable = "Precipitation",
#'   path = "./data/ppt"
#' )
#' }
#' @export
# nolint end
process_terraclimate <- function(
    date = c("2023-09-01", "2023-09-01"),
    variable = NULL,
    path = NULL,
    extent = NULL,
    ...) {
  #### directory setup
  path <- download_sanitize_path(path)
  #### check for variable
  check_for_null_parameters(mget(ls()))
  variable_checked <- process_variable_codes(
    variables = variable,
    source = "terraclimate"
  )
  variable_checked_long <- process_terraclimate_codes(
    variable_checked,
    invert = TRUE
  )
  #### identify file paths
  data_paths <- list.files(
    path,
    pattern = variable_checked,
    full.names = TRUE
  )
  data_paths <- data_paths[grep(
    ".nc",
    data_paths
  )]
  #### define date sequence
  date_sequence <- generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### years of interest
  yoi <- unique(
    substr(
      date_sequence,
      1, 4
    )
  )
  #### year-months of interest
  ymoi <- unique(
    substr(
      date_sequence,
      1, 6
    )
  )
  #### subset file paths to only dates of interest
  data_paths <- unique(
    grep(
      paste(
        yoi,
        collapse = "|"
      ),
      data_paths,
      value = TRUE
    )
  )
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths)) {
    #### import data
    data_year <- terra::rast(data_paths[p], win = extent)
    message(paste0(
      "Cleaning monthly ",
      variable_checked_long,
      " data for ",
      substr(
        terra::time(data_year)[1],
        1,
        4
      ),
      "...\n"
    ))
    names(data_year) <- paste0(
      variable_checked,
      "_",
      substr(
        gsub(
          "-",
          "",
          terra::time(data_year)
        ),
        1,
        6
      )
    )
    terra::varnames(data_year) <- variable_checked
    terra::longnames(data_year) <- variable_checked_long
    data_full <- c(
      data_full,
      data_year,
      warn = FALSE
    )
  }
  #### subset years to dates of interest
  data_return <- terra::subset(
    data_full,
    which(
      substr(
        names(data_full),
        nchar(names(data_full)) - 5,
        nchar(names(data_full))
      ) %in% ymoi
    )
  )
  message(paste0(
    "Returning monthly ",
    variable_checked_long,
    " data from ",
    month.name[
      as.numeric(
        substr(
          ymoi[1],
          5,
          6
        )
      )
    ],
    ", ",
    substr(
      ymoi[1],
      1,
      4
    ),
    " to ",
    month.name[
      as.numeric(
        substr(
          ymoi[length(ymoi)],
          5,
          6
        )
      )
    ],
    ", ",
    substr(
      ymoi[length(ymoi)],
      1,
      4
    ),
    ".\n"
  ))
  #### return SpatRaster
  return(data_return)
}



#' Retrieve Hydrologic Unit Code (HUC) data
#' @author Insang Song
#' @param path character. Path to the file or the directory containing HUC data.
#' @param layer_name character(1). Layer name in the `path`
#' @param huc_level character(1). Field name of HUC level
#' @param huc_header character(1). The upper level HUC code header to extract
#'  lower level HUCs.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Arguments passed to `nhdplusTools::get_huc()`
#' @returns a `SpatVector` object
#' @seealso [`nhdplusTools::get_huc`]
#' @importFrom terra vect
#' @importFrom terra vector_layers
#' @importFrom rlang inject
#' @importFrom nhdplusTools get_huc
#' @examples
#' \dontrun{
#' library(terra)
#' getf <- "WBD_National_GDB.gdb"
#' # check the layer name to read
#' terra::vector_layers(getf)
#' test1 <- process_huc(
#'   getf,
#'   layer_name = "WBDHU8",
#'   huc_level = "huc8"
#' )
#' test2 <- process_huc(
#'   getf,
#'   layer_name = "WBDHU8",
#'   huc_level = "huc8"
#' )
#' test3 <- process_huc(
#'   "",
#'   layer_name = NULL,
#'   huc_level = NULL,
#'   huc_header = NULL,
#'   id = "030202",
#'   type = "huc06"
#' )
#' }
#' @export
process_huc <-
  function(
    path,
    layer_name = NULL,
    huc_level = NULL,
    huc_header = NULL,
    extent = NULL,
    ...
  ) {
    # exclude the coverage due to write permission related to memoization
    #nocov start
    if (missing(path) || (!file.exists(path) && !dir.exists(path))) {
      hucpoly <- try(
        rlang::inject(nhdplusTools::get_huc(!!!list(...)))
      )
      if (inherits(hucpoly, "try-error")) {
        stop("HUC data was not found.")
      }
      hucpoly <- terra::vect(hucpoly)
    }
    #nocov end
    if (file.exists(path) || dir.exists(path)) {
      if (!is.null(huc_header)) {
        querybase <-
          sprintf("SELECT * FROM %s WHERE %s LIKE '%s%%'",
                  layer_name, huc_level, huc_header)
      } else {
        querybase <-
          sprintf("SELECT * FROM %s", layer_name)
      }
      if (!layer_name %in% terra::vector_layers(path)) {
        stop(
          paste0(
            "Layer ",
            layer_name,
            " not found in ",
            path
          )
        )
      }

      hucpoly <- try(
        terra::vect(
          path,
          query = querybase,
          extent = extent
        )
      )
    }
    return(hucpoly)
  }



#' Process CropScape data
#' @description
#' This function imports and cleans raw CropScape data,
#' returning a single `SpatRaster` object.
#' @param path character giving CropScape data path
#' @param year numeric giving the year of CropScape data used
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @description Reads CropScape file of selected `year`.
#' @returns a `SpatRaster` object
#' @author Insang Song
#' @importFrom utils read.csv
#' @importFrom terra rast
#' @importFrom terra metags
#' @examples
#' \dontrun{
#' cropscape <- process_cropscape(
#'   path = "./data/cropscape_example.tif",
#'   year = 2020
#' )
#' }
#' @export
process_cropscape <-
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
    if (!is.numeric(year)) {
      stop("year is not a numeric.")
    }
    # open cdl file corresponding to the year
    if (dir.exists(path)) {
      cdl_file <-
        list.files(
          path,
          pattern = paste0("cdl_30m_*.*", year, "_*.*.tif$"),
          full.names = TRUE
        )
    } else {
      cdl_file <- path
    }
    cdl <- terra::rast(cdl_file, win = extent)
    terra::metags(cdl) <- c(year = year)
    return(cdl)
  }


#' Process PRISM data
#' @description
#' This function imports and cleans raw PRISM data,
#' returning a single `SpatRaster` object.
#' @param path character giving PRISM data path
#' Both file and directory path are acceptable.
#' @param element character(1). PRISM element name
#' @param time character(1). PRISM time name.
#' Should be character in length of 2, 4, 6, or 8.
#' "annual" is acceptable.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @description Reads time series or 30-year normal PRISM data.
#' @returns a `SpatRaster` object with metadata of time and element.
#' @seealso [`terra::rast`], [`terra::metags`]
#' @author Insang Song
#' @importFrom utils read.csv
#' @importFrom terra rast
#' @importFrom terra metags
#' @examples
#' \dontrun{
#' prism <- process_prism(
#'   path = "./data/PRISM_ppt_stable_4kmM3_202104_nc.nc",
#'   element = "ppt",
#'   time = "202104"
#' )
#' }
#' @export
# nolint start
process_prism <-
  function(
    path = NULL,
    element = NULL,
    time = NULL,
    extent = NULL,
    ...
  ) {
    # check inputs
    if (!element %in%
          c("ppt", "tmin", "tmax", "tmean", "tdmean",
            "vpdmin", "vpdmax",
            "solslope", "soltotal", "solclear", "soltrans")) {
      stop("element is not a valid PRISM element.")
    }
    if (!is.character(path) || is.null(path)) {
      stop("path is not a character.")
    }
    if (!nchar(time) %in% seq(2, 8, 2)) {
      stop("time does not have valid length.")
    }

    if (dir.exists(path)) {
      pattern <- "PRISM_%s*.*_%s_*.*(bil|nc|grib2|asc)$"
      pattern <- sprintf(pattern, element, time)
      prism_file <-
        list.files(
          path,
          pattern = pattern,
          full.names = TRUE
        )
    } else {
      prism_file <- path
    }
    prism <- terra::rast(prism_file, win = extent)
    terra::metags(prism) <- c(time = time, element = element)
    return(prism)
  }
# nolint end


#' Process OpenLandMap data
#' @param path character giving OpenLandMap data path
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @returns a `SpatRaster` object
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
