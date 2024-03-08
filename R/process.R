# nolint start
#' Process raw data wrapper function
#' @description
#' The \code{process_covariates()} function processes raw data files which have
#' been downloaded by \code{download_data()}. \code{process_covariates()} and
#' the underlying source-specific processing functions have been designed to
#' operate on the raw data files. To avoid errors, \strong{do not edit the raw
#' data files before passing to \code{process_covariates()}}.
#' @param covariate character(1). Covariate type.
#' @param path character(1). Directory or file path to raw data
#' depending on `covariate` value.
#' @param ... Arguments passed to each raw data processing function.
#' @seealso
#' - [`process_modis_swath`]: `"modis_swath"`
#' - [`process_modis_merge`]: `"modis_merge"`
#' - [`process_bluemarble`]: `"bluemarble"`
#' - [`process_koppen_geiger`]: `"koppen-geiger"`, `"koeppen-geiger"`, `"koppen"`,
#' - [`process_ecoregion`]: `"ecoregion"`, `"ecoregions"`
#' - [`process_nlcd`]: `"nlcd"`, `"NLCD"`
#' - [`process_tri`]: `"tri"`, `"TRI"`
#' - [`process_nei`]: `"nei"`, `"NEI`
#' - [`process_geos`]: `"geos"`
#' - [`process_gmted`]: `"gmted"`, `"GMTED"`
#' - [`process_aqs`]: `"aqs"`, `"AQS"`
#' - [`process_hms`]: `"hms"`, `"HMS"`, `"smoke"`
#' - [`process_narr`]: `"narr"`, `"NARR"`
#' - [`process_sedac_groads`]: `"sedac_groads"`, `"roads"`, `"groads"`
#' - [`process_sedac_population`]: `"sedac_population"`, `"population"`
#' @returns `SpatVector`, `SpatRaster`, `sf`, or `character` depending on
#' covariate type and selections.
#' @author Insang Song
#' @export
# nolint end
process_covariates <-
  function(
    covariate = c("modis_swath", "modis_merge",
                  "koppen-geiger",
                  "bluemarble",
                  "koeppen-geiger", "koppen", "koeppen",
                  "geos", "dummies", "gmted",
                  "hms", "smoke",
                  "sedac_population", "population",
                  "sedac_groads", "groads", "roads",
                  "nlcd", "tri", "narr", "nei",
                  "ecoregions", "ecoregion"),
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
      bluemarble = process_bluemarble,
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
      gmted = process_gmted
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
#' @note
#' Preset product codes and associated variables include \code{MOD11A1} -
#' land surface temperature (LST), \code{MOD13A2} - Normalized Difference
#' Vegetation Index (NDVI), \code{MOD09GA} - surface reflectance, and
#' \code{MCD19A2} - aerosol optical depth (AOD). For a full list of available
#' MODIS product codes, see the "Short Name" column at
#' [https://lpdaac.usgs.gov/product_search/?collections=Combined+MODIS&collections=Terra+MODIS&collections=Aqua+MODIS&view=list].
#' When utilizing a product code from this "Short Name" column, \strong{do
#' not include} the version number following the period. For example, if "Short
#' Name" = MCD12C1.006, then \code{product = MCD12C1}.
# nolint end
#' @author Insang Song
#' @returns A character object that conforms to the regular
#' expression. Details of regular expression in R can be found in [regexp].
#' @seealso [calc_modis_par]
#' @export
# previously modis_prefilter_sds
process_modis_sds <-
  function(
    product = c("MOD11A1", "MOD13A2", "MOD09GA", "MCD19A2"),
    custom_sel = NULL
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
#' @returns a SpatRaster object
#' @author Insang Song
#' @seealso [terra::tapp], [terra::rast], [terra::describe]
#' @description Some MODIS products consist of multi-layer subdatasets.
#' This function aggregates multiple layers into single layer SpatRaster.
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
#' @export
# previously modis_aggregate_sds
process_flatten_sds <-
  function(
    path = NULL,
    subdataset = NULL,
    fun_agg = "mean"
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
#'  preferably a recursive search result from \code{list.files}.
#' @param date character(1). date to query. Should be in
#' \code{"YYYY-MM-DD"} format.
#' @param subdataset character(1). subdataset names to extract.
#' Should conform to regular expression. See \link{regex} for details.
#' Default is `NULL`, which will result in errors. Users should specify
#' which subdatasets will be imported.
#' @param fun_agg Function name or custom function to aggregate overlapping
#' cell values. See \code{fun} description in \link[terra]{tapp} for details.
#' @param ... For internal use.
#' @note Curvilinear products (i.e., swaths) will not be accepted.
#' MODIS products downloaded by functions in `amadeus`,
#' [MODISTools](https://cran.r-project.org/web/packages/MODISTools/index.html),
#' and [luna](https://github.com/rspatial/luna) are accepted.
#' @seealso [download_data]
#' @author Insang Song
#' @returns a SpatRaster object
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
#' Process Blue Marble corners
#' @description
#' Tile corner generator for Blue Marble products.
#' @param hrange integer(2). Both should be in 0-35.
#' @param vrange integer(2). Both should be in 0-17.
#' @description Blue Marble products are in HDF5 format and are read without
#' georeference with typical R geospatial packages.
#' This function generates a data.frame of corner coordinates for assignment.
#' @returns data.frame with xmin, xmax, ymin, and ymax fields
#' @author Insang Song
#' @references
#' - [Wang, Z. (2022). Blue Marble User Guide (Version 1.3). NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)
#' @export
# nolint end
process_bluemarble_corners <-
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

#' Check date format
#' @description
#' Check date input strings conform to the required format.
#' @param instr character(1). String to check.
#' @param format character(1). Matching format to be checked.
#' Default is `"%Y-%m-%d"`, which can detect `"%Y/%m/%d`.
#' See [strftime] for details of formatting this string.
#' @returns No returning value. It stops the function if `instr` doesn't
#' conform to the `format`.
#' @author Insang Song
#' @keywords internal
#' @export
is_date_proper <- function(
  instr = NULL,
  format = "%Y-%m-%d"
) {
  # the results are alphabetically ordered
  argnames <- mget(ls())
  datestr <- try(strftime(instr, format = format))
  if (inherits(datestr, "try-error")) {
    stop(sprintf("%s does not conform to the required format
         \"YYYY-MM-DD\".\n", names(argnames)[2]))
  }
}


# nolint start
#' Assign VIIRS Blue Marble products corner coordinates to retrieve a merged raster
#' @description This function will return a SpatRaster object with
#' georeferenced h5 files of Blue Marble product. Referencing corner coordinates
#' are necessary as the original h5 data do not include such information.
#' @param path character. Full paths of h5 files.
#' @param date character(1). Date to query.
#' @param tile_df data.frame. Contains four corner coordinates in fields named
#' `c("xmin", "xmax", "ymin", "ymax")`.
#' See [`process_bluemarble_corners`] to generate a valid object for this argument.
#' @param subdataset integer(1). Subdataset number to process.
#' Default is 3L.
#' @param crs character(1). terra::crs compatible CRS.
#' Default is `"EPSG:4326"`
#' @param ... For internal use.
#' @returns a SpatRaster object
#' @author Insang Song
#' @seealso
#' * [`terra::describe`]
#' * [`terra::merge`]
#' * [`process_bluemarble_corners`]
#' @references
#' - [Wang, Z. (2022). Blue Marble User Guide (Version 1.3). NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)
#' @importFrom terra rast
#' @importFrom terra ext
#' @importFrom terra crs
#' @importFrom terra merge
#' @export
# previously modis_preprocess_vnp46
# nolint end
process_bluemarble <- function(
  path = NULL,
  date = NULL,
  tile_df = process_bluemarble_corners(),
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
#' Main procedure is done with [stars::st_warp], in which users are able to
#' customize the threshold to fill potential gaps that appear where
#' the target resolution is finer than the local resolution of curvilinear
#' grid points.
#' @param path File path of MODIS swath with exact sub-dataset specification.
#' @param cellsize numeric(1). Cell size (spatial resolution) of
#' output rectilinear grid raster.
#' @param threshold numeric(1). Maximum distance to fill gaps if occur.
#' @param crs integer(1)/character(1). Coordinate system definition.
#' Should be compatible with EPSG codes or WKT2.
#' See [terra::crs] and [sf::st_crs] / [EPSG](https://www.epsg.io)
#' @param ... For internal use.
#' @note Users should specify sub-dataset with all flags that are
#' compatible with `gdalinfo`
#' @returns a stars object
#' @author Insang Song
#' @seealso [terra::rectify]
#' @importFrom stars st_warp
#' @importFrom stars read_stars
#' @export
# previously modis_warp_stars
process_modis_warp <-
  function(
    path = NULL,
    cellsize = 0.25,
    threshold = 0.5,
    crs = 4326,
    ...
  ) {
    options(sf_use_s2 = FALSE)
    ras <- stars::read_stars(path)
    rtd <-
      stars::st_warp(
        ras,
        crs = crs,
        cellsize = cellsize,
        threshold = threshold
      )
    return(rtd)
  }



# nolint start
#' Mosaic MODIS swaths
#' @description This function will return a SpatRaster object with
#' values of selected subdatasets. Swath data include curvilinear
#' grids, which require warping/rectifying the original curvilinear grids
#' into rectilinear grids. The function internally warps each of inputs
#' then mosaic the warped images into one large SpatRaster object.
#' Users need to select a subdataset to process. The full path looks like
#' `"HDF4_EOS:EOS_SWATH:{file_path}:mod06:subdataset"`, where file_path is
#' the full path to the hdf file.
#' @param path character. Full paths of hdf files.
#' @param date character(1). Date to query.
#' @param subdataset character. One of `"Cloud_Fraction_Day"` or
#' `"Cloud_Fraction_Night"` (which are available in MOD06_L2)
#' @param suffix character(1). Should be formatted `:{product}:`,
#' e.g., `:mod06:`
#' @param resolution numeric(1). Resolution of output raster.
#' Unit is degree.
#' @param ... For internal use.
#' @seealso
#' * [process_modis_warp]
#' * [GDAL HDF4 driver documentation](https://gdal.org/drivers/raster/hdf4.html)
#' * [terra::describe]: to list the full subdataset list with `sds = TRUE`
#' @returns a SpatRaster object (crs = `"EPSG:4326"`)
#' @author Insang Song
#' @importFrom terra rast
#' @importFrom terra crop
#' @importFrom terra mosaic
#' @export
# nolint end
# previously modis_mosaic_mod06
process_modis_swath <-
  function(
    path = NULL,
    date = NULL,
    subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
    suffix = ":mod06:",
    resolution = 0.025,
    ...
  ) {
    # check date format
    is_date_proper(instr = date)
    header <- "HDF4_EOS:EOS_SWATH:"
    ras_mod06 <- vector("list", 2L)
    datejul <- strftime(date, format = "%Y%j")
    paths_today <- grep(sprintf("A%s", datejul), path, value = TRUE)

    # if two or more paths are put in,
    # these are read into a list then mosaicked
    if (length(path) > 1) {
      for (element in seq_along(subdataset)) {
        target_text <-
          sprintf("%s%s%s%s", header, paths_today, suffix, subdataset[element])
        # rectified stars objects to SpatRaster
        mod06_element <- split(target_text, target_text) |>
          lapply(process_modis_warp) |>
          lapply(terra::rast)
        # mosaick the warped SpatRasters into one
        mod06_element <- Reduce(f = terra::mosaic, x = mod06_element)
        ras_mod06[[element]] <- mod06_element
      }
      mod06_mosaic <- c(ras_mod06[[1]], ras_mod06[[2]])
      # assigning variable name
      terra::varnames(mod06_mosaic) <- subdataset
    } else {
      mod06_mosaic <- terra::rast(process_modis_warp(path))
    }
    return(mod06_mosaic)
  }



# Process downloaded raw data

#' Process climate classification data
#' @description
#' The \code{process_koppen_geiger()} function imports and cleans raw climate
#' classification data, returning a single SpatRaster object.
#' @param path character(1). Path to Koppen-Geiger
#'  climate zone raster file
#' @param year data year. Not applicable for this function.
#' @returns a SpatRaster object
#' @author Insang Song
#' @importFrom terra rast
#' @export
process_koppen_geiger <-
  function(
    path = NULL,
    year = NULL
  ) {
    kg_rast <- terra::rast(path)
    return(kg_rast)
  }


#' Process land cover data
#' @description
#' The \code{process_nlcd()} function imports and cleans raw land cover data,
#' returning a single SpatRaster object.
#' @param path character giving nlcd data path
#' @param year numeric giving the year of NLCD data used
#' @description Reads NLCD file of selected `year`.
#' @returns a SpatRaster object
#' @author Eva Marques, Insang Song
#' @importFrom utils read.csv
#' @importFrom terra rast
#' @importFrom terra metags
#' @export
process_nlcd <-
  function(
    path = NULL,
    year = 2021
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
        pattern = paste0("nlcd_", year, "_.*.tif$"),
        full.names = TRUE
      )
    if (length(nlcd_file) == 0) {
      stop("NLCD data not available for this year.")
    }
    nlcd <- terra::rast(nlcd_file)
    terra::metags(nlcd) <- c(year = year)
    return(nlcd)
  }


#' Process ecoregion data
#' @description
#' The \code{process_ecoregion()} function imports and cleans raw ecoregion
#' data, returning a SpatVector object.
#' @param path character(1). Path to Ecoregion Shapefiles
#' @author Insang Song
#' @returns a SpatVector object
#' @importFrom terra vect
#' @export
process_ecoregion <-
  function(
    path = NULL
  ) {
    ecoreg <- terra::vect(path)
    ecoreg <- ecoreg[, grepl("^(L2_KEY|L3_KEY)", names(ecoreg))]
    return(ecoreg)
  }


#' Check input assumptions
#' @param locs Data. [sf][sf::st_as_sf],
#' [SpatVector][terra::vect], or [data.frame]
#' @param check_time logical(1). Whether `"time"` exists in column names.
#' @param locs_epsg character(1). `"{authority}:{code}"` or
#' Well-Known Text format for coordinate reference system definition.
#' @description Check if all of `"lon"`, `"lat"`, and `"time"`
#' (only if `check_time = TRUE`) then convert inputs into a
#' SpatVector object.
#' @returns a SpatVector object
#' @author Insang Song
#' @importFrom methods is
#' @importFrom terra vect
#' @export
process_conformity <-
  function(
    locs = NULL,
    check_time = FALSE,
    locs_epsg = "EPSG:4326"
  ) {
    keyword <- c("lon", "lat", "time")
    if (!check_time) {
      keyword <- keyword[-3]
    }
    if (!all(keyword %in% names(locs))) {
      stop("locs should have 'lon', 'lat', (and 'time') fields.\n")
    }
    if (!methods::is(locs, "SpatVector")) {
      if (methods::is(locs, "sf")) {
        locs <- terra::vect(locs)
      }
      if (is.data.frame(locs)) {
        locs <-
          terra::vect(
            locs,
            geom = c("lon", "lat"),
            keepgeom = TRUE,
            crs = locs_epsg
          )
      }
    }
    return(locs)
  }






# nolint start
#' Process toxic release data
#' @description
#' The \code{process_tri()} function imports and cleans raw toxic release data,
#' returning a single SpatVector (points) object for the selected `year`.
#' @param path character(1). Path to the directory with TRI CSV files
#' @param year integer(1). Single year to select.
#' @param variables integer. Column index of TRI data.
#' @param ... Placeholders.
#' @author Insang Song, Mariana Kassien
#' @returns a SpatVector object (points) in `year`
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
# nolint end
#' @export
process_tri <- function(
  path = NULL,
  year = 2018,
  variables = c(1, 13, 12, 14, 20, 34, 36, 47, 48, 49),
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

  return(spvect_tri)
}
# nolint end




# nolint start
#' Process road emissions data
#' @description
#' The \code{process_tri()} function imports and cleans raw road emissions data,
#' returning a single SpatVector object.
#' @param path character(1). Directory with NEI csv files.
#' @param county SpatVector/sf. County boundaries.
#' @param year integer(1) Year to use. Currently only 2017 or 2020
#' is accepted.
#' @returns a SpatVector object
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
#' @export
# nolint end
process_nei <- function(
  path = NULL,
  county = NULL,
  year = c(2017, 2020)
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
  if (!year %in% c(2017, 2020)) {
    stop("year should be one of 2017 or 2020.\n")
  }
  # Concatenate NEI csv files
  csvs_nei <- list.files(path = path, pattern = "*.csv$", full.names = TRUE)
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
  csvs_nei$nei_year <- year

  # read county vector
  cnty_geoid_guess <- grep("GEOID", names(county))
  names(county)[cnty_geoid_guess] <- "geoid"
  county$geoid <- sprintf("%05d", as.integer(county$geoid))
  cnty_vect <- merge(county, csvs_nei, by = "geoid")
  cnty_vect <- cnty_vect[, c("geoid", "nei_year", "TRF_NEINP_0_00000")]
  return(cnty_vect)

}

# nolint start
#' Process unique U.S. EPA AQS sites
#' @description
#' The \code{process_aqs()} function cleans and imports raw air quality
#' monitoring sites, returning a single SpatVector or sf object. 
#' @param path character(1). Directory path to daily measurement data.
#' @param date character(2). Start and end date.
#'  Should be in `"YYYY-MM-DD"` format and sorted. If `NULL`,
#'  only unique locations are returned.
#' @param return_format character(1). `"terra"` or `"sf"`.
#' @returns a SpatVector or sf object depending on the `return_format`
#' @importFrom data.table as.data.table
#' @importFrom utils read.csv
#' @importFrom terra vect
#' @importFrom terra project
#' @importFrom sf st_as_sf
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @note `date = NULL` will return a massive data.table
#' object. Please choose proper `date` values.
#' @export
process_aqs <-
  function(
    path = NULL,
    date = c("2018-01-01", "2022-12-31"),
    return_format = "terra"
  ) {
    if (!is.null(date)) {
      date <- try(as.Date(date))
      if (inherits(date, "try-error")) {
        stop("date has invalid format(s). Please check the values.")
      }
      if (length(date) != 2) {
        stop("date should be a character vector of length 2.")
      }
    }

    if (length(path) == 1 && dir.exists(path)) {
      path <- list.files(
        path = path,
        pattern = "*.csv$",
        full.names = TRUE
      )
    }
    
    pathfiles <- try(lapply(path, read.csv))

    if (inherits(pathfiles, "try-error")) {
      stop("path is not a directory or does not contain csv files.")
    }

    sites <- data.table::rbindlist(pathfiles)

    ## get unique sites
    sites$site_id <-
      sprintf("%02d%03d%04d%05d",
              sites$State.Code,
              sites$County.Code,
              sites$Site.Num,
              sites$Parameter.Code)

    site_id <- NULL
    Datum <- NULL

    # select relevant fields only
    sites <- sites |>
      dplyr::as_tibble() |>
      dplyr::group_by(site_id) |>
      dplyr::filter(POC == min(POC)) |>
      dplyr::ungroup()
    sites_v <- unique(sites[, c("site_id", "Longitude", "Latitude", "Datum")])
    names(sites_v)[2:3] <- c("lon", "lat")
    sites_v <- data.table::as.data.table(sites_v)

    # subset mainland
    sites_v <- sites_v[!grepl("^(02|15|72|78|6|80)", site_id), ]

    # NAD83 to WGS84
    sites_v_nad <-
      sites_v[Datum == "NAD83"]
    sites_v_nad <-
      terra::vect(
        sites_v_nad,
        keepgeom = TRUE,
        crs = "EPSG:4269"
      )
    sites_v_nad <- terra::project(sites_v_nad, "EPSG:4326")
    # postprocessing: combine WGS84 and new WGS84 records
    sites_v_nad <- sites_v_nad[, seq(1, 3)]
    sites_v_nad <- as.data.frame(sites_v_nad)
    sites_v_wgs <- sites_v[Datum == "WGS84"][, -4]
    final_sites <- rbind(sites_v_wgs, sites_v_nad)

    if (!is.null(date)) {
      date_start <- as.Date(date[1])
      date_end <- as.Date(date[2])
      date_sequence <- seq(date_start, date_end, "day")
      final_sites <-
        split(date_sequence, date_sequence) |>
        lapply(function(x) {
          fs_time <- final_sites
          fs_time$time <- x
          return(fs_time)
        })
      final_sites <- Reduce(rbind, final_sites)
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
        )
      )

    return(final_sites)
  }


#' Process population density data
#' @description
#' The \code{process_secac_population()} function imports and cleans raw
#' population density data, returning a single SpatRaster object.
#' @param path character(1). Path to GeoTIFF (.tif) or netCDF (.nc) file.
#' @author Mitchell Manware
#' @return a SpatRaster object
#' @importFrom terra rast
#' @export
# nolint end
process_sedac_population <- function(
    path = NULL) {
  if (substr(path, nchar(path) - 2, nchar(path)) == ".nc") {
    cat(paste0("netCDF functionality for SEDAC data is under construction.\n"))
    return()
  }
  #### check for variable
  check_for_null_parameters(mget(ls()))
  #### import data
  data <- terra::rast(path)
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
    cat(paste0(
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
  }
  return(data)
}


# nolint start
#' Process roads data
#' @description
#' The \code{process_sedac_groads()} function imports and cleans raw road data,
#' returning a single SpatVector object.
#' @param path character(1). Path to geodatabase or shapefiles.
#' @note U.S. context.
#' @author Insang Song
#' @returns a SpatVector boject
#' @importFrom terra vect
#' @export
# nolint end
process_sedac_groads <- function(
    path = NULL) {
  #### check for variable
  check_for_null_parameters(mget(ls()))
  if (!grepl("(shp|gdb)$", path)) {
    stop("Input is not in expected format.\n")
  }
  #### import data
  data <- terra::vect(path)
  return(data)
}


# nolint start
#' Process wildfire smoke data
#' @description
#' The \code{process_hms()} function imports and cleans raw wildfire smoke
#' plume coverage data, returning a single SpatVector object. 
#' @param date character(2). length of 10 each.
#' Start/end date of downloaded data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). "Light", "Medium", or "Heavy".
#' @param path character(1). Directory with downloaded NOAA HMS data files.
#' @note
#' \code{process_hms()} will return a character object if there are no wildfire
#' smoke plumes present for the selected dates and density. The returned
#' character will contain the selected density value and the sequence of dates
#' for which no wildfire smoke plumes were detected (see "Examples").
#' @examples
#' process_hms(
#'   date = c("2018-12-30", "2019-01-01"),
#'   variable = "Light",
#'   path = "../tests/testdata/hms/"
#' )
# nolint end
#' @author Mitchell Manware
#' @return a SpatVector or character object
#' @importFrom terra vect
#' @importFrom terra aggregate
#' @importFrom terra subset
#' @export
process_hms <- function(
    date = c("2018-01-01", "2018-01-01"),
    variable = c("Light", "Medium", "Heavy"),
    path = NULL) {
  #### directory setup
  path <- download_sanitize_path(path)
  #### check for variable
  check_for_null_parameters(mget(ls()))
  #### identify file paths
  paths <- list.files(
    path,
    pattern = "hms_smoke",
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
    data_date <- terra::vect(data_paths[d])
    data_date_p <- terra::project(
      data_date,
      "EPSG:4326"
    )
    #### subset to density of interest
    data_density <- data_date_p[data_date_p$Density == variable]
    #### absent polygons (ie. December 31, 2018)
    if (nrow(data_density) == 0) {
      cat(paste0(
        variable,
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
        substr(
          data_density$Start[1],
          1,
          7
        ),
        format = "%Y%j"
      )
      cat(paste0(
        "Cleaning ",
        tolower(variable),
        " data for date ",
        date,
        "...\n"
      ))
      #### zero buffer to avoid self intersection
      data_0_buffer <- terra::buffer(
        data_density,
        width = 0
      )
      #### aggregate polygons
      data_aggregate <- terra::aggregate(
        data_0_buffer,
        by = "Density",
        dissolve = TRUE
      )
      #### factorize
      data_aggregate$Date <- paste0(
        gsub(
          "-",
          "",
          date
        )
      )
      #### select "Density" and "Date"
      data_aggregate <- data_aggregate[
        seq_len(nrow(data_aggregate)), c("Density", "Date")
      ]
      #### merge with other data
      data_return <- rbind(data_return, data_aggregate)
    }
  }
  #### if no polygons
  if (nrow(data_return) == 0) {
    cat(paste0(
      variable,
      " smoke plume polygons absent from ",
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
    return(c(variable, dates_of_interest))
  } else if (nrow(data_return) > 0) {
    cat(paste0(
      "Returning daily ",
      tolower(variable),
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
    #### return SpatVector
    return(data_return)
  }
}

#' Process elevation data
#' @description
#' The \code{process_gmted()} function imports and cleans raw elevation data,
#' returning a single SpatRaster object.
#' @param variable vector(1). Vector containing the GMTED statistic first and
#' the resolution second. (Example: variable = c("Breakline Emphasis",
#' "7.5 arc-seconds")).
#' @param path character(1). Directory with downloaded GMTED  "*_grd"
#' folder containing .adf files.
#' @author Mitchell Manware
#' @note
#' SpatRaster layer name indicates selected variable and resolution.
#' @return a SpatRaster object
#' @importFrom terra rast
#' @export
process_gmted <- function(
    variable = NULL,
    path = NULL) {
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
  cat(paste0(
    "Cleaning ",
    statistic,
    " data at ",
    resolution,
    " resolution.\n"
  ))
  #### identify file path
  paths <- list.files(
    path,
    full.names = TRUE
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
  data_path <- data_paths[endsWith(data_paths, "_grd")]
  #### import data
  data <- terra::rast(data_path)
  #### set coordinate reference system
  return(data)
}

#' Process meteorological data
#' @description
#' The \code{process_narr()} function imports and cleans raw meteorological
#' data, returning a single SpatRaster object.
#' @param date character(2). length of 10 each. Format "YYYY-MM-DD".
#' @param variable character(1). Variable name acronym.
#' @param path character(1). Directory with downloaded netCDF (.nc) files.
#' @note
#' Layer names of the returned SpatRaster object contain the variable acronym,
#' pressure level, and date.
#' @author Mitchell Manware
#' @return a SpatRaster object
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra subset
#' @importFrom stringi stri_pad
#' @export
process_narr <- function(
    date = c("2023-09-01", "2023-09-01"),
    variable = NULL,
    path = NULL) {
  #### directory setup
  path <- download_sanitize_path(path)
  #### check for variable
  check_for_null_parameters(mget(ls()))
  #### identify file paths
  data_paths <- list.files(
    path,
    pattern = variable,
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
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths)) {
    #### import data
    data_year <- terra::rast(data_paths[p])
    cat(paste0(
      "Cleaning ",
      variable,
      " data for year ",
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
      cat(paste0("Detected pressure levels data...\n"))
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
          stringi::stri_pad(
            days,
            width = 3,
            pad = "0",
            side = "left"
          )
        ),
        format = "%Y%j"
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
      cat(paste0("Detected monolevel data...\n"))
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
  cat(paste0(
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
#' composition data, returning a single SpatRaster object.
#' @param date character(2). length of 10. Format "YYYY-MM-DD".
#' @param variable character(1). GEOS-CF variable name(s).
#' @param path character(1). Directory with downloaded netCDF (.nc4) files.
#' @note
#' Layer names of the returned SpatRaster object contain the variable,
#' pressure level, date, and hour.
#' @author Mitchell Manware
#' @return a SpatRaster object;
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra varnames
#' @importFrom terra crs
#' @importFrom terra timeInfo
#' @importFrom terra hasValues
#' @importFrom terra subset
#' @export
process_geos <-
  function(date = c("2018-01-01", "2018-01-01"),
           variable = NULL,
           path = NULL) {
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
    collection <- process_geos_collection(
      data_paths[1],
      collection = TRUE
    )
    cat(
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
      data_datetime <- process_geos_collection(data_paths[p], datetime = TRUE)
      cat(paste0(
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
    #### set coordinate refernce system
    terra::crs(data_return) <- "EPSG:4326"
    cat(paste0(
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

#' Process GEOS-CF collections
#' @description
#' Identify the GEOS-CF collection based on the file path.
#' @param path character(1). File path to GEOS-CF data file.
#' @param collection logical(1). Identifies and returns GEOS-CF collection
#' name(s) based on provided file path(s).
#' @param date logical(1). Identifies and returns date sequence (YYYYMMDD) based
#' on provided file path(s).
#' @param datetime logical(1). Identifies and returns date time sequence
#' (YYYYMoMoDDHHMiMi) based on provided file path(s).
#' @keywords internal
#' @return character
#' @export
process_geos_collection <-
  function(
      path,
      collection = FALSE,
      date = FALSE,
      datetime = FALSE) {
    #### check for more than one true
    parameters <- c(collection, date, datetime)
    if (length(parameters[parameters == TRUE]) > 1) {
      stop(
        paste0(
          "Select one of 'collection', 'date', or 'datetime'.\n"
        )
      )
    }
    #### split full file path based on unique GEOS-CF character
    split_geos <- unlist(
      strsplit(
        path,
        "GEOS-CF.v01.rpl."
      )
    )
    #### split file path into collection, datetime, and "nc4"
    split_period <- unlist(
      strsplit(
        split_geos[
          which(
            endsWith(split_geos, ".nc4")
          )
        ],
        "\\."
      )
    )
    #### remove "nc4"
    split_wo_nc4 <- split_period[!split_period == "nc4"]
    #### create data frame
    split_df <- data.frame(
      split_wo_nc4[
        which(
          !(endsWith(
            split_wo_nc4,
            "z"
          ))
        )
      ],
      split_wo_nc4[
        which(
          endsWith(
            split_wo_nc4,
            "z"
          )
        )
      ]
    )
    #### colnames
    colnames(split_df) <- c("collection", "datetime")
    #### return only collection name
    if (collection == TRUE) {
      return(split_df$collection)
    }
    #### return date sequence
    if (date == TRUE) {
      split_dates <- substr(
        split_df$datetime,
        1,
        8
      )
      return(split_dates)
    }
    #### return datetime sequence
    if (datetime == TRUE) {
      split_datetime <- gsub(
        "_",
        "",
        gsub(
          "z",
          "",
          split_df$datetime
        )
      )
      return(split_datetime)
    }
  }

#' Process elevation statistic and resolution codes
#' @description
#' Identify the GMTED statistic and resolution based on the file path. Convert
#' statistic and resolution to/from full string to/from statistic and
#' resolution code.
#' @param string character(1). File path to GMTED data file.
#' @param statistic logical(1). Matches statistic to statistic code.
#' @param resolution logical(1). Matches resolution to resolution code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides statistic or resolution code, and returns full length statistic
#' or resolution.
#' @keywords internal
#' @return character
#' @export
process_gmted_codes <-
  function(
      string,
      statistic = FALSE,
      resolution = FALSE,
      invert = FALSE) {
    statistics <- c(
      "Breakline Emphasis", "Systematic Subsample",
      "Median Statistic", "Minimum Statistic",
      "Mean Statistic", "Maximum Statistic",
      "Standard Deviation Statistic"
    )
    statistic_codes <- c("be", "ds", "md", "mi", "mn", "mx", "sd")
    statistic_codes <- cbind(statistics, statistic_codes)
    resolutions <- c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds")
    resolution_codes <- c("75", "15", "30")
    resolution_codes <- cbind(resolutions, resolution_codes)
    if (statistic == TRUE && invert == FALSE) {
      code <- statistic_codes[statistic_codes[, 1] == string][2]
    } else if (statistic == TRUE && invert == TRUE) {
      code <- statistic_codes[statistic_codes[, 2] == string][1]
    }
    if (resolution == TRUE && invert == FALSE) {
      code <- resolution_codes[resolution_codes[, 1] == string][2]
    } else if (resolution == TRUE && invert == TRUE) {
      code <- resolution_codes[resolution_codes[, 2] == string][1]
    }
    return(code)
  }

#' Process population resolution code
#' @description
#' Convert full length resolution name to/from resolution code.
#' @param string character(1). Resolution name or code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides resolution code, and returns full length resolution.
#' @keywords internal
#' @export
process_sedac_codes <-
  function(
    string,
    invert = FALSE
  ) {
    resolution_namecodes <- cbind(
      c(
        "60 minute", "30 second", "2.5 minute",
        "15 minute", "30 minute"
      ),
      c(
        "1_deg", "30_sec", "2pt5_min",
        "15_min", "30_min"
      )
    )
    if (invert == FALSE) {
      resolution <-
        resolution_namecodes[resolution_namecodes[, 1] == string][2]
    } else if (invert == TRUE) {
      resolution <-
        resolution_namecodes[resolution_namecodes[, 2] == string][1]
    }
    return(resolution)
  }

#' Process locations buffer
#' @description
#' Create circular buffer around locations based on user defined radius.
#' @param locs SpatVector(1). SpatVector object with point geometry
#' @param radius integer(1). Circular buffer size (meters).
#' @description Creates a circular buffer around points if `radius` is > 0.
#' Returns points if `radius` is 0.
#' @keywords internal
#' @returns SpatVector.
#' @importFrom terra buffer
#' @export
process_locs_radius <-
  function(
    locs,
    radius
  ) {
    cat(paste0(
      "Utilizing ",
      radius,
      " meter buffer for covariate calculations.\n"
    ))
    if (radius == 0) {
      return(locs)
    } else if (radius > 0) {
      sites_buffer <- terra::buffer(
        locs,
        radius
      )
      return(sites_buffer)
    }
  }

#' Process locations as SpatVector
#' @description
#' Convert locations from class \code{data.frame} or \code{data.table} to
#' SpatVector object, project to coordinate reference system, and apply
#' circular buffer.
#' @param locs data.frame(1). Data frame containing columns for unique
#' identifier, latitute, and longitude. Latitude and longitude columns **must**
#' be named "lat" and "lon", respectively.
#' @param crs Coordinate reference system (CRS) description utilizing
#' `terra::crs()`.
#' @param radius integer(1). Circular buffer size (meters).
#' @keywords internal
#' @returns SpatVector
#' @importFrom terra crs
#' @importFrom terra vect
#' @importFrom terra project
#' @export
process_locs_vector <-
  function(
    locs,
    crs,
    radius
  ) {
    #### sites as data frame
    if ("data.table" %in% class(locs)) {
      cat(paste0(
        "Converting data.table to data.frame...\n"
      ))
      sites_df <- data.frame(locs)
    } else if ("data.frame" %in% class(locs) &&
                 !("data.table" %in% class(locs))) {
      cat(paste0(
        "Sites are class data.frame...\n"
      ))
      sites_df <- locs
    } else if (!("data.table" %in% class(locs)) &&
                 !("data.frame" %in% class(locs))) {
      stop(
        paste0(
          "Detected a ",
          class(locs)[1],
          " object. Sites must be class data.frame or data.table.\n"
        )
      )
    }
    #### columns
    if (any(!(c("lon", "lat") %in% colnames(locs)))) {
      stop(paste0(
        "Sites data is missing 'lon', 'lat', or both.\n"
      ))
    }
    #### as SpatVector
    sites_v <- terra::vect(
      sites_df,
      geom = c("lon", "lat"),
      crs = "EPSG:4326"
    )
    #### project SpatVector
    cat(paste0(
      "Projecting data to desired coordinate reference system...\n"
    ))
    sites_p <- terra::project(
      sites_v,
      crs
    )
    #### buffer SpatVector
    sites_b <- process_locs_radius(
      sites_p,
      radius
    )
    return(sites_b)
  }
