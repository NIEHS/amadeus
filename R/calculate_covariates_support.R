
#' Selected MODIS sinusoidal grid product subdataset name selector
#' @param product character(1). Product code.
#' @param custom_sel character(1). Custom filter.
#' If this value is not NULL, preset filter is
#' overridden.
#' @description Four presets are supported. `custom_sel` supersedes
#' presets of `product` values.
#' @author Insang Song
#' @returns A character object that conforms to the regular
#' expression. Details of regular expression in R can be found in [regexp].
#' @seealso [calc_modis]
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


#' Aggregate layers in a sub-dataset in sinusoidal MODIS products
#' @param path character(1). Full path to MODIS HDF4/HDF5 file.
#' Direct sub-dataset access is supported, for example,
#' HDF4_EOS:EOS_GRID:\{filename\}:\{base_grid_information\}:\{sub-dataset\}
#' @param product character(1). Name of MODIS product.
#' @param subdataset character(1). Exact or regular expression filter of
#' sub-dataset. See [process_modis_sds] for details.
#' @param fun_agg character(1). Function name to aggregate layers.
#' Should be acceptable to [terra::tapp].
#' @returns SpatRaster.
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
    path,
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
#' Get mosaicked or merged raster from multiple MODIS hdf files
#' @param paths character. Full list of hdf file paths.
#'  preferably a recursive search result from \code{list.files}.
#' @param date_in character(1). date to query. Should be in
#' \code{"YYYY-MM-DD"} format.
#' @param subdataset character(1). subdataset names to extract.
#' Should conform to regular expression. See \link{regex} for details.
#' Default is NULL, which will result in errors. Users should specify
#' which subdatasets will be imported.
#' @param foo Function name or custom function to aggregate overlapping
#' cell values. See \code{fun} description in \link[terra]{tapp} for details.
#' @param ... For internal use.
#' @note Curvilinear products (i.e., swaths) will not be accepted.
#' MODIS products downloaded by functions in `amadeus`,
#' [MODISTools](https://cran.r-project.org/web/packages/MODISTools/index.html),
#' and [luna](https://github.com/rspatial/luna) are accepted.
#' @seealso [download_data]
#' @author Insang Song
#' @returns A SpatRaster object.
#' @export
# nolint end
# previously modis_get_vrt
process_modis_merge <- function(
    paths,
    date_in = NULL,
    subdataset = NULL,
    foo = "mean",
    ...) {

  if (!is.character(paths)) {
    stop("Argument flist should be a list of hdf files (character).\n")
  }
  if (!(is.character(foo) || is.function(foo))) {
    stop("Argument foo should be a function or name of a function
         that is accepted in terra::tapp.\n")
  }
  # date format check
  is_date_proper(instr = date_in)

  # interpret date
  today <- as.character(date_in)
  dayjul <- strftime(today, "%Y%j")
  ftarget <- grep(sprintf("A%s", dayjul), paths, value = TRUE)

  # get layer information
  layer_target <-
    lapply(ftarget,
           function(x) {
             process_flatten_sds(
               x,
               subdataset = subdataset,
               fun_agg = foo
             )
           })
  # Merge multiple rasters into one
  # do.call(f, l) is equivalent to f(l[[1]], ... , l[[length(l)]])
  if (length(paths) > 1) {
    result_merged <- do.call(terra::merge, layer_target)
    gc()
  } else {
    result_merged <- layer_target[[1]]
  }
  return(result_merged)
}


# nolint start
#' Tile corner generator for Blue Marble products
#' @param hrange integer(2). Both should be in [0, 35]
#' @param vrange integer(2). Both should be in [0, 17]
#' @description Blue Marble products are in HDF5 format and are read without
#' georeference with typical R geospatial packages.
#' This function generates a data.frame of corner coordinates for assignment.
#' @returns data.frame with xmin, xmax, ymin, and ymax fields.
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
    if (!any(hrange %in% seq(0, 35)) || !any(vrange %in% seq(0, 17))) {
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

#' Check input strings conform to the required format
#' @param instr character(1). String to check.
#' @param format character(1). Matching format to be checked.
#' Default is `"%Y-%m-%d"`, which can detect `"%Y/%m/%d`.
#' See [strftime] for details of formatting this string.
#' @returns No returning value. It stops the function if `instr` doesn't
#' conform to the `format`.
#' @author Insang Song
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
#' @param paths character. Full paths of h5 files.
#' @param date_in character(1). Date to query.
#' @param tile_df data.frame. Contains four corner coordinates in fields named
#' `c("xmin", "xmax", "ymin", "ymax")`.
#' See [process_bluemarble_corners] to generate a valid object for this argument.
#' @param subdataset integer(1). Subdataset number to process.
#' Default is 3L.
#' @param crs_ref character(1). terra::crs compatible CRS.
#' Default is "EPSG:4326"
#' @param ... For internal use.
#' @returns SpatRaster.
#' @author Insang Song
#' @seealso
#' * [terra::describe]
#' * [terra::merge]
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
  paths,
  date_in,
  tile_df = NULL,
  subdataset = 3L,
  crs_ref = "EPSG:4326",
  ...
) {
  is_date_proper(instr = date_in)
  # interpret date from paths
  date_in <- as.Date(date_in)
  datejul <- strftime(date_in, format = "%Y%j")
  stdtile <- tile_df$tile

  filepaths_today <- grep(sprintf("A%s", datejul), paths, value = TRUE)
  # today's filenames
  filepaths_today <-
    grep(paste0("(",
               paste(stdtile, collapse = "|"), ")"),
         filepaths_today, value = TRUE)

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
      # print(tile_ext)
      terra::crs(vnp_) <- terra::crs(crs_ref)
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
#' @param crs_out integer(1)/character(1). Coordinate system definition.
#' Should be compatible with EPSG codes or WKT2.
#' See [terra::crs] and [sf::st_crs] / [EPSG](https://www.epsg.io)
#' @param ... For internal use.
#' @note Users should specify sub-dataset with all flags that are
#' compatible with `gdalinfo`
#' @returns stars object.
#' @author Insang Song
#' @seealso [terra::rectify]
#' @importFrom stars st_warp
#' @importFrom stars read_stars
#' @export
# previously modis_warp_stars
process_modis_warp <-
  function(
    path,
    cellsize = 0.25,
    threshold = 0.5,
    crs_out = 4326,
    ...
  ) {
    options(sf_use_s2 = FALSE)
    ras <- stars::read_stars(path)
    rtd <-
      stars::st_warp(
        ras,
        crs = crs_out,
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
#' @param paths character. Full paths of hdf files.
#' @param date_in character(1). Date to query.
#' @param get_var character. One of `"Cloud_Fraction_Day"` or
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
#' @returns SpatRaster object. CRS is `"EPSG:4326"`.
#' @author Insang Song
#' @importFrom terra rast
#' @importFrom terra crop
#' @importFrom terra mosaic
#' @export
# nolint end
# previously modis_mosaic_mod06
process_modis_swath <-
  function(
    paths,
    date_in,
    get_var = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
    suffix = ":mod06:",
    resolution = 0.025,
    ...
  ) {
    # check date format
    is_date_proper(instr = date_in)
    header <- "HDF4_EOS:EOS_SWATH:"
    ras_mod06 <- vector("list", 2L)
    datejul <- strftime(date_in, format = "%Y%j")
    paths_today <- grep(sprintf("A%s", datejul), paths, value = TRUE)

    # if two or more paths are put in,
    # these are read into a list then mosaicked
    if (length(paths) > 1) {
      for (element in seq_along(get_var)) {
        target_text <-
          sprintf("%s%s%s%s", header, paths_today, suffix, get_var[element])
        # rectified stars objects to SpatRaster
        mod06_element <- split(target_text, target_text) |>
          lapply(process_modis_warp) |>
          lapply(terra::rast)
        mod06_element <- Reduce(f = terra::mosaic, x = mod06_element)
        ras_mod06[[element]] <- mod06_element
      }
      mod06_mosaic <- c(ras_mod06[[1]], ras_mod06[[2]])
      terra::varnames(mod06_mosaic) <- get_var
    } else {
      mod06_mosaic <- terra::rast(process_modis_warp(paths))
    }
    return(mod06_mosaic)
  }
