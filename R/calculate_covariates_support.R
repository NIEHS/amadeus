
#' Selected MODIS product subdataset name selector
#' @param product character(1). Product code.
#' @param custom_sel character(1). Custom filter.
#' If this value is not NULL, preset filter is
#' overridden.
#' @author Insang Song
#' @returns A character object that conforms to the regular
#' expression. Details of regular expression in R can be found in [regexp].
#' @seealso [calc_modis]
#' @export
modis_prefilter_sds <-
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


#' Aggregate layers in a MODIS sub-dataset
#' @param path character(1). Full path to MODIS HDF4/HDF5 file.
#' Direct sub-dataset access is supported, for example,
#' HDF4_EOS:EOS_GRID:\{filename\}:\{base_grid_information\}:\{sub-dataset\}
#' @param product character(1). Name of MODIS product.
#' @param nsds character(1). Exact or regular expression filter of sub-dataset.
#' See [modis_prefilter_sds] for details.
#' @param fun_agg character(1). Function name to aggregate layers.
#' Should be acceptable to [terra::tapp].
#' @author Insang Song
#' @seealso [terra::tapp], [terra::rast], [terra::describe]
#' @note HDF values are read as original without scaling.
#' Users should consult MODIS product documentation to apply proper
#' scaling factor for post-hoc adjustment. If users have no preliminary
#' information about MODIS HDF sub-datasets, consider running
#' `terra::describe(__filename__, sds = TRUE)` to navigate the full
#' list of sub-datasets in the input file.
#' @importFrom terra describe
#' @importFrom terra rast
#' @importFrom terra nlyr
#' @importFrom terra tapp
#' @export
modis_aggregate_sds <-
  function(
    path,
    product = c("MOD11A1", "MOD13A2", "MOD09GA", "MCD19A2"),
    nsds,
    fun_agg = "mean"
  ) {
    product <- match.arg(product)

    # describe provides subdataset information
    if (!any(grepl(":", path))) {
      # we use var to get detailed information in subdatasets
      sds_desc <- terra::describe(path, sds = TRUE)
      index_sds <- grep(nsds, sds_desc$var)
      sds_desc <- sds_desc[index_sds, c("name", "var", "nlyr")]

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

#' Get mosaicked or merged raster from multiple MODIS hdf files
#' @param paths character. Full list of hdf file paths.
#'  preferably a recursive search result from \code{list.files}.
#' @param product character(1). Product code of MODIS. Should be one of
#' \code{c("MOD11A1", "MOD13A2", "MOD09GA", "MCD19A2")}
#' @param date_in character(1). date to query. Should be in
#' \code{"YYYY-MM-DD"} format.
#' @param regex_sds character(1). subdataset names to extract.
#' Should conform to regular expression. See \link{regex} for details.
#' Default is NULL, which means that the subdataset names are automatically
#' selected based on \code{product} value.
#' @param foo Function name or custom function to aggregate overlapping
#' cell values. See \code{fun} description in \link[terra]{tapp} for details.
#' @author Insang Song
#' @returns A SpatRaster object.
#' @export
modis_get_vrt <- function(
    paths,
    product = c("MOD11A1", "MOD13A2",
                "MOD09GA", "MCD19A2"),
    date_in = NULL,
    regex_sds = NULL,
    foo = "mean") {

  product <- match.arg(product)

  if (!is.character(paths)) {
    stop("Argument flist should be a list of hdf files (character).\n")
  }
  if (!(is.character(foo) || is.function(foo))) {
    stop("Argument foo should be a function or name of a function
         that is accepted in terra::tapp.\n")
  }
  # this case cannot detect malform like 2024-02-30.
  if (!grepl("[0-9]{4,4}\\-([0][1-9]|[1][0-2])\\-([0-2][0-9]|[3][0-1])",
             date_in)
  ) {
    stop("date_in does not conform to the required format
         'YYYY-MM-DD'.\n")
  }

  # interpret date
  today <- as.character(date_in)
  dayjul <- strftime(today, "%Y%j")
  ftarget <- grep(sprintf("A%s", dayjul), paths, value = TRUE)

  # get layer information
  layer_target <-
    lapply(ftarget,
           function(x) {
             modis_aggregate_sds(
               x,
               product = product,
               nsds = regex_sds,
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


#' Assign MODIS VNP46 corner coordinates to retrieve a merged raster
#' @description This function will return a SpatRaster object with
#' georeferenced h5 files of VNP46A2 product. Referencing corner coordinates
#' are necessary as the original h5 data do not include such information.
#' @param paths character. Full paths of h5 files.
#' @param date_in character(1). Date to query.
#' @param subdataset integer(1). Subdataset number to process.
#' Default is 3L.
#' @param crs_ref character(1). terra::crs compatible CRS.
#' Default is "EPSG:4326"
#' @author Insang Song
#' @importFrom terra rast
#' @importFrom terra ext
#' @importFrom terra crs
#' @importFrom terra merge
#' @export
modis_preprocess_vnp46 <- function(
  paths,
  date_in,
  subdataset = 3L,
  crs_ref = "EPSG:4326"
) {
  # this case cannot detect malform like 2024-02-30.
  if (!grepl("[0-9]{4,4}\\-([0][1-9]|[1][0-2])\\-([0-2][0-9]|[3][0-1])",
             date_in)
  ) {
    stop("date_in does not conform to the required format
         'YYYY-MM-DD'.\n")
  }

  tile_df <-
    expand.grid(
      vaddr = sprintf("v%02d", 3:6),
      haddr = sprintf("h%02d", 5:11)
    )
  tile_df$tile <- paste0(tile_df$haddr, tile_df$vaddr)
  tile_df <- data.frame(tile = tile_df$tile)
  tile_df$xmin <- rep(seq(-130, -70, 10), each = 4)
  tile_df$xmax <- tile_df$xmin + 10
  tile_df$ymin <- rep(seq(50, 20, -10), 7)
  tile_df$ymax <- tile_df$ymin + 10

  date_in <- as.Date(date_in)
  datejul <- strftime(date_in, format = "%Y%j")
  stdtile <- tile_df$tile

  filepaths_today <- grep(sprintf("A%s", datejul), paths, value = TRUE)
  # today's filenames
  filepaths_today <-
    grep(paste("(",
               paste(stdtile, collapse = "|"), ")"),
         filepaths_today, value = TRUE)

  filepaths_today_tiles <-
    regmatches(filepaths_today,
               regexpr("h([0-2][0-9]|[3][0-6])v([0-1][0-9])", filepaths_today))

  vnp46_today <- unname(split(filepaths_today, filepaths_today))
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
    }, vnp46_today, filepaths_today_tiles_list, SIMPLIFY = FALSE)
  if (length(filepaths_today) > 1) {
    vnp_all <- do.call(terra::merge, vnp_assigned)
  } else {
    vnp_all <- vnp_assigned[[1]]
  }
  return(vnp_all)
}

#' Warp MODIS Swath data into rectilinear grid raster
#' @description Swath data is a type of MODIS data organization,
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
#' @note Users should specify sub-dataset with all flags that are
#' compatible with `gdalinfo`
#' @returns stars object.
#' @author Insang Song
#' @seealso [terra::rectify]
#' @importFrom stars st_warp
#' @importFrom stars read_stars
#' @export
modis_warp_stars <-
  function(
    path,
    cellsize = 0.25,
    threshold = 0.5,
    crs_out = 4326
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




#' Mosaic MODIS MOD06_L2 product files
#' @description This function will return a SpatRaster object with
#' mosaicked 5-minute cloud coverage values. Swath data include curvilinear
#' grids, which require warping/rectifying the original curvilinear grids
#' into rectilinear grids. The function internally warps each of inputs
#' then mosaic the warped images into one large SpatRaster object.
#' @param paths character. Full paths of hdf files.
#' @param date_in character(1). Date to query.
#' @param get_var character. One of `"Cloud_Fraction_Day"` or
#' `"Cloud_Fraction_Night"`
#' @param resolution numeric(1). Resolution of output raster.
#' Unit is degree.
#' @returns SpatRaster object. CRS is `"EPSG:4326"`.
#' @author Insang Song
#' @importFrom terra rast
#' @importFrom terra crop
#' @importFrom terra mosaic
#' @export
modis_mosaic_mod06 <-
  function(
    paths,
    date_in,
    get_var = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
    resolution = 0.025
  ) {
    if (!grepl("[0-9]{4,4}\\-([0][1-9]|[1][0-2])\\-([0-2][0-9]|[3][0-1])",
               date_in)
    ) {
      stop("date_in does not conform to the required format
           'YYYY-MM-DD'.\n")
    }
    header <- "HDF4_EOS:EOS_SWATH:"
    suffix <- ":mod06:"
    ras_mod06 <- vector("list", 2L)
    datejul <- strftime(date_in, format = "%Y%j")
    paths_today <- grep(sprintf("A%s", datejul), paths, value = TRUE)

    if (length(paths) > 1) {
      for (element in seq_along(get_var)) {
        target_text <-
          sprintf("%s%s%s%s", header, paths_today, suffix, get_var[element])
        # rectified stars objects to SpatRaster
        mod06_element <- split(target_text, target_text) |>
          lapply(modis_warp_stars) |>
          lapply(terra::rast)
        mod06_element <- Reduce(f = terra::mosaic, x = mod06_element)
        ras_mod06[[element]] <- mod06_element
      }
      mod06_mosaic <- c(ras_mod06[[1]], ras_mod06[[2]])
      terra::varnames(mod06_mosaic) <- get_var
      mod06_mosaic <- terra::crop(mod06_mosaic,
                                  terra::ext(c(-130, -60, 20, 54)))
    } else {
      mod06_mosaic <- terra::rast(modis_warp_stars(paths))
    }
    return(mod06_mosaic)
  }
