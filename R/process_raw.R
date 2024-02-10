# Process downloaded raw data

#' Process Koeppen-Geiger climate data
#' @param path character(1). Path to Koppen-Geiger
#'  climate zone raster file
#' @returns SpatRaster object
#' @author Insang Song
#' @importFrom terra rast
#' @export
process_koppen_geiger <-
  function(
    path = "./input/koppen_geiger/raw/Beck_KG_V1_present_0p0083.tif"
  ) {
    kg_rast <- terra::rast(path)
    return(kg_rast)
  }


#' Process raw National Landuse Classification Dataset
#'
#' @param path character giving nlcd data path
#' @param year numeric giving the year of NLCD data used
#' @description Reads NLCD file of selected `year`.
#' @returns SpatRaster object.
#' @author Eva Marques
#' @importFrom utils read.csv
#' @importFrom terra rast
#' @export
process_nlcd_ratio <-
  function(
    path,
    year = 2021
  ) {
    # check inputs
    if (!is.character(path)) {
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
    return(nlcd)
  }


#' Process EPA Ecoregion shapefiles
#' @param path character(1). Path to Ecoregion Shapefiles
#' @author Insang Song
#' @returns SpatVector object.
#' @importFrom terra vect
#' @export
calc_ecoregion <-
  function(
    path = "./input/data/ecoregions/raw/us_eco_l3_state_boundaries.shp"
  ) {
    ecoreg <- terra::vect(path)
    ecoreg <- ecoreg[, grepl("^(L2_KEY|L3_KEY)", names(ecoreg))]
    return(ecoreg)
  }


#' Check input assumptions
#' @param locs [stdt][convert_stobj_to_stdt], [sf][sf::st_as_sf],
#' [SpatVector][terra::vect], or [data.frame]
#' @description Check if all of `"lon"`, `"lat"`, and `"time"`
#' (only if `check_time = TRUE`) then convert inputs into a
#' SpatVector object.
#' @returns SpatVector object.
#' @author Insang Song
#' @importFrom methods is
#' @importFrom terra vect
#' @export
process_conformity <-
  function(
    locs,
    check_time = FALSE
  ) {
    if (is_stdt(locs)) {
      locs <- locs$stdt
      locs_epsg <- locs$crs_dt
    } else {
      if (!all(c("lon", "lat", "time") %in% names(locs))) {
        stop("sites should be stdt or 
have 'lon', 'lat', and 'time' fields.\n")
      }
      locs_epsg <- terra::crs(locs)
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
    }
    return(locs)
  }




#' Process raw TRI data
#' @param path character(1). Path to the directory with TRI CSV files
#' @param domain_year integer. Year to select.
#'  Default is 2018.
#' @author Insang Song, Mariana Kassien
#' @returns SpatVector object of TRI locations in the selected `year``
#' @note U.S. context.
#' @importFrom terra vect
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
#' @export
process_tri <-
  function(
    path = "./input/tri/",
    year = 2018L
  ) {
    if (!is.numeric(radius)) {
      stop("radius should be numeric.\n")
    }

    csvs_tri_path <-
      list.files(path = path, pattern = "*.csv$", full.names = TRUE)
    csvs_tri <- lapply(csvs_tri_path, read.csv)
    col_sel <- c(1, 13, 12, 14, 20, 34, 36, 47, 48, 49)
    csvs_tri <- data.table::rbindlist(csvs_tri)
    dt_tri <- csvs_tri[, col_sel, with = FALSE]

    # column name readjustment
    tri_cns <- colnames(dt_tri)
    tri_cns <- sub(".*?\\.\\.", "", tri_cns)
    tri_cns <- sub("^[^A-Za-z]*", "", tri_cns)
    tri_cns <- gsub("\\.", "_", tri_cns)
    dt_tri <- setNames(dt_tri, tri_cns)

    # depending on the way the chemicals are summarized
    # Unit is kilogram
    # nolint start
    dt_tri_x <-
      dt_tri |>
      dplyr::filter(YEAR == year) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::ends_with("_AIR"),
          ~ifelse(UNIT_OF_MEASURE == "Pounds", . / 453.592 / 1e3, . / 1e3)
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

    spvect_tri <-
      terra::vect(
        dt_tri_x,
        geom = c("LONGITUDE", "LATITUDE"),
        crs = "EPSG:4269", # all are NAD83
        keepgeom = TRUE
      )

  return(spvect_tri)
}
# nolint end

#' Process raw National Emission Inventory (NEI) data
#' @description NEI data comprises multiple csv files where emissions of
#' 50+ pollutants are recorded at county level in ten EPA regions.
#' This function will return a combined table of NEI data.
#' @param path character(1). Path to the directory with NEI CSV files
#' @param year integer(1). Data year.
#'  Currently only accepts `c(2017, 2020)`
#' @param auxfile character(1) or SpatVector.
#' Path to or SpatVector object of the county boundaries in `year`
#' @author Insang Song, Ranadeep Daw
#' @returns SpatVector object. The third column stores the total emission.
#' @importFrom methods is
#' @importFrom data.table .SD
#' @importFrom data.table fread
#' @importFrom data.table rbindlist
#' @export
process_nei <-
  function(
    path = "./input/nei/",
    year = 2017,
    auxfile
  ) {
    if (!year %in% c(2017, 2020)) {
      stop("year should be one of 2017 or 2020.\n")
    }
    if (is.null(auxfile)) {
      stop("auxfile should be provided. Put the right path to the
      county boundary (polygon) file.")
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
    csvs_nei$geoid <- sprintf("%05d", as.integer(csvs_nei$geoid))
    csvs_nei <-
      csvs_nei[, list(
        TRF_NEINP_0_00000 = sum(emissions_total_ton, na.rm = TRUE)
      ),
      by = geoid]
    csvs_nei$Year <- year

    # read county vector
    if (is.character(auxfile)) {
      cnty_vect <- terra::vect(auxfile)
    }
    cnty_geoid_guess <- grep("GEOID", names(cnty_vect))
    names(cnty_vect)[cnty_geoid_guess] <- "geoid"
    cnty_vect$geoid <- sprintf("%05d", as.integer(cnty_vect$geoid))
    cnty_vect <- merge(cnty_vect, csvs_nei, by = "geoid")
    cnty_vect <- cnty_vect[, c("geoid", "Year", "TRF_NEINP_0_00000")]

    yearabbr <- substr(year, 3, 4)
    names(cnty_vect)[3] <- sub("NP", yearabbr, names(cnty_vect)[3])

    return(cnty_vect)
}


