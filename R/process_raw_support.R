#' Identify GEOS-CF collection based on user-defined file paths
#' @param path character(1). File path to GEOS-CF data file.
#' @param collection logical(1). Identifies and returns GEOS-CF collection
#' name(s) based on provided file path(s).
#' @param date logical(1). Identifies and returns date sequence (YYYYMMDD) based
#' on provided file path(s).
#' @param datetime logical(1). Identifies and returns date time sequence
#' (YYYYMoMoDDHHMiMi) based on provided file path(s).
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

#' Generate statistic and resolution codes based on GMTED statistic and
#' resolution.
#' @param string character(1). File path to GEOS-CF data file.
#' @param statistic logical(1). Matches statistic to statistic code.
#' @param resolution logical(1). Matches resolution to resolution code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides statistic or resolution code, and returns full length statistic
#' or resolution.
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

#' Generate resolution codes based on NASA SEDAC population resolution.
#' @param string character(1). Resolution name or code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides resolution code, and returns full length resolution.
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
