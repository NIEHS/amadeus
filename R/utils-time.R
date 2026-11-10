################################################################################
# Time-related helpers.
################################################################################

################################################################################
#' Generate an inclusive sequence of years
#' @description
#' Validate all supplied years and generate every year from the minimum
#' through the maximum, including both endpoints.
#' @param year numeric(1 or more). A non-empty vector of non-missing,
#'   integer-valued years between 1900 and the current calendar year.
#'   A single year, two endpoints, or a longer vector may be supplied in
#'   any order; repeated years are accepted.
#' @details Gaps are filled automatically: `c(2018, 2023)` and
#'   `c(2018, 2020, 2023)` both return `2018:2023`.
#'   Source-specific data availability limits are checked by the caller.
#' @return Numeric vector of consecutive years in ascending order, including
#'   both endpoints. A single input year returns one value.
#' @keywords internal
#' @noRd
sequence_years <- function(year) {
  # ----------------------------------------------------------------------------
  # Validate every supplied year before selecting the range.
  checkmate::assert_integerish(
    year,
    min.len = 1L,
    lower = 1900,
    upper = as.integer(format(Sys.Date(), "%Y")),
    any.missing = FALSE
  )
  # ----------------------------------------------------------------------------
  # Find the earliest and latest years.
  int_years <- range(year)
  # ----------------------------------------------------------------------------
  # Generate the inclusive sequence.
  seq(int_years[1L], int_years[2L], by = 1)
}

################################################################################
#' Generate an inclusive sequence of dates
#' @description
#' Generate consecutive daily dates from a single date or two range endpoints.
#' Range endpoints are ordered before generating the sequence.
#' @param date character(1 or 2). A single date or two range endpoints in
#' `"YYYY-MM-DD"` format. Endpoints may be supplied in either order.
#' @param hyphen logical(1). Include hyphens in the returned date strings.
#' If `FALSE` (the default), return `"YYYYMMDD"`; if `TRUE`, return
#' `"YYYY-MM-DD"`.
#' @return A character vector of daily dates in ascending order, including
#' both endpoints. A single input date returns one date string. The output
#' format is controlled by `hyphen`.
#' @author Mitchell Manware
#' @seealso [base::seq.Date()], [base::as.Date()]
#' @examples
#' sequence_dates("2020-05-05")
#' sequence_dates(c("2020-05-07", "2020-05-05"))
#' sequence_dates(c("2020-05-05", "2020-05-07"), hyphen = TRUE)
#' @keywords internal auxiliary
#' @export
sequence_dates <- function(date, hyphen = FALSE) {
  # ----------------------------------------------------------------------------
  # Ensure `hyphen` is TRUE/FALSE.
  checkmate::assert_flag(hyphen)
  # ----------------------------------------------------------------------------
  # Repeat date if only one.
  chr_dates <- if (length(date) == 1) rep(date, 2) else date
  # ----------------------------------------------------------------------------
  # Ensure `date` is length 2.
  checkmate::assert_character(chr_dates, n.chars = 10, len = 2)
  # ----------------------------------------------------------------------------
  # Order dates.
  chr_dates <- sort(chr_dates)
  # ----------------------------------------------------------------------------
  # Ensure valid date range.
  checkmate::assert_date(
    as.Date(chr_dates, "%Y-%m-%d"),
    lower = as.Date("1950-01-01"),
    upper = Sys.Date()
  )
  # ----------------------------------------------------------------------------
  # Generate sequence.
  date_seq <- as.character(
    seq(
      as.Date(chr_dates[1], format = "%Y-%m-%d"),
      as.Date(chr_dates[2], format = "%Y-%m-%d"),
      "day"
    )
  )
  # ----------------------------------------------------------------------------
  # Format dates according to `hyphen`.
  if (hyphen) date_seq else gsub("-", "", as.character(date_seq))
}

################################################################################
#' Generate a daily sequence of clock times
#' @description
#' Generate equally spaced clock times throughout a day, starting at an
#' offset from midnight and ending before the next midnight, then retain
#' only the requested hours.
#' @param by integer(1). Positive interval in minutes. Default is 60.
#' @param offset integer(1). Minutes after midnight at which the sequence
#' starts, from 0 through 1439. Default is 0.
#' @param colon logical(1). Include a colon in returned time strings.
#' If `FALSE` (the default), return `"HHMM"`; if `TRUE`, return `"HH:MM"`.
#' @param hours integer vector. Non-empty selection of hours from 0 through
#' 23 to retain. Default is `0:23`, which retains all hours. Input order and
#' repeated hours do not affect the output.
#' @details The interval and offset define the daily schedule before filtering
#' by `hours`; selecting hours does not restart the interval at each hour.
#' @return A character vector of retained times in ascending order. Times never
#' extend past `"23:59"` or wrap into the next day. Returns `character(0)` when
#' no scheduled times fall within the requested hours.
#' @examples
#' sequence_times()
#' sequence_times(by = 60, offset = 30)
#' sequence_times(by = 90, offset = 45, colon = TRUE)
#' sequence_times(offset = 30, hours = c(0, 6, 12, 18))
#' @keywords internal
#' @noRd
sequence_times <- function(
  by = 60L,
  offset = 0L,
  colon = FALSE,
  hours = 0:23
) {
  # ----------------------------------------------------------------------------
  # Check output format, interval, starting offset, and selected hours.
  checkmate::assert_flag(colon)
  checkmate::assert_int(by, lower = 1L)
  checkmate::assert_int(offset, lower = 0L, upper = 1439L)
  checkmate::assert_integerish(
    hours,
    lower = 0L,
    upper = 23L,
    min.len = 1L,
    any.missing = FALSE
  )
  # ----------------------------------------------------------------------------
  # Generate the sequence in minutes and retain the requested hours.
  int_seq <- seq.int(from = offset, to = 1439L, by = by)
  int_seq <- int_seq[int_seq %/% 60L %in% hours]
  # ----------------------------------------------------------------------------
  # Format clock times.
  sprintf(
    if (colon) "%02d:%02d" else "%02d%02d",
    int_seq %/% 60L,
    int_seq %% 60L
  )
}
