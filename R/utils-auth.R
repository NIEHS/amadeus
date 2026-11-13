################################################################################
# Authentication-related helpers.
################################################################################

################################################################################
#' Set up NASA EarthData authentication
#' @description
#' Interactive helper to securely set up NASA EarthData authentication.
#' This function guides users through setting up their token in a secure way
#' that won't be exposed in scripts or version control.
#' @param method character(1). Setup method:
#'   - "renviron": Add to ~/.Renviron (recommended, persists across sessions)
#'   - "file": Save to ~/.nasa_earthdata_token file
#'   - "session": Set for current R session only
#' @param token character(1). Your NASA EarthData token. If NULL, will prompt.
#' @return invisible(NULL). Sets up authentication.
#' @examples
#' \dontrun{
#' # Interactive setup (recommended)
#' setup_nasa_token()
#'
#' # Save to .Renviron for permanent setup
#' setup_nasa_token(method = "renviron", token = "your_token_here")
#'
#' # Save to file
#' setup_nasa_token(method = "file", token = "your_token_here")
#'
#' # Current session only
#' setup_nasa_token(method = "session", token = "your_token_here")
#' }
#' @export
setup_nasa_token <- function(
  method = c("renviron", "file", "session"),
  token = NULL
) {
  # ----------------------------------------------------------------------------
  # Check the token storage method.
  method <- match.arg(method)
  # ----------------------------------------------------------------------------
  # Prompt for a token when none is supplied.
  if (is.null(token)) {
    if (interactive()) {
      cat("Enter your NASA EarthData token: ")
      token <- readline()
      token <- trimws(token)
    } else {
      stop("Token must be provided in non-interactive mode.\n", call. = FALSE)
    }
  }
  # ----------------------------------------------------------------------------
  # Reject an empty token.
  if (!nzchar(token)) {
    stop("Token cannot be empty.\n", call. = FALSE)
  }
  # ----------------------------------------------------------------------------
  # Store the token using the selected method.
  switch(
    method,
    renviron = {
      # ------------------------------------------------------------------------
      # Locate the user environment file.
      renviron_path <- path.expand("~/.Renviron")
      # ------------------------------------------------------------------------
      # Read existing settings and remove previous token entries.
      if (file.exists(renviron_path)) {
        renviron_lines <- readLines(renviron_path)
        renviron_lines <- renviron_lines[
          !grepl("^NASA_EARTHDATA_TOKEN=", renviron_lines)
        ]
      } else {
        renviron_lines <- character(0)
      }
      # ------------------------------------------------------------------------
      # Save the token alongside the existing environment settings.
      renviron_lines <- c(
        renviron_lines,
        paste0("NASA_EARTHDATA_TOKEN=", token)
      )
      writeLines(renviron_lines, renviron_path)
      # ------------------------------------------------------------------------
      # Report where the token was saved and how to load it.
      message(sprintf(
        "Token saved to %s\n",
        renviron_path
      ))
      message(
        "  Restart R for changes to take effect,",
        " or run: readRenviron('~/.Renviron')\n"
      )
    },
    file = {
      # ------------------------------------------------------------------------
      # Save the token in the user token file.
      token_path <- path.expand("~/.nasa_earthdata_token")
      writeLines(token, token_path)
      # ------------------------------------------------------------------------
      # Restrict file access to the owner on Unix-like systems.
      if (.Platform$OS.type != "windows") {
        Sys.chmod(token_path, mode = "0600")
      }
      # ------------------------------------------------------------------------
      # Report where the token was saved and how to use it.
      message(sprintf("Token saved to %s\n", token_path))
      message(
        "  Use in functions:",
        " nasa_earth_data_token = '~/.nasa_earthdata_token'\n"
      )
    },
    session = {
      # ------------------------------------------------------------------------
      # Set the token for the current session and report its lifetime.
      Sys.setenv(NASA_EARTHDATA_TOKEN = token)
      message("Token set for current R session\n")
      message(
        "  This will be lost when you close R.",
        " Use method='renviron' for permanent setup.\n"
      )
    }
  )
  # ----------------------------------------------------------------------------
  # Return invisibly after configuring authentication.
  invisible(NULL)
}

################################################################################
#' Get authentication token from various sources
#' @description
#' Retrieves authentication token from environment variable, file, or direct
#' input.
#' Priority order: 1) Environment variable, 2) File path, 3) Direct token
#' string.
#' This function helps prevent accidental token exposure in code or logs.
#' @param token character(1) or NULL. Can be:
#'   - NULL: reads from environment variable (recommended)
#'   - File path: reads token from file
#'   - Token string: uses directly (not recommended for scripts)
#' @param env_var character(1). Name of environment variable containing token.
#'   Default is "NASA_EARTHDATA_TOKEN"
#' @return character(1). The authentication token
#' @keywords internal
#' @export
get_token <- function(token = NULL, env_var = "NASA_EARTHDATA_TOKEN") {
  # ----------------------------------------------------------------------------
  # Use the environment variable first when it contains a token.
  token_env <- Sys.getenv(env_var, unset = NA)
  if (!is.na(token_env) && nzchar(token_env)) {
    message(sprintf("Using token from environment variable: %s\n", env_var))
    return(trimws(token_env))
  }
  # ----------------------------------------------------------------------------
  # Check the supplied value when no environment token is available.
  if (!is.null(token)) {
    # --------------------------------------------------------------------------
    # Read and validate the token when the supplied value is a file path.
    if (length(token) == 1 && file.exists(token)) {
      message(sprintf("Reading token from file: %s\n", token))
      token_file <- trimws(readLines(token, n = 1, warn = FALSE))
      if (length(token_file) == 0 || !nzchar(token_file)) {
        stop(sprintf("Token file '%s' is empty.\n", token))
      }
      return(token_file)
    }
    # --------------------------------------------------------------------------
    # Use a supplied token string without printing its contents.
    if (length(token) == 1 && nzchar(token)) {
      message("Using provided token string.\n")
      return(trimws(token))
    }
  }
  # ----------------------------------------------------------------------------
  # Explain how to provide a token when no source contains one.
  stop(
    "No authentication token found. Please provide a token using one of:\n",
    sprintf(
      "  1. Set environment variable: Sys.setenv(%s = 'your_token')\n",
      env_var
    ),
    "  2. Create ~/.nasa_earthdata_token file with your token\n",
    paste0(
      "  3. Pass token file path:",
      " nasa_earth_data_token = '~/.nasa_earthdata_token'\n"
    ),
    "  4. Pass token directly:",
    " nasa_earth_data_token = 'your_token' (not recommended)\n",
    sprintf(
      paste0(
        "\nTo set up for all R sessions,",
        " add to ~/.Renviron:\n  %s=your_token_here\n"
      ),
      env_var
    ),
    call. = FALSE
  )
}
