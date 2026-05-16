# Set up NASA EarthData authentication

Interactive helper to securely set up NASA EarthData authentication.
This function guides users through setting up their token in a secure
way that won't be exposed in scripts or version control.

## Usage

``` r
setup_nasa_token(method = c("renviron", "file", "session"), token = NULL)
```

## Arguments

- method:

  character(1). Setup method:

  - "renviron": Add to ~/.Renviron (recommended, persists across
    sessions)

  - "file": Save to ~/.nasa_earthdata_token file

  - "session": Set for current R session only

- token:

  character(1). Your NASA EarthData token. If NULL, will prompt.

## Value

invisible(NULL). Sets up authentication.

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive setup (recommended)
setup_nasa_token()

# Save to .Renviron for permanent setup
setup_nasa_token(method = "renviron", token = "your_token_here")

# Save to file
setup_nasa_token(method = "file", token = "your_token_here")

# Current session only
setup_nasa_token(method = "session", token = "your_token_here")
} # }
```
