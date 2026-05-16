# Get authentication token from various sources

Retrieves authentication token from environment variable, file, or
direct input. Priority order: 1) Environment variable, 2) File path, 3)
Direct token string. This function helps prevent accidental token
exposure in code or logs.

## Usage

``` r
get_token(token = NULL, env_var = "NASA_EARTHDATA_TOKEN")
```

## Arguments

- token:

  character(1) or NULL. Can be:

  - NULL: reads from environment variable (recommended)

  - File path: reads token from file

  - Token string: uses directly (not recommended for scripts)

- env_var:

  character(1). Name of environment variable containing token. Default
  is "NASA_EARTHDATA_TOKEN"

## Value

character(1). The authentication token
