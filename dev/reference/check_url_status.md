# Check HTTP status

Check if provided URL returns HTTP status 200 or 206.

## Usage

``` r
check_url_status(url, max_tries = 3L)
```

## Arguments

- url:

  Download URL to be checked.

- max_tries:

  integer(1). Maximum number of retry attempts for transient failures
  (SSL drops, connection resets). Default 3L.

## Value

logical object

## Author

Insang Song; Mitchell Manware; Kyle Messier
