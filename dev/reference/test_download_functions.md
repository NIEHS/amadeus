# Download unit tests

Implement directory, file, and download URL unit tests.

## Usage

``` r
test_download_functions(
  directory_to_save = directory_to_save,
  commands_path = commands_path,
  url_status = url_status
)
```

## Arguments

- directory_to_save:

  directory to test saving

- commands_path:

  file path with download commands

- url_status:

  logical vector for URL status = 200

## Value

NULL; returns stop error if one or more tests fail
