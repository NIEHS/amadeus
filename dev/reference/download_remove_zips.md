# Remove zip files

Remove downloaded ".zip" files.

## Usage

``` r
download_remove_zips(remove = FALSE, download_name)
```

## Arguments

- remove:

  logical(1). Confirm removal. Default is FALSE.

- download_name:

  character. Full zip file path

## Value

NULL; removes downloaded zip files after they are unzipped

## Note

!!! USE THE FUNCTION WITH CAUTION !!! If `remove = TRUE`, ensure that
`unzip = TRUE`. Choosing to remove ".zip" files without unzipping will
retain none of the downloaded data. then it will remove all files in the
second higher level directory.
