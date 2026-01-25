# Run all tests within a single file from `tests/testthat/` directory with the `container.sif` container.

Run all tests within a single file from `tests/testthat/` directory with
the `container.sif` container.

## Usage

``` r
test(pattern = NULL)
```

## Arguments

- pattern:

  A regular expression to match the test file name.

## Value

NULL; Prints the output of the `testthat` tests.

## See also

[`testthat::test_file()`](https://testthat.r-lib.org/reference/test_file.html)
