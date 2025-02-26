## Testing amadeus

In order to avoid issues which arise from mismatched dependencies between geospatial and machine learning packages on NIEHS HPC, `amadeus` tests should be run through a container.
Testing within a containerized environment will ensure `amadeus` functions are not developed according to the NIEHS HPC system, which is an exception, but to the normal installations of `sf`, `terra`, and their dependencies.

### tests/container/

The `tests/container/` directory contains the Apptainer definition file (`container.def`) and the image building script (`build_container.sh`) To run `amadeus` tests in the containerized environment, first build the container with `build_container.sh`, and move the `container.sif` file to the package root directory.

```sh
cd path/to/amadeus/tests/container
sh build_container.sh
mv container.sif ../../
```

Once the container image is built, the tests can be run with the `test()` and `cov()` helper functions. `test()` requires a pattern, which is used to run all of the tests within a single file from `tests/testthat`. This example will run all the tests in the `tests/testthat/test-geos.R` file.

```r
test("geos")
```

`cov()` does not require a pattern, and will run all of the tests in the `tests/testthat/` directory to calculate the code coverage for the whole package. The code coverage is stored in the `cov/` directory, which is ignored from GitHub.

```r
cov()
```
