## Testing amadeus

In order to avoid issues which arise from mismatched dependencies between geospatial and machine learning packages on NIEHS HPC, `amadeus` tests should be run through a container.
Testing within a containerized environment will ensure `amadeus` functions are not developed according to the NIEHS HPC system, which is an exception, but to the normal installations of `sf`, `terra`, and their dependencies.

### tests/container/

The `tests/container/` folder contains the Apptainer definition file (`container.def`), the image building script (`build_container.sh`), and the testing run script (`test-local.sh`).
To run `amadeus` tests in the containerized environment, first build the container with `build_container.sh`.

```sh
cd path/to/amadeus/tests/container
sh build_container.sh
```

Once the container image is built, the tests can be run with `test-local.sh` in `bash` 

```sh
cd /path/to/amadeus/tests/container
sh test-local.sh
```

or from `R`

```r
setwd("path/to/amadeus/")
system("sh tests/container/test-local.sh")
```

Currently, `test-local.sh` is written to run the `covr::package_coverage` function, running all of the tests in the `tests/testthat/` folder.

```sh
apptainer exec \
  --bind $PWD:/mnt \
  --bind /tmp:/opt/tmp \
  container/container.sif Rscript -e \
  ".libPaths(); \
   library(amadeus); \
   covr::package_coverage(quiet = FALSE)"
```

`test-local.sh` can also be adapted to test smaller groups or individual files within the `tests/testthat/` folder.
The following example runs only `tests/testthat/test-modis.R`.

```sh
apptainer exec \
  --bind $PWD:/mnt \
  --bind /tmp:/opt/tmp \
  container/container.sif Rscript -e \
  ".libPaths(); \
   library(amadeus); \
   test_files <- list.files('/mnt/tests/testthat', full.names = TRUE)[2:28]; \
   test_files <- grep('modis', test_files, value = TRUE, invert = FALSE); \
   source_files <- list.files('/mnt/R', full.names = TRUE); \
   covr::file_coverage(test_files, source_files)"
```
