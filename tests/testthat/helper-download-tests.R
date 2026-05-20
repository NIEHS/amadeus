# Download test helpers used only by testthat files.
test_download_functions <- function(
  directory_to_save = directory_to_save,
  commands_path = commands_path,
  url_status = url_status
) {
  testthat::expect_true(dir.exists(directory_to_save))
  testthat::expect_true(file.exists(commands_path))
  if (!is.null(url_status)) {
    testthat::expect_true(all(url_status))
  }
}
