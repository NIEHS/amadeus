################################################################################
testthat::test_that("metadata_hrrr(variable = NULL): returns a standalone catalogue", {
  info <- metadata_hrrr()
  testthat::expect_s3_class(info, "data.frame")
  testthat::expect_named(
    info,
    c("short_name", "variable", "long_name", "level", "units", "layer_name",
      "products")
  )
  testthat::expect_identical(anyDuplicated(info$short_name), 0L)
  testthat::expect_identical(
    anyDuplicated(info[, c("variable", "level")]),
    0L
  )
  testthat::expect_match(info$short_name, "^[A-Z0-9]+_[A-Z0-9]+$")
  testthat::expect_identical(
    info$short_name,
    sort(info$short_name, method = "radix")
  )
  for (column in info) {
    testthat::expect_type(column, "character")
    testthat::expect_false(anyNA(column))
    testthat::expect_false(any(!nzchar(column)))
  }
  testthat::expect_setequal(
    unique(unlist(strsplit(info$products, ";", fixed = TRUE))),
    c("prs", "nat", "sfc", "subh")
  )
  expected <- utils::read.csv(
    system.file(
      "extdata", "df_hrrr.csv", package = "amadeus", mustWork = TRUE
    ),
    stringsAsFactors = FALSE
  )
  expected$layer_name <- gsub(
    "{accumulation_prefix}", "", expected$layer_name, fixed = TRUE
  )
  testthat::expect_identical(info, expected)
})

################################################################################
testthat::test_that("metadata_hrrr(variable = codes): preserves request order", {
  info <- metadata_hrrr(c("ugrd", "TMP", "UGRD", "dpt"))
  testthat::expect_identical(unique(info$variable), c("UGRD", "TMP", "DPT"))
  testthat::expect_identical(
    unique(info$long_name),
    c("U-Component of Wind", "Temperature", "Dew Point Temperature")
  )
  testthat::expect_identical(unique(info$units), c("m/s", "K"))
  testthat::expect_identical(unique(metadata_hrrr("tmp")$variable), "TMP")
  testthat::expect_s3_class(metadata_hrrr("TMP"), "data.frame")
  testthat::expect_identical(
    info[info$variable == "TMP", "level"],
    metadata_hrrr("TMP")$level
  )
})

################################################################################
testthat::test_that("metadata_hrrr(variable = REFC): separates display and labels", {
  info <- metadata_hrrr("REFC")
  testthat::expect_identical(info$long_name, "Composite reflectivity")
  testthat::expect_identical(
    info$layer_name,
    "EATM=Entire Atmosphere; Maximum / Composite radar reflectivity [dB]"
  )
  testthat::expect_identical(metadata_hrrr("VIL")$short_name, "VIL_EATM")
  testthat::expect_identical(
    unique(metadata_hrrr("CLWMR")$long_name),
    "Cloud Mixing Ratio"
  )
})

################################################################################
testthat::test_that("metadata_hrrr(variable = TMP, level = 2 m): selects one field", {
  # Include multiple levels and temperature variables sharing a substring.
  layer_names <- c(
    "2[m] HTGL=Specified height level above ground; Temperature [C]",
    "2[m] HTGL=Specified height level above ground; Dew point temperature [C]",
    "2[m] HTGL=Specified height level above ground; Potential temperature [C]",
    "85000[Pa] ISBL=Isobaric surface; Temperature [C]",
    "0[m] DBLL=Depth below land surface; Soil temperature [C]",
    "SFC=Ground or water surface; Temperature [C]"
  )
  info <- metadata_hrrr("TMP", level = "2 m above ground")
  testthat::expect_identical(nrow(info), 1L)
  testthat::expect_identical(info$short_name, "TMP_2MAGL")
  testthat::expect_identical(info$variable, "TMP")
  testthat::expect_identical(info$level, "2 m above ground")
  testthat::expect_identical(
    which(layer_names %in% info$layer_name),
    1L
  )
  testthat::expect_identical(
    which(layer_names %in% metadata_hrrr("TMP")$layer_name),
    c(1L, 4L, 6L)
  )
})

################################################################################
testthat::test_that("metadata_hrrr(variable = field IDs): resolves unique fields", {
  ids <- c("TMP_2MAGL", "TMP_SFC", "TMP_850MB", "TMP_HYBRID1", "TMP_1013P2MB")
  info <- metadata_hrrr(tolower(ids))
  testthat::expect_identical(info$short_name, ids)
  testthat::expect_identical(
    info$level,
    c("2 m above ground", "surface", "850 mb", "1 hybrid level", "1013.2 mb")
  )
  testthat::expect_identical(
    metadata_hrrr("TMP_2MAGL"),
    metadata_hrrr("TMP", "2 m above ground")
  )
  testthat::expect_identical(
    metadata_hrrr("SOILW_0P01TO0P01MBGL")$level,
    "0.01-0.01 m below ground"
  )
  testthat::expect_identical(
    metadata_hrrr("CAPE_0TO3000MAGL")$level,
    "0-3000 m above ground"
  )
})

################################################################################
testthat::test_that("metadata_hrrr(variable = mixed): avoids duplicate fields", {
  info <- metadata_hrrr(c("TMP_850MB", "DPT_2MAGL", "TMP", "tmp_850mb"))
  testthat::expect_identical(
    info$short_name[1:2],
    c("TMP_850MB", "DPT_2MAGL")
  )
  testthat::expect_identical(anyDuplicated(info$short_name), 0L)
  testthat::expect_setequal(
    info$short_name,
    c(metadata_hrrr("TMP")$short_name, "DPT_2MAGL")
  )
})

################################################################################
testthat::test_that("metadata_hrrr(variable = TMP): supplies distinct layer names", {
  info <- metadata_hrrr(c("TMP_2MAGL", "TMP_SFC", "TMP_850MB"))
  layer_names <- paste0(info$short_name, "_20210504_0200")
  testthat::expect_identical(
    layer_names,
    c("TMP_2MAGL_20210504_0200", "TMP_SFC_20210504_0200",
      "TMP_850MB_20210504_0200")
  )
  components <- strsplit(layer_names, "_", fixed = TRUE)
  testthat::expect_identical(lengths(components), rep(4L, 3L))
  testthat::expect_identical(
    vapply(components, `[`, character(1), 2L),
    c("2MAGL", "SFC", "850MB")
  )
})

################################################################################
testthat::test_that("metadata_hrrr(variable = TMP): retains distinct levels", {
  info <- metadata_hrrr("TMP")
  testthat::expect_identical(
    intersect(c("2 m above ground", "surface", "850 mb"), info$level),
    c("2 m above ground", "surface", "850 mb")
  )
  testthat::expect_setequal(
    info$level[grepl("hybrid level$", info$level)],
    paste(1:50, "hybrid level")
  )
  testthat::expect_identical(
    metadata_hrrr("TMP", "850 mb")$layer_name,
    "85000[Pa] ISBL=Isobaric surface; Temperature [C]"
  )
  testthat::expect_identical(
    metadata_hrrr("TMP", "1013.2 mb")$layer_name,
    "101320[Pa] ISBL=Isobaric surface; Temperature [C]"
  )
  testthat::expect_identical(
    metadata_hrrr("TMP", "1 hybrid level")$layer_name,
    "1[-] HYBL=Hybrid level; Temperature [C]"
  )
})

################################################################################
testthat::test_that("metadata_hrrr(variable = TMP): reports products by level", {
  testthat::expect_identical(
    metadata_hrrr("TMP", "2 m above ground")$products,
    "prs;nat;sfc;subh"
  )
  testthat::expect_identical(
    metadata_hrrr("TMP", "850 mb")$products,
    "prs;sfc"
  )
  testthat::expect_identical(
    metadata_hrrr("TMP", "1 hybrid level")$products,
    "nat"
  )
})

################################################################################
testthat::test_that("metadata_hrrr(level = uppercase): ignores case", {
  info <- metadata_hrrr("tmp", c("850 MB", "2 M ABOVE GROUND", "850 mb"))
  testthat::expect_identical(nrow(info), 2L)
  testthat::expect_setequal(info$level, c("850 mb", "2 m above ground"))
  testthat::expect_identical(
    unique(metadata_hrrr(level = "2 M ABOVE GROUND")$level),
    "2 m above ground"
  )
})

################################################################################
testthat::test_that("metadata_hrrr(variable = invalid): rejects invalid selectors", {
  testthat::expect_error(metadata_hrrr(1), regexp = "variable.*character")
  testthat::expect_error(metadata_hrrr(NA_character_), regexp = "variable.*missing")
  testthat::expect_error(metadata_hrrr(character()), regexp = "variable.*length")
  testthat::expect_error(metadata_hrrr(""), regexp = "variable.*character")
  testthat::expect_error(metadata_hrrr("not_a_variable"), regexp = "variable.*subset")
  testthat::expect_error(
    metadata_hrrr(c("TMP", "not_a_variable")),
    regexp = "variable.*subset"
  )
  testthat::expect_error(
    metadata_hrrr("TMP_2MBGL"),
    regexp = "variable.*subset"
  )
})

################################################################################
testthat::test_that("metadata_hrrr(level = invalid): rejects invalid selections", {
  testthat::expect_error(metadata_hrrr(level = 2), regexp = "level.*character")
  testthat::expect_error(
    metadata_hrrr(level = NA_character_),
    regexp = "level.*missing"
  )
  testthat::expect_error(metadata_hrrr(level = character()), regexp = "level.*length")
  testthat::expect_error(metadata_hrrr(level = ""), regexp = "level.*character")
  testthat::expect_error(metadata_hrrr("TMP", "not_a_level"), regexp = "level.*subset")
  testthat::expect_error(
    metadata_hrrr("TMP", c("2 m above ground", "not_a_level")),
    regexp = "level.*subset"
  )
  testthat::expect_error(metadata_hrrr("TMP", "mean sea level"), regexp = "level.*subset")
  testthat::expect_error(
    metadata_hrrr(c("TMP", "DPT"), "surface"),
    regexp = "No requested levels found for variable\\(s\\): DPT"
  )
  testthat::expect_error(
    metadata_hrrr("TMP_2MAGL", "850 mb"),
    regexp = "level.*subset"
  )
  testthat::expect_error(
    metadata_hrrr(c("TMP_2MAGL", "TMP_850MB"), "2 m above ground"),
    regexp = "No requested levels found for variable\\(s\\): TMP_850MB"
  )
})
