args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) > 0) args[[1]] else getwd()
report_dir <- file.path(root, "tests", "test_report")
report_file <- file.path(report_dir, "test_report.Rmd")

if (!file.exists(report_file)) {
  stop("Report source not found: ", report_file)
}

setwd(root)

rmarkdown::render(
  input = report_file,
  output_format = "html_document",
  output_file = "test_report.html",
  output_dir = report_dir,
  knit_root_dir = root,
  quiet = TRUE
)

pdf_capable <- nzchar(Sys.which("pdflatex")) || nzchar(Sys.which("xelatex")) ||
  nzchar(Sys.which("lualatex"))

if (pdf_capable) {
  try(
    rmarkdown::render(
      input = report_file,
      output_format = "pdf_document",
      output_file = "test_report.pdf",
      output_dir = report_dir,
      knit_root_dir = root,
      quiet = TRUE
    ),
    silent = TRUE
  )
}
