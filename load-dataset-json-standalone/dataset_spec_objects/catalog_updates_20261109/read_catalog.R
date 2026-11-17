# Preserve YAML integer byte counts beyond R's 32-bit integer range. These
# catalog counts fit exactly in an IEEE-754 double; reject values that do not.
read_catalog_yaml <- function(path) {
  overflow <- character()
  result <- yaml::read_yaml(
    path,
    handlers = list(int = function(text) {
      value <- as.numeric(text)
      if (!is.finite(value) || abs(value) > 2^53 - 1) {
        # yaml catches errors raised inside handlers and falls back to its
        # lossy default. Record the error here and reject after parsing.
        overflow <<- c(overflow, text)
        return(NA_real_)
      }
      if (abs(value) <= .Machine$integer.max) as.integer(value) else value
    })
  )
  if (length(overflow)) {
    stop("Catalog integer cannot be represented exactly in R: ",
         paste(overflow, collapse = ", "))
  }
  result
}
