% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_covariates.R
\name{calculate_population}
\alias{calculate_population}
\title{Calculate population density covariates}
\usage{
calculate_population(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  geom = FALSE,
  ...
)
}
\arguments{
\item{from}{SpatRaster(1). Output of \code{process_population()}.}

\item{locs}{data.frame, characater to file path, SpatVector, or sf object.}

\item{locs_id}{character(1). Column within \code{locations} CSV file
containing identifier for each unique coordinate location.}

\item{radius}{integer(1). Circular buffer distance around site locations.
(Default = 0).}

\item{fun}{character(1). Function used to summarize multiple raster cells
within sites location buffer (Default = \code{mean}).}

\item{geom}{FALSE/"sf"/"terra".. Should the function return with geometry?
Default is \code{FALSE}, options with geometry are "sf" or "terra". The
coordinate reference system of the \code{sf} or \code{SpatVector} is that of \code{from.}}

\item{...}{Placeholders}
}
\value{
a data.frame or SpatVector object
}
\description{
Extract population density values at point locations. Returns a
\code{data.frame} object containing \code{locs_id}, year, and population
density variable. Population density variable column name reflects
spatial resolution of \code{from} and circular buffer radius.
}
\examples{
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
\dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
calculate_population(
  from = pop, # derived from process_population() example
  locs = loc,
  locs_id = "id",
  radius = 0,
  fun = "mean",
  geom = FALSE
)
}
}
\seealso{
\code{\link[=process_population]{process_population()}}
}
\author{
Mitchell Manware
}
