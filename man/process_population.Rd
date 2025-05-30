% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.R
\name{process_population}
\alias{process_population}
\title{Process population density data}
\usage{
process_population(path = NULL, extent = NULL, ...)
}
\arguments{
\item{path}{character(1). Path to GeoTIFF (.tif) or netCDF (.nc) file.}

\item{extent}{numeric(4) or SpatExtent giving the extent of the raster
if \code{NULL} (default), the entire raster is loaded}

\item{...}{Placeholders.}
}
\value{
a \code{SpatRaster} object
}
\description{
The \code{process_secac_population()} function imports and cleans raw
population density data, returning a single \code{SpatRaster} object.
}
\examples{
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
\dontrun{
pop <- process_population(
  path = "./data/sedac_population_example.tif"
)
}
}
\author{
Mitchell Manware
}
