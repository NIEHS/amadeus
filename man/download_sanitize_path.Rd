% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_auxiliary.R
\name{download_sanitize_path}
\alias{download_sanitize_path}
\title{Sanitize directory}
\usage{
download_sanitize_path(directory)
}
\arguments{
\item{directory}{character(1). Path}
}
\value{
character ending with a forward slash.
}
\description{
Append forward slash to end of \code{directory} if it does not already
end with one.
}
\keyword{internal}
