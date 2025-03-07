% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_auxiliary.R
\name{download_hash}
\alias{download_hash}
\title{Create hash of downloaded files.}
\usage{
download_hash(hash = TRUE, dir = NULL)
}
\arguments{
\item{hash}{logical(1). Create hash of downloaded files.}

\item{dir}{character(1). Directory path.}
}
\value{
character(1) Combined 128-bit md5sum of download files.
}
\description{
Create a combined md5sum hash based on the files in a specified directory.
}
\keyword{auxiliary}
\keyword{internal}
