% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_auxiliary.R
\name{download_hash}
\alias{download_hash}
\title{Create has of downloaded files.}
\usage{
download_hash(hash = TRUE, dir = NULL)
}
\arguments{
\item{hash}{logical(1). Create hash of downloaded files.}

\item{dir}{character(1). Directory path.}
}
\value{
character(1) \code{rlang::hash} of downloaded files.
}
\description{
Create \code{rlang::hash_file} of the downloaded files.
}
\keyword{auxiliary}
\keyword{internal}
