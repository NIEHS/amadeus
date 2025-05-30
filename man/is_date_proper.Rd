% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_auxiliary.R
\name{is_date_proper}
\alias{is_date_proper}
\title{Check date format}
\usage{
is_date_proper(instr = NULL, format = "\%Y-\%m-\%d")
}
\arguments{
\item{instr}{character(1). String to check.}

\item{format}{character(1). Matching format to be checked.
Default is \code{"\%Y-\%m-\%d"}, which can detect \verb{"\%Y/\%m/\%d}.
See \code{\link{strftime}} for details of formatting this string.}
}
\value{
No returning value. It stops the function if \code{instr} doesn't
conform to the \code{format}.
}
\description{
Check date input strings conform to the required format.
}
\author{
Insang Song
}
\keyword{auxiliary}
\keyword{internal}
