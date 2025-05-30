% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{download_prism}
\alias{download_prism}
\title{Download PRISM data}
\usage{
download_prism(
  time,
  element = c("ppt", "tmin", "tmax", "tmean", "tdmean", "vpdmin", "vpdmax", "solslope",
    "soltotal", "solclear", "soltrans"),
  data_type = c("ts", "normals_800", "normals"),
  format = c("nc", "asc", "grib2"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  hash = FALSE
)
}
\arguments{
\item{time}{character(1). Length of 2, 4, 6, or 8. Time period for
time series or normals. According to the PRISM Web Service Guide,
acceptable formats include (disclaimer: the following is a direct quote;
minimal formatting is applied):
\strong{Time Series}:
\itemize{
\item \code{YYYYMMDD} for daily data (between yesterday and January 1st, 1981) – returns a single grid in a .zip file
\item \code{YYYYMM} for monthly data (between last month and January 1981) – returns a single grid in a .zip file
\item \code{YYYY} for annual data (between last year and 1981) - returns a single grid in a .zip file
\item \code{YYYY} for historical data (between 1980 and 1895) - returns a single zip file containing 12 monthly grids for \code{YYYY} plus the annual.
}

\strong{Normals}:
\itemize{
\item Monthly normal: date is \code{MM} (i.e., 04 for April) or the value 14, which returns the annual normal
\item Daily normal: date is \code{MMDD} (i.e., 0430 for April 30)
}}

\item{element}{character(1). Data element.
One of \code{c("ppt", "tmin", "tmax", "tmean", "tdmean", "vpdmin", "vpdmax")}
For normals, \code{c("solslope", "soltotal", "solclear", "soltrans")} are also accepted.}

\item{data_type}{character(1). Data type.
\itemize{
\item \code{"ts"}: 4km resolution time series.
\item \code{"normals_800"}: 800m resolution normals.
\item \code{"normals"}: 4km resolution normals.
}}

\item{format}{character(1). Data format. Only applicable for \code{data_type = "ts"}.}

\item{directory_to_save}{character(1). Directory to download files.}

\item{acknowledgement}{logical(1). By setting \code{TRUE} the
user acknowledges that the data downloaded using this function may be very
large and use lots of machine storage and memory.}

\item{download}{logical(1). \code{FALSE} will generate a *.txt file
containing all download commands. By setting \code{TRUE} the function
will download all of the requested data files.}

\item{remove_command}{logical(1).
Remove (\code{TRUE}) or keep (\code{FALSE})
the text file containing download commands.}

\item{hash}{logical(1). By setting \code{TRUE} the function will return
an \code{rlang::hash_file()} hash character corresponding to the
downloaded files. Default is \code{FALSE}.}
}
\value{
\itemize{
\item For \code{hash = FALSE}, NULL
\item For \code{hash = TRUE}, an \code{rlang::hash_file} character.
\item .bil (normals) or single grid files depending on the format
choice will be stored in \code{directory_to_save}.
}
}
\description{
Accesses and downloads Oregon State University's
PRISM data from the PRISM Climate Group Web Service
}
\examples{
\dontrun{
download_prism(
  time = "202104",
  element = "ppt",
  data_type = "ts",
  format = "nc",
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE
)
}
}
\references{
\insertRef{article_daly2000prism}{amadeus}

\itemize{
\item \href{https://prism.oregonstate.edu/}{PRISM Climate Group}
\item \href{https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf}{PRISM Web Service Guide}
}
}
\author{
Insang Song
}
