% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{download_cropscape}
\alias{download_cropscape}
\title{Download CropScape data}
\usage{
download_cropscape(
  year = seq(1997, 2023),
  source = c("USDA", "GMU"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE,
  hash = FALSE
)
}
\arguments{
\item{year}{integer(1). Year of the data to download.}

\item{source}{character(1). Data source, one of \code{c("USDA", "GMU")}.
\itemize{
\item \code{"USDA"} will download the national data from the USDA website (available in 2008-last year).
\item \code{"GMU"} will download the data from the George Mason University website (available in 1997-last year).
}}

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

\item{unzip}{logical(1). Unzip the downloaded compressed files.
Default is \code{FALSE}.}

\item{hash}{logical(1). By setting \code{TRUE} the function will return
an \code{rlang::hash_file()} hash character corresponding to the
downloaded files. Default is \code{FALSE}.}
}
\value{
\itemize{
\item For \code{hash = FALSE}, NULL
\item For \code{hash = TRUE}, an \code{rlang::hash_file} character.
\item Yearly comma-separated value (CSV) files will be stored in
\code{directory_to_save}.
}
}
\description{
Accesses and downloads United States Department of Agriculture
CropScape Cropland Data Layer data from
the \href{https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php}{USDA National Agricultural Statistics Service} or the
\href{https://nassgeodata.gmu.edu/CropScape/}{George Mason University website}.
}
\note{
JSON files should be found at STAC catalog of OpenLandMap
}
\examples{
\dontrun{
download_cropscape(
  year = 2020,
  source = "USDA",
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
}
}
\author{
Insang Song
}
