% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{download_groads}
\alias{download_groads}
\title{Download roads data}
\usage{
download_groads(
  data_region = c("Americas", "Global", "Africa", "Asia", "Europe", "Oceania East",
    "Oceania West"),
  data_format = c("Shapefile", "Geodatabase"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  hash = FALSE
)
}
\arguments{
\item{data_region}{character(1). Data can be downloaded for \code{"Global"},
\code{"Africa"}, \code{"Asia"}, \code{"Europe"}, \code{"Americas"}, \code{"Oceania East"}, and \code{"Oceania West"}.}

\item{data_format}{character(1). Data can be downloaded as \code{"Shapefile"} or
\code{"Geodatabase"}. (Only \code{"Geodatabase"} available for \code{"Global"} region).}

\item{directory_to_save}{character(1). Directory to save data. Two
sub-directories will be created for the downloaded zip files ("/zip_files")
and the unzipped shapefiles ("/data_files").}

\item{acknowledgement}{logical(1). By setting \code{TRUE} the
user acknowledges that the data downloaded using this function may be very
large and use lots of machine storage and memory.}

\item{download}{logical(1). \code{FALSE} will generate a *.txt file
containing all download commands. By setting \code{TRUE} the function
will download all of the requested data files.}

\item{remove_command}{logical(1).
Remove (\code{TRUE}) or keep (\code{FALSE})
the text file containing download commands.}

\item{unzip}{logical(1). Unzip zip files. Default is \code{TRUE}.}

\item{remove_zip}{logical(1). Remove zip files from directory_to_download.
Default is \code{FALSE}.}

\item{hash}{logical(1). By setting \code{TRUE} the function will return
an \code{rlang::hash_file()} hash character corresponding to the
downloaded files. Default is \code{FALSE}.}
}
\value{
\itemize{
\item For \code{hash = FALSE}, NULL
\item For \code{hash = TRUE}, an \code{rlang::hash_file} character.
\item Zip and/or data files will be downloaded and stored in
respective sub-directories within \code{directory_to_save}.
}
}
\description{
The \code{download_groads()} function accesses and downloads
roads data from \href{https://earthdata.nasa.gov/data/catalog/sedac-ciesin-sedac-groads-v1-1.00}{NASA's Global Roads Open Access Data Set (gROADS), v1 (1980-2010)}.
}
\examples{
\dontrun{
download_groads(
  data_region = "Americas",
  data_format = "Shapefile",
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
}
}
\references{
\insertRef{data_ciesin2013groads}{amadeus}
}
\author{
Mitchell Manware, Insang Song
}
