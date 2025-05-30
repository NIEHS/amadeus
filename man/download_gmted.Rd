% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{download_gmted}
\alias{download_gmted}
\title{Download elevation data}
\usage{
download_gmted(
  statistic = c("Breakline Emphasis", "Systematic Subsample", "Median Statistic",
    "Minimum Statistic", "Mean Statistic", "Maximum Statistic",
    "Standard Deviation Statistic"),
  resolution = c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"),
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
\item{statistic}{character(1). Available statistics include \code{"Breakline Emphasis"}, \code{"Systematic Subsample"}, \code{"Median Statistic"},
\code{"Minimum Statistic"}, \code{"Mean Statistic"}, \code{"Maximum Statistic"}, and
\code{"Standard Deviation Statistic"}.}

\item{resolution}{character(1). Available resolutions include \code{"7.5 arc-seconds"}, \code{"15 arc-seconds"}, and \code{"30 arc-seconds"}.}

\item{directory_to_save}{character(1). Directory to save data. Two
sub-directories will be created for the downloaded zip files ("/zip_files")
and the unzipped data files ("/data_files").}

\item{acknowledgement}{logical(1). By setting \code{TRUE} the
user acknowledges that the data downloaded using this function may be very
large and use lots of machine storage and memory.}

\item{download}{logical(1). \code{FALSE} will generate a *.txt file
containing all download commands. By setting \code{TRUE} the function
will download all of the requested data files.}

\item{remove_command}{logical(1).
Remove (\code{TRUE}) or keep (\code{FALSE})
the text file containing download commands. Default is FALSE.}

\item{unzip}{logical(1). Unzip zip files. Default is \code{TRUE}.}

\item{remove_zip}{logical(1). Remove zip file from directory_to_download.
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
\code{directory_to_save}.
}
}
\description{
The \code{download_gmted()} function accesses and downloads Global
Multi-resolution Terrain Elevation Data (GMTED2010) from
\href{https://www.usgs.gov/coastal-changes-and-impacts/gmted2010}{U.S. Geological Survey and National Geospatial-Intelligence Agency}.
}
\examples{
\dontrun{
download_gmted(
  statistic = "Breakline Emphasis",
  resolution = "7.5 arc-seconds",
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
}
}
\references{
\insertRef{danielson_global_2011}{amadeus}
}
\author{
Mitchell Manware, Insang Song
}
