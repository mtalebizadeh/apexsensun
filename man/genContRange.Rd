% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genContRange.R
\name{genContRange}
\alias{genContRange}
\title{genContRange}
\usage{
genContRange(controlRangeFile)
}
\arguments{
\item{controlRangeFile}{File name associated to parameter range file containing parameters inside the APEXCONT.dat file.}
}
\value{
Void
}
\description{
Creates a dataframe for setting lower and upper limits of uncertain model parameters inside the APEXCONT.dat file.
}
\examples{
# Used only if the inputs are to be read through text file.
\dontrun{
    genContRange("controlRangeFile.txt")
    }
}
