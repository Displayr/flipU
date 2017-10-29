% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilites.R
\name{ConvertCommaSeparatedStringToVector}
\alias{ConvertCommaSeparatedStringToVector}
\title{\code{ConvertCommaSeparatedStringToVector}}
\usage{
ConvertCommaSeparatedStringToVector(string, split = ",")
}
\arguments{
\item{string}{A \code{\link{character}} to be converted.}

\item{split}{A \code{\link{character}} vector containing regular expressions to be used in splitting. Where multiple entries are in the vector they are recycled along the vector of \code{string} (i.e., they are not all used as delimiters).}
}
\value{
A \code{vector} of \code{character}s.
}
\description{
Converts a string containing commas into a vector, trimming whistepaces along the way.
}