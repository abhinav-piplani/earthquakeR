% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{eq_clean_data}
\alias{eq_clean_data}
\title{Takes raw NOAA data frame and returns a clean data frame}
\usage{
eq_clean_data(df)
}
\arguments{
\item{df}{The raw uncleaned version of the data.frame}
}
\value{
A clean version of the data.frame, after pre-processing
}
\description{
Takes raw NOAA data frame and returns a clean data frame
}
\details{
BC years are marked with the 'neg.date' column
}
\examples{
\dontrun{
  raw_data <- eq_data()
  clean_data <- eq_clean_data(raw_data)
}
}
