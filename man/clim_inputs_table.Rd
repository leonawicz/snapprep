% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_clim_ar5.R
\name{clim_inputs_table}
\alias{clim_inputs_table}
\title{Prepare inputs table for climate data extraction}
\usage{
clim_inputs_table(base_path = snapdef()$ar5dir,
  vars = snapdef()$ar5var, models = snapdef()$ar5all)
}
\arguments{
\item{base_path}{source directory for downscaled climate tifs.}

\item{vars}{vector of climate variables.}

\item{models}{vector of GCMs and CRU model.}
}
\value{
a data frame.
}
\description{
This function creates a data frame of inputs used for climate data extraction from downscaled AR5 source data.
}
\details{
Default arguments are those in the \code{snapdef()} defaults list.
}
\examples{
\dontrun{
library(dplyr)
clim_inputs_table() \%>\%
  filter(!(model == "ts40" & var \%in\% c("tasmin", "tasmax")))
}
}
