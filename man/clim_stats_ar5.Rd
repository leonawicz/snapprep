% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_clim_ar5.R
\name{clim_stats_ar5}
\alias{clim_stats_ar5}
\title{Compute climate statistics}
\usage{
clim_stats_ar5(type = "monthly", in_dir, out_dir, region_group,
  mc.cores = 32)
}
\arguments{
\item{type}{character, \code{"monthly"} or \code{"seasonal"}.}

\item{in_dir}{input directory, e.g. \code{snapdef()$ar5dir_dist_monthly} or \code{snapdef()$ar5dir_dist_seasonal}. See details.}

\item{out_dir}{output directory, e.g. one of the \code{snapdef()$ar5dir_dist_stats} entries. See details.}

\item{region_group}{character, optional, to split into smaller file batches. See details.}

\item{mc.cores}{number of CPUs when processing years in parallel. Defaults to 32 assuming Atlas compute node context.}
}
\description{
Compute climate statistics from spatial probability distributions.
}
\details{
For efficiency, this function operates on outputs from \code{clim_dist_monthly} and \code{clim_dist_seasonal}.
It does not need to redundantly access source downscaled geotiffs. This function is specific to AR5 outputs in the current implementation.

The example \code{in_dir} and \code{out_dir} shown are assumed if these arguments are missing.
This is for convenience and they will adjust automatically based on \code{type}.
If providing alternate directories, make sure to specify in accordance with your \code{type}.

Use \code{region_group} to optionally specify a region group file identifier.
This will be used for pattern matching when listing files inside \code{in_dir}.
This is helpful when there are many files, such that there could be RAM or time limitations.
}
\examples{
\dontrun{
clim_stats_ar5(type = "monthly")
clim_stats_ar5(type = "seasonal")
}
}
