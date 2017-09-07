globalVariables(c(".x", ".y", ".data"))

#' snapprep: R functions for data prep and curation from raw SNAP data.
#'
#' \code{snapprep} is a developer package for the SNAPverse, used for preparing data sets for SNAPverse data packages.
#'
#' The \code{snapprep} package contains R functions used to support a wide range of SNAP projects by preparing and curating useful data sets
#' from upstream raw SNAP data. The data sets compiled with the aid of \code{snapprep} are then made avaible to other projects.
#' This includes compiling data sets that are contained in SNAPverse data packages.
#'
#' \code{snapprep} is a developer package used by the SNAPverse author and maintainer.
#' For user packages catering to analysis and graphing of the curated data sets available in SNAPverse data packages,
#' see the `snapfuns` package instead.

#'
#' @docType package
#' @name snapprep
NULL

#' @importFrom magrittr %>%
NULL

#' A function that returns a list of SNAP defaults such as path names to data sets and lists of available climate variables and models, etc.
#'
#' @return a list.
#' @export
#'
#' @examples
#' snapdef()$ar5gcm
snapdef <- function(){
  list(
    ar5dir = "/workspace/Shared/Tech_Projects/DeltaDownscaling/project_data/downscaled", # nolint start
    ar5dir_dist_monthly = "/workspace/UA/mfleonawicz/data/clim_2km_monthly",
    ar5dir_dist_seasonal = "/workspace/UA/mfleonawicz/data/clim_2km_seasonal",
    ar5dir_dist_stats = paste0("/workspace/UA/mfleonawicz/data/clim_2km_", c("monthly", "seasonal"), "_stats"),
    ar5gcm = c("GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3", "NCAR-CCSM4"),
    ar5cru = "ts40",
    ar5all = c("GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3", "NCAR-CCSM4", "ts40"),
    ar5var = c("pr", "tas", "tasmin", "tasmax"),
    akcan1km2km = "/atlas_scratch/mfleonawicz/projects/DataExtraction/workspaces/shapes2cells_akcan1km2km.RData" # nolint end
  )
}
