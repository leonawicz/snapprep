globalVariables(c(".x", ".y", ".data"))

#' snapprep: R functions for data prep and curation from raw SNAP data.
#'
#' \code{snapprep} is a developer package for the SNAPverse, used for preparing data sets for SNAPverse data packages.
#'
#' The \code{snapprep} package contains R functions used to support a wide range of SNAP projects by preparing and curating useful data sets
#' from upstream raw SNAP data. The data sets compiled with the aid of \code{snapprep} are then made available to other projects.
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
#' @importFrom methods slot
NULL

#' A function that returns a list of SNAP defaults such as path names to data sets and lists of available climate variables and models, etc.
#'
#' @return a list.
#' @export
#'
#' @examples
#' snapdef()$ar5gcm
snapdef <- function(){
  bsalf <- "/big_scratch/mfleonawicz/Alf_Files_20121129" # nolint start
  datex <- "/atlas_scratch/mfleonawicz/projects/DataExtraction/workspaces"
  alfex <- "/atlas_scratch/mfleonawicz/alfresco"
  mldat <- "/workspace/UA/mfleonawicz/data"
  list(
    dist_dir = mldat,
    ar5dir = "/workspace/Shared/Tech_Projects/DeltaDownscaling/project_data/downscaled", # "/Data/Base_Data/Climate/AK_CAN_2km_v2_1",
    ar5dir_dist_monthly = file.path(mldat, "climate/clim_2km_monthly_grouped"),
    ar5dir_dist_monthly_split = file.path(mldat, "climate/clim_2km_monthly_single"),
    ar5dir_dist_seasonal = file.path(mldat, "climate/clim_2km_seasonal"),
    ar5dir_dist_stats = paste0(mldat, "/climate/clim_2km_", c("monthly", "seasonal"), "_stats"),
    ar5gcm = c("GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3", "NCAR-CCSM4"),
    ar5cru = "ts40",
    ar5all = c("GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3", "NCAR-CCSM4", "ts40"),
    ar5var = c("pr", "tas", "tasmin", "tasmax"),
    template_akcan1km = "/atlas_scratch/apbennett/IEM/FinalCalib/cccma_cgcm3_1.sresa1b/Maps/1900/Age_0_1900.tif",
    template_akcan2km = "/Data/Base_Data/Climate/AK_CAN_2km/projected/AR5_CMIP5_models_deprecated/rcp60/5ModelAvg/pr/pr_total_mm_AR5_5ModelAvg_rcp60_01_2006.tif",
    template_ak1km = file.path(alfex, "CMIP5_Statewide/outputs/3m/rcp45.CCSM4/Maps/2014/Age_0_2014.tif"),
    veg_ak = file.path(bsalf, "alf2005.cavm.merged.030212.tif"),
    fmoBuffer_ak = file.path(bsalf, "fmo_2017_buffered.tif"),
    fmoRatios_ak = file.path(bsalf, "fmo_2017_buffered_fs.tif"),
    cells_akcan1km2km = file.path(datex, "cells_akcan1km2km.rds"),
    cells_ak1km = file.path(datex, "cells_ak1km.rds"),
    celldir = datex # nolint end
  )
}
