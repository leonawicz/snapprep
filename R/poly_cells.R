#' Generate nested list of spatial polygons data frames
#'
#' Generate a nested list of spatial polygons data frames from maps in the `snappoly` package.
#'
#' The list is subset to exclude the `canada` and `lcc` maps that extend outside Alaska.
#'
#' @param domain defaults to \code{"akcan"}, the standard SNAP geospatial extent.
#' The other option is \code{"ak"} pertaining to the classic ALFRESCO model "statewide" extent template.
#'
#' @return a list.
#' @export
#'
#' @examples
#' \dontrun{snap_poly_list()}
snap_poly_list <- function(domain = "akcan"){
  if(!domain %in% c("akcan", "ak")) stop("`domain` must be 'akcan' or 'ak'.")
  alaska <- snappoly::alaska
  canada <- snappoly::canada
  canada_ids <- c("Alberta", "Saskatchewan", "Manitoba", "Yukon Territory", "British Columbia")
  canada <- canada[canada[["NAME"]] %in% canada_ids, ]
  ecoreg <- snappoly::ecoreg
  ecoreg9 <- maptools::unionSpatialPolygons(ecoreg, ecoreg@data$LEVEL_2)
  ecoreg3 <- maptools::unionSpatialPolygons(ecoreg, ecoreg@data$LEVEL_1)
  ecoreg_ids <- gsub("\\.", "", as.data.frame(ecoreg)[, 1])
  ecoreg9_ids <- sapply(methods::slot(ecoreg9, "polygons"), function(x) methods::slot(x, "ID"))
  ecoreg3_ids <- sapply(methods::slot(ecoreg3, "polygons"), function(x) methods::slot(x, "ID"))
  aklcc <- snappoly::aklcc
  aklcc_ids <- as.data.frame(aklcc)[, 1]
  lcc <- snappoly::lcc
  lcc_ids <- gsub("/", "-", as.data.frame(lcc)[, 1])
  cavm <- snappoly::cavm
  cavm_ids <- as.data.frame(cavm)[, 4]
  fmz <- snappoly::fmz
  fmz_ids <- as.data.frame(fmz)[, 2]
  tpa <- snappoly::tpa
  tpa_ids <- as.data.frame(tpa)[, 4]
  tpa_ids[5] <- "Departments of Defense (DOD) and Energy (DOE)"
  grp_names <- c(rep("Political Boundaries", 2), paste0("Alaska L", 3:1, " Ecoregions"),
                 paste(c("AK LCC", "CAVM", "LCC", "FMZ", "TPA"), "regions"))
  poly_list <- list(alaska, canada, ecoreg, ecoreg9, ecoreg3, aklcc, cavm, lcc, fmz, tpa)
  poly_names <- list("Alaska", canada_ids, ecoreg_ids, ecoreg9_ids, ecoreg3_ids,
                    aklcc_ids, cavm_ids, lcc_ids, fmz_ids, tpa_ids)
  if(domain == "ak"){
    can_idx <- c(2, 8)
    grp_names <- grp_names[-can_idx]
    poly_list <- poly_list[-can_idx]
    poly_names <- poly_names[-can_idx]
  }
  list(poly_list = poly_list, poly_names = poly_names, group_names = grp_names)
}

.get_poly_cells <- function(i, r, shp, grp, loc, idx = raster::Which(!is.na(r), cells = TRUE)){
  stopifnot(length(shp) == length(grp) & length(shp) == length(loc))
  x <- raster::extract(r, shp[[i]], cellnumbers = TRUE)
  stopifnot(length(x) == length(loc[[i]]))
  purrr::map(seq_along(x), ~(
    if(!is.null(x[[.x]])) tibble::data_frame(
    LocGroup = grp[i], Location = loc[[i]][.x], Cell = sort(intersect(x[[.x]][, 1], idx))) else NULL)) %>%
    dplyr::bind_rows(x)
}

#' Save rds files containing raster cell tables for polygons
#'
#' Save rds files containing data frames of grid cell indices for the intersection of common SNAP spatial polygons
#' with SNAP template raster layers. Indices are stored for two conditions: matching to all raster cell indices and matching to
#' new cell indices based on first removing all NA-valued cells and re-indexing.
#'
#' Note that this function also folds in buffered FMO area unions from `snapgrid::swfmoBuffer` for the Alaska domain.
#'
#'
#' @param file_akcan file name for cell index table based on the Alaska/western Canada SNAP spatial domain.
#' @param file_ak file name for cell index table based on the Alaska "statewide" classic ALFRESCO SNAP spatial domain.
#' @param out_dir output directory.
#' @param mc.cores number of processors.
#'
#' @return invisible, writes files.
#' @export
#'
#' @examples
#' \dontrun{save_poly_cells()}
save_poly_cells <- function(file_akcan = "cells_akcan1km2km.rds", file_ak = "cells_ak1km.rds",
                            out_dir = snapdef()$celldir, mc.cores = 32){
  r1km <- snapgrid::akcan1km
  r2km <- snapgrid::akcan2km
  idx1 <- raster::Which(!is.na(r1km), cells = TRUE)
  idx2 <- raster::Which(!is.na(r2km), cells = TRUE)

  polylist <- snap_poly_list()
  cells1 <- tibble::data_frame(
    Source = "akcan1km", dplyr::bind_rows(
      parallel::mclapply(seq_along(polylist$poly_list), .get_poly_cells, r = r1km,
                         shp = polylist$poly_list, grp = polylist$grp_names,
                         loc = polylist$poly_names, idx = idx1, mc.cores = mc.cores)))
  cells1 <- dplyr::bind_rows(tibble::data_frame(
    Source = "akcan1km", LocGroup = "Political Boundaries", Location = "AK-CAN", Cell = idx1), cells1) %>%
    dplyr::group_by(.data[["Location"]]) %>%
    dplyr::mutate(Cell_rmNA = which(c(1:raster::ncell(r1km) %in% .data[["Cell"]])[idx1]))
  cells2 <- tibble::data_frame(
    Source = "akcan2km", dplyr::bind_rows(
      parallel::mclapply(seq_along(polylist$poly_list), .get_poly_cells, r = r2km,
                         shp = polylist$poly_list, grp = polylist$grp_names,
                         loc = polylist$poly_names, idx = idx2, mc.cores = mc.cores)))
  cells2 <- dplyr::bind_rows(tibble::data_frame(
    Source = "akcan2km", LocGroup = "Political Boundaries", Location = "AK-CAN", Cell = idx2), cells2) %>%
    dplyr::group_by(.data[["Location"]]) %>%
    dplyr::mutate(Cell_rmNA = which(c(1:raster::ncell(r2km) %in% .data[["Cell"]])[idx2]))

  cells <- dplyr::bind_rows(cells1, cells2) %>%
    dplyr::group_by(.data[["Source"]], .data[["LocGroup"]], .data[["Location"]])
  saveRDS(cells, file = file.path(out_dir, file_akcan))

  polylist <- snap_poly_list(domain = "ak")
  rak1km <- snapgrid::ak1km
  idx3 <- raster::Which(!is.na(rak1km), cells = TRUE)
  cells3 <-tibble::data_frame(
    Source = "ak1km", dplyr::bind_rows(
      parallel::mclapply(seq_along(polylist$poly_list), .get_poly_cells, r = rak1km,
                         shp = polylist$poly_list, grp = polylist$grp_names,
                         loc = polylist$poly_names, idx = idx3, mc.cores = mc.cores)))
  rfmo1km <- raster::readAll(snapgrid::swfmoBuffer)
  idx4 <- raster::Which(rfmo1km %in% 2:4, cells = TRUE) # union of modified/critical/full FMO 15-km buffers
  cells4 <- tibble::data_frame(Source = "ak1km", LocGroup = "FMO", Location = "MFC buffers", Cell = idx4)
  cells <- dplyr::bind_rows(tibble::data_frame(
    Source = "ak1km", LocGroup = "Statewide", Location = "AK", Cell = idx3), cells3, cells4) %>%
    dplyr::group_by(.data[["Location"]]) %>%
    dplyr::mutate(Cell_rmNA = which(c(1:raster::ncell(rak1km) %in% .data[["Cell"]])[idx3])) %>%
    dplyr::group_by(.data[["Source"]], .data[["LocGroup"]], .data[["Location"]])
  saveRDS(cells, file = file.path(out_dir, file_ak))
  invisible()
}
