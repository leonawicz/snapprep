#' Prepare inputs table for climate data extraction
#'
#' This function creates a data frame of inputs used for climate data extraction from downscaled AR5 source data.
#'
#' Default arguments are those in the \code{snapdef()} defaults list.
#'
#' @param base_path source directory for downscaled climate tifs.
#' @param vars vector of climate variables.
#' @param models vector of GCMs and CRU model.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' clim_inputs_table() %>%
#'   filter(!(model == "ts40" & var %in% c("tasmin", "tasmax")))
#' }
clim_inputs_table <- function(base_path = snapdef()$ar5dir, vars = snapdef()$ar5var,
                              models = snapdef()$ar5all){
  rcps <- purrr::map(models, ~rep(list.files(file.path(base_path, .x)), each = length(vars))) %>%
    purrr::map(~.x[.x != "rcp26"])
  models <- purrr::map2(models, rcps, ~rep(.x, each = length(.y)))
  vars <- purrr::map(models, ~rep(vars, length = length(.x)))
  zero.min <- purrr::map(vars, ~.x == "pr")
  tibble::data_frame(rcp = unlist(rcps), model = unlist(models),
                     var = unlist(vars), zero = unlist(zero.min))
}

.get_clim_files <- function(rcp, model, variable, dir){
  files <- list.files(file.path(dir, model, rcp, variable), pattern=".tif$", full.names = TRUE)
  n <- nchar(files)
  yrs <- as.numeric(substr(files, n - 7, n - 4))
  mos <- substr(files, n - 10, n - 9)
  ord <- order(paste(yrs, mos))
  list(files = files[ord], years = yrs[ord], months = mos[ord])
}

#' Extract climate data and compute distributions
#'
#' Extract climate data and estimate monthly spatial probability distributions.
#'
#' \code{inputs} generally comes from \link{clim_inputs_table}. \code{clim_dist_monthly} processes data sets referred to by
#' one row of this data frame at a time. Internally processing uses 32 CPUs on an Atlas compute node. It is expected that the different
#' data sets in the full \code{inputs} be processed serially. See example call below.
#'
#' @param inputs data frame of inputs (one row). See details.
#' @param in_dir input directory, e.g., \code{snapdef()$ar5dir}.
#' @param out_dir output directory, e.g., \code{snapdef()$ar5dir_dist_monthly}
#' @param na.rm logical, remove NAs.
#' @param density.args arguments list passed to \code{density}.
#' @param sample.size numeric, sample size.
#' @param verbose logical, verbose progress.
#' @param overwrite logical, overwrite existing files.
#' @param move_akcan logical, relocate the \code{AK-CAN} domain from \code{Political Boundaries} subdirectory to top level as its own location group.
#' @param mc.cores number of CPUs when processing years in parallel. Defaults to 32 assuming Atlas compute node context.
#'
#' @return invisible, writes files.
#' @export
#'
#' @examples
#' \dontrun{
#' purrr::walk(1:nrow(inputs), ~slice(inputs, .x) %>% clim_dist_monthly())
#' }
clim_dist_monthly <- function(inputs, in_dir = snapdef()$ar5dir,
                              out_dir = snapdef()$ar5dir_dist_monthly,
                              na.rm = TRUE, density.args = list(n = 200, adjust = 0.1),
                              sample.size = 10000, verbose = TRUE, overwrite = FALSE,
                              move_akcan = TRUE, mc.cores = 32){
  if(nrow(inputs) != 1)
    stop("`inputs` must be a single-row data frame (one row from `clim_inputs_table`).")
  cells <- readRDS(snapdef()$cells_akcan1km2km) %>%
    dplyr::filter(.data[["Source"]] == "akcan2km")
  verbose <- if(verbose) TRUE else FALSE
  rcp <- inputs$rcp
  model <- inputs$model
  variable <- inputs$var
  zero.min <- inputs$zero
  files <- .get_clim_files(rcp, model, variable, in_dir)
  x0 <- raster::as.matrix(raster::stack(files$files, quick=TRUE))
  if(verbose) cat("Matrix in memory...\n")
  if(na.rm) x0 <- x0[!is.na(x0[, 1]), ]
  for(i in unique(cells$LocGroup)){
    cells.i <- dplyr::filter(cells, .data[["LocGroup"]] == i) # nolint
    for(j in unique(cells.i$Location)){
      if(move_akcan & i == "Political Boundaries" & j == "AK-CAN"){
        dir.create(grpDir <- file.path(out_dir, "AK-CAN", j), showWarnings = FALSE, recursive = TRUE)
      } else {
        dir.create(grpDir <- file.path(out_dir, i,  j), showWarnings = FALSE, recursive = TRUE)
      }
      file <- paste0(grpDir, "/", variable, "_", rcp, "_", model, ".rds")
      if(!overwrite && exists(file)) next
      if(verbose) cat(paste("Compiling data for", j, "...\n"))
      cells.ij <- dplyr::filter(cells.i, .data[["Location"]] == j) # nolint
      idx <- if(na.rm) cells.ij$Cell_rmNA else cells.ij$Cell
      x <- x0[idx, ]
      use_sample <- nrow(x) > sample.size
      if(use_sample) x <- x[sort(sample(1:nrow(x), sample.size)), ]
      yrs <- as.integer(rep(files$years, each = nrow(x)))
      mos <- as.integer(rep(files$months, each = nrow(x)))
      x <- as.numeric(x)
      x <- split(x, paste(yrs, c(paste0(0, 1:9), 10:12)[mos]))
      nam <- names(x)
      if(verbose) cat(paste0("Number of time slices: ", length(nam), "\n"))

      rvt <- function(data, Val, Prob, density.args, zero.min){
        x <- rvtable::rvtable(data, Val = Val, Prob = Prob, density.args = density.args)
        if(zero.min && any(x$Val < 0)){
          density.args$from <- 0
          x <- rvtable::rvtable(data, Val = Val, Prob = Prob, density.args = density.args)
        }
        x
      }

      x <- parallel::mclapply(x, rvt, Val = "Val", Prob = "Prob", density.args = density.args,
                              zero.min = zero.min, mc.cores = mc.cores)
      x <- purrr::map2(x, nam, ~dplyr::mutate(
        .x, Year = as.integer(substr(.y, 1, 4)), # nolint
        Month = as.integer(substr(.y, 6, 7)))) %>% # nolint
        dplyr::bind_rows() %>%
        dplyr::select(.data[["Year"]], .data[["Month"]], .data[["Val"]], .data[["Prob"]]) %>% # nolint
        rvtable::rvtable(density.args = density.args)
      if(zero.min && any(x$Val < 0)) cat("Density includes values less than zero.\n")
      saveRDS(x, file)
    }
  }
  invisible()
}

#' Split grouped monthly files into individual files
#'
#' Split grouped monthly climate files into individual monthly files with \code{01} through \code{12} suffix in file names.
#'
#' This function trades fewer files for smaller files and can be preferable for data fetched from AWS in Shiny apps.
#'
#' @param in_dir input directory, e.g., \code{snapdef()$ar5dir_dist_monthly}.
#' @param out_dir output directory, e.g., \code{snapdef()$ar5dir_dist_monthly_split}
#' @param mc.cores number of CPUs when processing years in parallel. Defaults to 32 assuming Atlas compute node context.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' split_monthly_files()
#' }
split_monthly_files <- function(in_dir = snapdef()$ar5dir_dist_monthly,
                                out_dir = snapdef()$ar5dir_dist_monthly_split, mc.cores = 32){
  files <- list.files(in_dir, pattern = ".rds$", recursive = TRUE)
  load_split_save <- function(file, in_dir, out_dir, idx = c(paste0(0, 1:9), 10:12)){
    dir.create(file.path(out_dir, dirname(file)), recursive = TRUE, showWarnings = FALSE)
    x <- readRDS(file.path(in_dir, file)) %>% split(.$Month)
    purrr::walk2(x, idx, ~({
      out <- file.path(out_dir, gsub("\\.rds", paste0("_", .y, ".rds"), file))
      saveRDS(.x, out)
    })
    )
  }
  parallel::mclapply(files, load_split_save, in_dir = in_dir, out_dir = out_dir, mc.cores = mc.cores)
  invisible()
}

#' Compute seasonal climate data spatial distributions
#'
#' Compute seasonal climate data spatial probability distributions.
#'
#' Seasons are DJF, MAM, JJA and SON 3-month averages. A fifth "season" of full annual averages is also included.
#' For efficiency, this function operates on outputs from \code{clim_dist_monthly}. It does not need to redundantly
#' access source downscaled geotiffs.
#'
#' Use \code{variable} to optionally specify a climate variable file identifier: \code{"pr"}, \code{"tas"}, \code{"tasmin"} or \code{"tasmax"}.
#' This will be used for pattern matching when listing files inside \code{in_dir}.
#' Similarly, use \code{rcp} to process a smaller batch.
#' These are helpful when there are many files, such that there could be RAM or time limitations.
#'
#' @param in_dir input directory, e.g., \code{snapdef()$ar5dir_dist_monthly}.
#' @param out_dir output directory, e.g., \code{snapdef()$ar5dir_dist_seasonal}
#' @param variable character, optional, to split into smaller file batches. See details.
#' @param rcp character, optional, to split into smaller file batches. See details.
#' @param density.args arguments list passed to \code{density}.
#' @param mc.cores number of CPUs when processing years in parallel. Defaults to 32 assuming Atlas compute node context.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clim_dist_seasonal() # all variables
#' clim_dist_seasonal(variable = "pr") # precipitation
#' clim_dist_seasonal(variable = "pr", rcp = "rcp60") # precipitation and RCP 6.0
#' }
clim_dist_seasonal <- function(in_dir = snapdef()$ar5dir_dist_monthly,
                               out_dir = snapdef()$ar5dir_dist_seasonal,
                               variable, rcp, density.args = list(n = 200, adjust = 0.1), mc.cores = 32){
  pat <- if(missing(rcp)) ".rds$" else paste0(rcp, ".*.rds$")
  pat <- if(missing(variable)) pat else paste0("^", variable, ".*.", pat)
  files <- list.files(in_dir, pattern = pat, recursive = TRUE)

  .seasonal <- function(x, season, density.args){
    .season <- function(x, months){
      yrs <- range(x$Year)
      x <- dplyr::filter(x, .data[["Month"]] %in% months) # nolint
      if(any(months == 12)){
        y <- dplyr::mutate(
          x, Year = ifelse(.data[["Month"]] == 12, .data[["Year"]] + 1L, .data[["Year"]])) %>% # nolint
          dplyr::filter(.data[["Year"]] > yrs[1] & .data[["Year"]] <= yrs[2]) # nolint
        x <- dplyr::filter(x, .data[["Year"]] == yrs[1]) %>% dplyr::bind_rows(y) # nolint
      }
      rvtable::rvtable(x, density.args = density.args)
    }
    x <- switch(season,
                "annual" = rvtable::rvtable(x, density.args = density.args),
                "winter" = .season(x, c(1, 2, 12)),
                "spring" = .season(x, 3:5),
                "summer" = .season(x, 6:8),
                "autumn" = .season(x, 9:11))
    rvtable::marginalize(x, "Month", density.args = density.args)
  }

  compute_and_save <- function(file, in_dir, out_dir){
    dir.create(out_dir <- file.path(out_dir, dirname(file)), showWarnings = FALSE, recursive = TRUE)
    outfile <- file.path(out_dir, strsplit(basename(file), "\\.")[[1]][1])
    x <- readRDS(file.path(in_dir, file))
    y <- .seasonal(x, "annual", density.args=density.args)
    saveRDS(y, paste0(outfile, "_annual.rds"))
    y <- .seasonal(x, "winter", density.args=density.args)
    saveRDS(y, paste0(outfile, "_winter.rds"))
    y <- .seasonal(x, "spring", density.args=density.args)
    saveRDS(y, paste0(outfile, "_spring.rds"))
    y <- .seasonal(x, "summer", density.args=density.args)
    saveRDS(y, paste0(outfile, "_summer.rds"))
    y <- .seasonal(x, "autumn", density.args=density.args)
    saveRDS(y, paste0(outfile, "_autumn.rds"))
    invisible()
  }
  parallel::mclapply(files, compute_and_save, in_dir = in_dir, out_dir = out_dir, mc.cores = mc.cores)
  invisible()
}

#' Compute climate statistics
#'
#' Compute climate statistics from spatial probability distributions.
#'
#' For efficiency, this function operates on outputs from \code{clim_dist_monthly} and \code{clim_dist_seasonal}.
#' It does not need to redundantly access source downscaled geotiffs. This function is specific to AR5 outputs in the current implementation.
#'
#' The example \code{in_dir} and \code{out_dir} shown are assumed if these arguments are missing.
#' This is for convenience and they will adjust automatically based on \code{type}.
#' If providing alternate directories, make sure to specify in accordance with your \code{type}.
#'
#' Use \code{region_group} to optionally specify a region group file identifier.
#' This will be used for pattern matching when listing files inside \code{in_dir}.
#' This is helpful when there are many files, such that there could be RAM or time limitations.
#'
#' @param type character, \code{"monthly"} or \code{"seasonal"}.
#' @param in_dir input directory, e.g. \code{snapdef()$ar5dir_dist_monthly} or \code{snapdef()$ar5dir_dist_seasonal}. See details.
#' @param out_dir output directory, e.g. one of the \code{snapdef()$ar5dir_dist_stats} entries. See details.
#' @param region_group character, optional, to split into smaller file batches. See details.
#' @param mc.cores number of CPUs when processing years in parallel. Defaults to 32 assuming Atlas compute node context.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clim_stats_ar5(type = "monthly")
#' clim_stats_ar5(type = "seasonal")
#' }
clim_stats_ar5 <- function(type = "monthly", in_dir, out_dir, region_group, mc.cores = 32){
  if(!type %in% c("monthly", "seasonal")) stop("`type` must be 'monthly' or 'seasonal'.")
  if(missing(in_dir)){
    if(type == "monthly") in_dir <- snapdef()$ar5dir_dist_monthly
    if(type == "seasonal") in_dir <- snapdef()$ar5dir_dist_seasonal
  }
  if(missing(out_dir)){
    if(type == "monthly") out_dir <- snapdef()$ar5dir_dist_stats[1]
    if(type == "seasonal") out_dir <- snapdef()$ar5dir_dist_stats[2]
  }
  files <- list.files(in_dir, pattern = ".rds$", recursive = TRUE)
  if(!missing(region_group)) files <- files[substr(files, 1, nchar(region_group)) == region_group]
  grp <- basename(dirname(dirname(files)))
  loc <- basename(dirname(files))
  rcp_levels <- c("Historical", "4.5", "6.0", "8.5")
  relabel_rcps <- function(x) sapply(
    x, function(x) switch(x, historical = "Historical", rcp45 = "4.5", rcp60 = "6.0", rcp85 = "8.5"))
  model_levels <- c("CRU 4.0", "NCAR-CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3")
  season_levels <- if(type == "seasonal") c("Annual", "Winter", "Spring", "Summer", "Autumn") else month.abb
  relabel_seasons <- function(x) sapply(
    x, function(x) switch(x, annual = "Annual", winter = "Winter", spring = "Spring",
                          summer = "Summer", autumn = "Autumn"))
  f <- if(type == "seasonal") relabel_seasons else function(x) x
  files <- cbind(files, grp, loc, do.call(rbind, strsplit(basename(files), "_"))) %>%
    tibble::as_data_frame()
  nam <- c("files", "Group", "Region", "Var", "RCP", "GCM")
  if(type == "seasonal") nam <- c(nam, "Season")
  names(files) <- nam
  if(type == "monthly") files <- dplyr::mutate(files, GCM = gsub("\\.rds", "", .data[["GCM"]]))
  if(type == "seasonal") files <- dplyr::mutate(files, Season = gsub("\\.rds", "", .data[["Season"]]))
  files <- dplyr::mutate(
    files,
    RCP = factor(relabel_rcps(.data[["RCP"]]), levels = rcp_levels),
    GCM = factor(ifelse(.data[["GCM"]] == "ts40", "CRU 4.0", .data[["GCM"]]),
                        levels = model_levels),
    Var = factor(.data[["Var"]], levels = c("pr", "tas", "tasmin", "tasmax"))
  )
  if(type == "seasonal"){
    files <- dplyr::mutate(files, Season = factor(f(.data[["Season"]]), levels = season_levels))
  }
  .stats <- function(i, files, in_dir, type){
    file <- file.path(in_dir, files$files[i])
    x0 <- dplyr::slice(files, i) %>% dplyr::select(-.data[["files"]])
    x <- readRDS(file) %>% rvtable::sample_rvtable() %>% dplyr::ungroup()
    if(type == "monthly"){
      x <- dplyr::group_by(x, .data[["Year"]], .data[["Month"]])
    } else {
      x <- dplyr::group_by(x, .data[["Year"]])
    }
    x <- dplyr::summarise(x,
      Mean = round(mean(.data[["Val"]]), 1),
      SD = round(stats::sd(.data[["Val"]]), 1),
      Min = round(min(.data[["Val"]]), 1),
      Max = round(max(.data[["Val"]]), 1),
      Pct_025 = round(stats::quantile(.data[["Val"]], 0.025), 1),
      Pct_05 = round(stats::quantile(.data[["Val"]], 0.05), 1),
      Pct_10 = round(stats::quantile(.data[["Val"]], 0.10), 1),
      Pct_25 = round(stats::quantile(.data[["Val"]], 0.25), 1),
      Pct_50 = round(stats::quantile(.data[["Val"]], 0.50), 1),
      Pct_75 = round(stats::quantile(.data[["Val"]], 0.75), 1),
      Pct_90 = round(stats::quantile(.data[["Val"]], 0.90), 1),
      Pct_95 = round(stats::quantile(.data[["Val"]], 0.95), 1),
      Pct_975 = round(stats::quantile(.data[["Val"]], 0.975), 1)) %>%
    dplyr::ungroup() %>% dplyr::mutate(
      RCP = x0$RCP, GCM = x0$GCM, Var = x0$Var, Group = x0$Group, Region = x0$Region)
    if(type == "monthly") x <- dplyr::mutate(
      x, Month = factor(month.abb[.data[["Month"]]], levels = month.abb))
    if(type == "seasonal") x <- dplyr::mutate(x, Season = x0$Season)
    x
  }
  files <- split(files, paste(grp, loc, sep = "_"))
  for(j in seq_along(files)){
    grp_loc <- strsplit(names(files)[j], "_")[[1]]
    cat(paste0("Processing file ", j, " of ", length(files), ": ", grp_loc[1], "/", grp_loc[2], "...\n"))
    x <- parallel::mclapply(1:nrow(files[[j]]), .stats,
                            files = files[[j]], in_dir = in_dir, type = type, mc.cores = mc.cores)
    x <- dplyr::bind_rows(x)
    if(type == "monthly")
      x <- dplyr::select(x, c(16:20, 1:15)) %>%
      dplyr::arrange(.data[["RCP"]], .data[["GCM"]], .data[["Var"]], .data[["Year"]], .data[["Month"]])
    if(type == "seasonal")
      x <- dplyr::select(x, c(15:19, 1, 20, 2:14)) %>%
      dplyr::arrange(.data[["RCP"]], .data[["GCM"]], .data[["Var"]], .data[["Year"]], .data[["Season"]])
    dir.create(out_dir_tmp <- file.path(out_dir, grp_loc[1]), showWarnings = FALSE, recursive = TRUE)
    outfile <- paste0(out_dir_tmp, "/", grp_loc[2], "_clim_stats.rds")
    saveRDS(x, outfile)
  }
  invisible()
}

#' Extract climate values for point locations
#'
#' Extract monthly climate values for point locations by RCP, climate model and climate variable.
#'
#' \code{inputs} generally comes from \link{clim_inputs_table}. \code{clim_locs_prep} processes data sets referred to by
#' one row of this data frame at a time. This function generates intermediary temp files used susbsequently by \code{\link{clim_locs}}
#' and can be deleted afterward if not needed for other uses where it is convenient to have extracted point location climate data
#' segmented into files by RCP, model and variable as opposed to by point location.
#'
#' @param inputs data frame of inputs (one row). See details.
#' @param in_dir input directory, e.g., \code{snapdef()$ar5dir}.
#' @param out_dir output directory, e.g., \code{snapdef()$ar5dir_locs_prep}
#' @param verbose logical, verbose progress.
#' @param overwrite logical, overwrite existing files.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(parallel)
#' inputs <- clim_inputs_table() %>%
#'   filter(!(model == "ts40" & var %in% c("tasmin", "tasmax")))
#' mclapply(split(inputs, 1:nrow(inputs)), clim_locs_prep, mc.cores = 32)
#' }
clim_locs_prep <- function(inputs, in_dir = snapdef()$ar5dir,
  out_dir = snapdef()$ar5dir_locs_prep, verbose = TRUE, overwrite = FALSE){
  if(nrow(inputs) != 1)
    stop("`inputs` must be a single-row data frame (one row from `clim_inputs_table`).")
  locs <- snaplocs::locs %>% dplyr::filter(.data[["region"]] != "Northwest Territories")
  rcp <- inputs$rcp
  model <- inputs$model
  variable <- inputs$var
  rcp_levels <- c("Historical", "4.5", "6.0", "8.5")
  relabel_rcps <- function(x) sapply(
    x, function(x) switch(x, historical = "Historical", rcp45 = "4.5", rcp60 = "6.0", rcp85 = "8.5"))
  model_levels <- c("CRU 4.0", "NCAR-CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3")
  files <- .get_clim_files(rcp, model, variable, in_dir)
  template <- raster::raster(files$files[1])
  cells <- raster::cellFromXY(template, snaplocs::wgs2ak(locs[, 3:4]))
  verbose <- if(verbose) TRUE else FALSE
  x0 <- raster::extract(raster::stack(files$files, quick = TRUE), cells)
  if(verbose) cat("Matrix in memory...\n")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  file <- paste0(out_dir, "/", "locs_", variable, "_", rcp, "_", model, ".rds")
  if(!overwrite && exists(file)) next
  x <- tibble::data_frame(RCP = factor(relabel_rcps(rcp), levels = rcp_levels),
                          GCM = factor(ifelse(model == "ts40", "CRU 4.0", model), levels = model_levels),
                          Var = factor(variable, levels = c("pr", "tas", "tasmin", "tasmax")),
                          Year = as.integer(rep(files$years, each = nrow(x0))),
                          Month = factor(rep(month.abb[as.integer(files$months)], each = nrow(x0)), levels = month.abb),
                          Mean = as.numeric(x0),
                          LocGroup = rep(locs$region, ncol(x0)),
                          Location = rep(locs$loc, ncol(x0)))
  saveRDS(x, file)
  invisible()
}

#' Arrange climate by location and compute seasonals.
#'
#' Arrange extracted monthly climate values for point locations into individual files by location
#' from preliminary monthly files split by RCP, climate model and climate variable.
#' Compute seasonal/annual means for temperature variables and seasonal/annual totals for precipitation.
#'
#' @param in_dir input directory containing files produced by \code{\link{clim_locs_prep}}.
#' @param out_dir output directory.
#' @param overwrite logical, overwrite existing files.
#' @param mc.cores number of CPUs when processing years in parallel. Defaults to 32 assuming Atlas compute node context.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clim_locs()
#' }
clim_locs <- function(in_dir = snapdef()$ar5dir_locs_prep, out_dir = snapdef()$ar5dir_locs,
                      overwrite = FALSE, mc.cores = 32){
  files <- list.files(in_dir, full.names = TRUE)
  x <- parallel::mclapply(files, readRDS, mc.cores = mc.cores)
  x <- dplyr::bind_rows(x) %>% dplyr::arrange(
    .data[["LocGroup"]], .data[["Location"]], .data[["RCP"]], .data[["GCM"]],
    .data[["Var"]], .data[["Year"]], .data[["Month"]])
  gc()

  .seasonal <- function(x, season){
    .season <- function(x, months){
      yrs <- range(x$Year)
      x <- dplyr::filter(x, .data[["Month"]] %in% months) # nolint
      if(any(months == "Dec")){
        y <- dplyr::mutate(
          x, Year = ifelse(.data[["Month"]] == "Dec", .data[["Year"]] + 1L, .data[["Year"]])) %>% # nolint
          dplyr::filter(.data[["Year"]] > yrs[1] & .data[["Year"]] <= yrs[2]) # nolint
        x <- dplyr::filter(x, .data[["Year"]] == yrs[1]) %>% dplyr::bind_rows(y) # nolint
      }
      x
    }
    season_levels <- c("Annual", "Winter", "Spring", "Summer", "Autumn")
    x <- switch(season,
                "Annual" = x,
                "Winter" = .season(x, month.abb[c(1, 2, 12)]),
                "Spring" = .season(x, month.abb[3:5]),
                "Summer" = .season(x, month.abb[6:8]),
                "Autumn" = .season(x, month.abb[9:11]))
    a <- ifelse(season == "Annual", 12, 3)
    dplyr::group_by(x, .data[["RCP"]], .data[["GCM"]], .data[["Var"]], .data[["LocGroup"]],
                    .data[["Location"]], .data[["Year"]]) %>%
      dplyr::summarise(Mean = round(mean(.data[["Mean"]]), 1)) %>% dplyr::ungroup() %>%
      dplyr::mutate(Mean = ifelse(
        .data[["Var"]] == "pr", round(a * .data[["Mean"]]), round(.data[["Mean"]], 1))) %>%
      dplyr::mutate(Season = factor(season, levels = season_levels)) %>%
      dplyr::select(.data[["RCP"]], .data[["GCM"]], .data[["Var"]], .data[["LocGroup"]], .data[["Location"]],
                    .data[["Year"]], .data[["Season"]], .data[["Mean"]])
  }

  uni_locs <- paste(x$LocGroup, x$Location)
  gc()
  x <- split(x, uni_locs)
  gc()

  save_all <- function(x, overwrite = FALSE){
    lg <- x$LocGroup[1]
    loc <- x$Location[1]
    cat("Processing", loc, "...\n")
    dir.create(mon_dir <- file.path(out_dir, "monthly", lg), recursive = TRUE, showWarnings = FALSE)
    file <- paste0(gsub("/", "--", loc), "_clim_stats.rds")
    if(overwrite || !file.exists(file.path(mon_dir, file))){
      x <- dplyr::select(x, .data[["RCP"]], .data[["GCM"]], .data[["Var"]], .data[["LocGroup"]], .data[["Location"]],
                         .data[["Year"]], .data[["Month"]], .data[["Mean"]])
      tryCatch(saveRDS(x, file.path(mon_dir, file)), error = function(e) { cat("Error saving", file, "\n") }) # nolint
    }
    dir.create(sea_dir <- file.path(out_dir, "seasonal", lg), recursive = TRUE, showWarnings = FALSE)
    dir.create(dec1_dir <- file.path(out_dir, "decavg", "monthly", lg), recursive = TRUE, showWarnings = FALSE)
    dir.create(dec2_dir <- file.path(out_dir, "decavg", "seasonal", lg), recursive = TRUE, showWarnings = FALSE)
    if(overwrite || !file.exists(file.path(sea_dir, file)) ||
       !file.exists(file.path(dec1_dir, file)) || !file.exists(file.path(dec2_dir, file))){
      x1 <- .seasonal(x, "Annual")
      x2 <- .seasonal(x, "Winter")
      x3 <- .seasonal(x, "Spring")
      x4 <- .seasonal(x, "Summer")
      x5 <- .seasonal(x, "Autumn")
      xs <- dplyr::bind_rows(x1, x2, x3, x4, x5) %>%
        dplyr::arrange(.data[["RCP"]], .data[["GCM"]], .data[["Var"]], .data[["Year"]], .data[["Season"]])
      tryCatch(saveRDS(xs, file.path(sea_dir, file)), error = function(e) { cat("Error saving", file, "\n") }) # nolint
    }
    dir.create(dec_dir <- file.path(out_dir, "decavg", "monthly", lg), recursive = TRUE, showWarnings = FALSE)
    if(overwrite || !file.exists(file.path(dec_dir, file))){
      x <- dplyr::mutate(x, Decade = as.integer(.data[["Year"]] - .data[["Year"]] %% 10)) %>%
        dplyr::group_by(.data[["RCP"]], .data[["GCM"]], .data[["Var"]], .data[["LocGroup"]], .data[["Location"]],
                        .data[["Decade"]], .data[["Month"]]) %>%
        dplyr::summarise(Mean = round(mean(.data[["Mean"]]), 1)) %>% dplyr::ungroup()
      tryCatch(saveRDS(x, file.path(dec_dir, file)), error = function(e) { cat("Error saving", file, "\n") }) # nolint
    }
    dir.create(dec_dir <- file.path(out_dir, "decavg", "seasonal", lg), recursive = TRUE, showWarnings = FALSE)
    if(overwrite || !file.exists(file.path(dec_dir, file))){
      xs <- dplyr::mutate(xs, Decade = as.integer(.data[["Year"]] - .data[["Year"]] %% 10)) %>%
        dplyr::group_by(.data[["RCP"]], .data[["GCM"]], .data[["Var"]], .data[["LocGroup"]], .data[["Location"]],
                        .data[["Decade"]], .data[["Season"]]) %>%
        dplyr::summarise(Mean = round(mean(.data[["Mean"]]), 1)) %>% dplyr::ungroup()
      tryCatch(saveRDS(xs, file.path(dec_dir, file)), error = function(e) { cat("Error saving", file, "\n") }) # nolint
    }
    invisible()
  }
  parallel::mclapply(x, save_all, overwrite = overwrite, mc.cores = mc.cores)
}

#' Consolidate decadal climate values for all point locations.
#'
#' Consolidate extracted and curated decadal climate values (monthly and seasonal) for point locations.
#'
#' Consolidate into one monthly and one seasonal file per climate variable.
#' If \code{by_variable = FALSE}, consolidate into one monthly and one seasonal file for all variables.
#'
#'
#' @param in_dir input directory containing files produced by \code{\link{clim_locs}}.
#' @param out_dir output directory.
#' @param by_variable logical, unique output file directory per climate variable. See details.
#' @param mc.cores number of CPUs when processing years in parallel. Defaults to 32 assuming Atlas compute node context.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clim_locs_dec_all()
#' }
clim_locs_dec_all <- function(in_dir = snapdef()$ar5dir_locs_dec, out_dir = snapdef()$ar5dir_locs_all,
                              by_variable = TRUE, mc.cores = 32){
  f <- function(id, byvar){
    dir.create(out <- file.path(out_dir, id), recursive = TRUE, showWarnings = FALSE)
    files <- list.files(file.path(in_dir, id), recursive = TRUE, full.names = TRUE)
    x <- parallel::mclapply(files, readRDS, mc.cores = mc.cores) %>% dplyr::bind_rows()
    if(byvar){
      x <- split(x, x$Var)
      purrr::walk2(x, names(x), ~saveRDS(.x, paste0(out, "/", .y, "_clim_stats.rds")))
    } else {
      saveRDS(x, file.path(out, "locs_clim_stats.rds"))
    }
    invisible()
  }
  f("monthly", by_variable)
  f("seasonal", by_variable)
}
