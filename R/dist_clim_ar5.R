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

#' Compute seasonal climate data spatial distributions
#'
#' Compute seasonal climate data spatial probability distributions.
#'
#' Seasons are DJF, MAM, JJA and SON 3-month averages. A fifth "season" of full annual averages is also included.
#' For efficiency, this function operates on outputs from \code{clim_dist_monthly}. It does not need to redundantly
#' access source downscaled geotiffs.
#'
#' @param i iterator for files.
#' @param files vector of input files.
#' @param in_dir input directory.
#' @param out_dir output directory.
#' @param density.args arguments list passed to \code{density}.
#'
#' @return invisible, writes files.
#' @export
#'
#' @examples
#' \dontrun{
#' mclapply(seq_along(files), clim_dist_seasonal,
#'  files = files, mc.cores = 32)
#'  }
clim_dist_seasonal <- function(i, files, in_dir = snapdef()$ar5dir_dist_monthly,
                               out_dir = snapdef()$ar5dir_dist_seasonal,
                               density.args = list(n = 200, adjust = 0.1)){
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
  file <- files[i]
  dir.create(out_dir <- file.path(out_dir, dirname(file)), showWarnings = FALSE, recursive = TRUE)
  outfile <- file.path(out_dir, strsplit(basename(file), "\\.")[[1]][1])
  x <- readRDS(file)
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

#' Compute climate statistics
#'
#' Compute climate statistics from spatial probability distributions.
#'
#' For efficiency, this function operates on outputs from \code{clim_dist_monthly} and \code{clim_dist_seasonal}.
#' It does not need to redundantly access source downscaled geotiffs. This function is specific to AR5 outputs in the current implementation.
#'
#' @param files input files.
#' @param type character, \code{"monthly"} or \code{"seasonal"}.
#' @param out_dir output directory, e.g. one of the \code{snapdef()$ar5dir_dist_stats} entries.
#' @param mc.cores number of processors.
#'
#' @return invisible, writes files.
#' @export
#'
#' @examples
#' \dontrun{clim_stats_ar5(files)}
clim_stats_ar5 <- function(files, type = "monthly", out_dir = snapdef()$ar5dir_dist_stats[1], mc.cores = 32){
  if(!type %in% c("monthly", "seasonal")) stop("`type` must be 'monthly' or 'seasonal'.")
  grpDir <- dirname(files[1])
  loc <- basename(grpDir)
  dir.create(out_dir <- file.path(out_dir, grpDir), showWarnings = FALSE, recursive = TRUE)
  rcp_levels <- c("Historical", "4.5", "6.0", "8.5")
  relabel_rcps <- function(x) sapply(
    x, function(x) switch(x, historical = "Historical", rcp45 = "4.5", rcp60 = "6.0", rcp85 = "8.5"))
  model_levels <- c("CRU 4.0", "NCAR-CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3")
  season_levels <- if(type == "seasonal") c("Annual", "Winter", "Spring", "Summer", "Autumn") else month.abb
  relabel_seasons <- function(x) sapply(
    x, function(x) switch(x, annual = "Annual", winter = "Winter", spring = "Spring",
                          summer = "Summer", autumn = "Autumn"))
  f <- if(type == "seasonal") relabel_seasons else function(x) x
  files <- cbind(files, loc, do.call(rbind, strsplit(basename(files), "_"))) %>%
    tibble::data_frame() %>% dplyr::mutate(V6 = gsub("\\.rds", "", .data[["V6"]]))
  names(files) <- c("files", "Region", "Var", "RCP", "GCM", "Season")
  files <- dplyr::mutate(
    files,
    Region = factor(.data[["Region"]], levels = sort(unique(loc))),
    Var = factor(.data[["Var"]], levels = c("pr", "tas", "tasmin", "tasmax")),
    RCP = factor(relabel_rcps(.data[["RCP"]]), levels = rcp_levels),
    GCM = factor(factor(ifelse(.data[["GCM"]] == "ts40", "CRU 4.0", .data[["GCM"]]), levels = model_levels)),
    Season = factor(f(.data[["Season"]]), levels = season_levels))
  .stats <- function(i, files){
    cat(paste("File", i, "of", nrow(files), "...\n"))
    readRDS(files$files[i]) %>% rvtable::rvtable %>% rvtable::sample_rvtable %>%
      dplyr::group_by(.data[["RCP"]], .data[["GCM"]], .data[["Var"]], .data[["Year"]], .data[["Season"]]) %>%
      dplyr::summarise(
        Mean = round(mean(.data[["Val"]]), 1),
        SD = round(stats::sd(.data[["Val"]]), 1),
        Min = round(min(.data[["Val"]]), 1),
        Pct_05 = round(stats::quantile(.data[["Val"]], 0.05), 1),
        Pct_10 = round(stats::quantile(.data[["Val"]], 0.10), 1),
        Pct_25 = round(stats::quantile(.data[["Val"]], 0.25), 1),
        Pct_50 = round(stats::quantile(.data[["Val"]], 0.50), 1),
        Pct_75 = round(stats::quantile(.data[["Val"]], 0.75), 1),
        Pct_90 = round(stats::quantile(.data[["Val"]], 0.90), 1),
        Pct_95 = round(stats::quantile(.data[["Val"]], 0.95), 1),
        Max = round(max(.data[["Val"]]), 1)) %>%
      dplyr::ungroup() %>% dplyr::mutate(
        Region = files$Region[i], Var = files$Var[i], RCP = files$RCP[i], GCM = files$GCM[i],
        Season = files$Season[i])
  }
  files <- split(files, loc)
  for(j in seq_along(files)){
    x <- parallel::mclapply(seq_along(files[[j]]), .stats, files = files[[j]], mc.cores = mc.cores)
    x <- dplyr::bind_rows(x) %>% dplyr::select(
      .data[["RCP"]], .data[["GCM"]], .data[["Region"]], .data[["Var"]], .data[["Year"]], .data[["Season"]],
      .data[["Mean"]], .data[["SD"]], .data[["Min"]], .data[["Pct_05"]], .data[["Pct_10"]], .data[["Pct_25"]],
      .data[["Pct_50"]], .data[["Pct_75"]], .data[["Pct_90"]], .data[["Pct_95"]], .data[["Max"]]) %>%
      dplyr::arrange(.data[["RCP"]], .data[["GCM"]], .data[["Region"]],
                     .data[["Var"]], .data[["Year"]], .data[["Season"]])
    if(type == "monthly") x <- dplyr::rename(x, Month = .data[["Season"]]) # nolint
    outfile <- file.path(out_dir, unique(as.character(files[[j]]$Region)), "_climate.rds")
    saveRDS(x, outfile)
  }
  invisible()
}
