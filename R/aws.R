#' Upload data to Amazon Web Services
#'
#' Upload prepared SNAP data to Amazon Web Services S3 buckets.
#'
#' This function uploads SNAP data to S3 buckets for use primarily by SNAP Shiny apps.
#' The files uploaded includes any recursively listed under \code{in_dir}. The names will include only the base names.
#' A \code{prefix} can be placed before the file base names. Using \code{/} in the prefix allows for the simulation of folders in the S3 bucket.
#' Remember that S3 buckets technically have a flat structure.
#'
#' NOTE: This function will not work unless the AWS keys granting permission to the bucket are loaded into the R session.
#'
#' @param in_dir character, input directory.
#' @param prefix character, optional prefix.
#' @param bkt S3 bucket name, defaults to the author since this is a SNAP developer package.
#' @param pattern file name pattern passed to \code{list.files} for filtering files in \code{in_dir}.
#' @param mc.cores number of CPUs when processing years in parallel. Defaults to 32 assuming Atlas compute node context.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' source("aws_key.R") # SEE DETAILS
#' aws_upload(snapdef()$ar5dir_dist_monthly, "clim/dist/ar5_2km/monthly")
#' aws_upload(snapdef()$ar5dir_dist_seasonal, "clim/dist/ar5_2km/seasonal")
#' }
aws_upload <- function(in_dir, prefix, bkt = "leonawicz", pattern = NULL, mc.cores = 32){
  files <- list.files(in_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
  objs <- file.path(prefix, basename(files))
  aws_put <- function(i, files, objs, bkt){
    cat(paste0("Uploading to AWS: ", files[i], "\n"))
    aws.s3::put_object(file = files[i], object = objs[i], bucket = bkt)
    invisible()
  }
  parallel::mclapply(seq_along(files), aws_put, files = files, objs = objs, bkt = bkt, mc.cores = mc.cores)
  cat(paste0("Files uploaded to ", bkt, "/", prefix, ".\n"))
  invisible()
}
