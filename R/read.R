
#' Read meta-data from saved files with raw data
#'
#' @param ff character vector with file names (fill or base).
#' @param workers integer, number of workers (files to process in parallel)
#' @param read_dir directory with files to read (if `ff` contains base names)
#'
#' @return
#' @export
#'
#' @examples
nrel_read_meta_files <- function(
    ff,
    read_dir = NULL,
    workers = future::availableCores() %/% 2
) {
  future::plan(future::multisession, workers = workers)
  # plan(list(
  #   tweak(multisession, workers = availableCores() %/% 4),
  #   tweak(multisession, workers = 4)
  # ))
  if (!is.null(read_dir)) {
    ff <- file.path(read_dir, ff)
  }
  d <- future.apply::future_lapply(ff, FUN = function(f) {
    o <- try({load(f)})
    if (inherits(o, "try-error")) {
      mm <- paste("error reading file", f)
    } else {
      mm <- NULL
      try({
        # mm <- future_lapply(get(o)[["raw"]], function(r) {
        mm <- lapply(get(o)[["raw"]], function(r) {
          if (is.null(r) || is.null(r$status_code) || r$status_code != 200) {
            return(NULL)
          }
          # ll <- nrel_read_responce(r)
          # ll$data
          # ll$meta
          nrel_read_meta(r)
        })
        mm <- rbindlist(mm, use.names = T)
        mm$file <- basename(f)
      })
    }
    return(mm)
  })
  plan(sequential)
  names(d) <- basename(ff)
  return(d)
}


#' Read saved raw data from files and safe results in files by locations
#'
#' @param meta data.frame with IDs and files to process (output of `nrel_read_meta_files`, filtered for locations of interest).
#' @param read_dir path to the directory to files with raw data (output of `nrel_fetch_points` function).
#' @param save_dir path to the directory to save data.
#' @param workers integer, number of workers - files to process in parallel (affects memory use).
#' @param columns columns of data to read from the data.
#' @param col_mask
#' @param scale_round
#' @param compress compression level, value in the range 0 to 100, an argument to `fst::write_fst` function.
#' @param overwrite logical, should files be overwritten if exist. If FALSE and the file already exists, a number will be added to the file name, indicating the version.
#' @param merge_files unused
#'
#' @return saved files in `save_dir` with derived data from "raw" files.
#' @export
#'
#' @examples
nrel_read_save_data <- function(
    meta,
    read_dir,
    save_dir,
    columns = NULL,
    col_mask = NULL, # col_mask = "wind speed"
    scale_round = 2,
    compress = 100,
    overwrite = TRUE,
    merge_files = TRUE,
    workers = max(future::availableCores() %/% 2, 5)
) {
  if (!exists(save_dir)) dir.create(save_dir)
  future::plan(future::multisession, workers = workers)
  SiteID_all <- unique(meta$SiteID)
  file_all <- unique(meta$file)
  meta <- meta %>% ungroup() %>% group_by(file)
  list_meta <- group_split(meta)
  # lapply(list_meta, FUN = function(x) {
  future.apply::future_lapply(list_meta, FUN = function(x) {
    ff <- unique(x$file)
    # SiteID <- unique(x$SiteID)
    if (!is.null(read_dir)) {
      ff <- file.path(read_dir, ff)
    }
    stopifnot(length(ff) <= 1); f <- ff
    o <- try({load(f)})
    if (inherits(o, "try-error")) {
      mm <- paste("error reading file", f)
    } else {
      mm <- NULL
      try({
        # mm <- future_lapply(get(o)[["raw"]], function(r) {
        lapply(get(o)[["raw"]], function(r) {
          if (is.null(r) || is.null(r$status_code) || r$status_code != 200) {
            return(NULL)
          }
          ll <- nrel_read_responce(r)
          SiteID <- ll$meta$SiteID
          mm <- dplyr::mutate(
            ll$data,
            SiteID = as.integer(SiteID),
            .before = 1
          )
          # $SiteID <- ll$meta$SiteID
          if (!(SiteID %in% SiteID_all)) return(NULL)
          if (!is.null(col_mask)) {
            cn <- colnames(mm)
            jj <- lapply(
              c("SiteID", "Year", "Month", "Day", "Hour", "Minute", col_mask),
              function(x) which(grepl(x, cn, ignore.case = T))
            )
            jj <- unlist(jj)
            # browser()
            mm <- dplyr::select(mm, jj)
          }
          mm <- unique(mm)
          if (!is.null(scale_round)) {
            cc <- sapply(mm, function(x) inherits(x, "numeric"))
            for (j in names(cc[cc])) {
              mm[[j]] <- as.integer(mm[[j]] * 10^scale_round)
            }
          }
          fname <- file.path(save_dir, paste0("SiteID_", SiteID))
          if (overwrite) {
            check_if_exists <- list.files(save_dir, pattern = paste0(fname, ".+fst$"))
            if (length(check_if_exists) > 0) {
              if (merge_files) {
                # x <- read_fst
                # mm0 <- lapply(check_if_exists, read_fst)
                # mm0[[length(mm0) + 1]] <- mm
                # mm <- rbindlist...
              }
              fname <- paste0(fname, "_", length(check_if_exists) + 1)
            }
          }
          fname <- paste0(fname, ".fst")
          write_fst(mm, path = fname, compress = compress)
        }) # loop over SideIDs
        # mm <- rbindlist(mm, use.names = T, fill = T)
        # browser()
      }) # try
    }
   }) # loop over files
  plan(sequential)
  # names(d) <- basename(ff)
  # return(d)
}


