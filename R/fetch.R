#' Fetch (request and download) data from NREL
#'
#' @param lon vector with longitude coordinate(s)
#' @param lat vector with latitude coordinate(s)
#' @param api_url character string with the url-address to a particular dataset
#' @param as httr parameter
#' @param ... attributes for the API query
#' @param timeout numeric, seconds, time to pass in `httr::timeout`, 20 by default
#'
#' @return
#' @export
#'
#' @examples
nrel_fetch_coord <- function(lon, lat, api_url = NULL, as = "raw", ...,
                             timeout = 100) {
  # browser()
  arguments <- list(...)
  crd <- data.frame(lon, lat)
  if (nrow(crd) > 1) type <- "POLYGON" else  type <- "POINT"

  crd <- paste(
    apply(crd, 1, function(x) {paste(x[1:2], collapse = " ")}),
    collapse = ",")

  query <- list(
    wkt = paste0(type, "(", crd, ")")
  )
  query <- c(query, arguments)
  query <- c(query,
             email = get_nrel_api_email(),
             api_key = get_nrel_api_key()
             )
  # browser()
  x <- httr::GET(url = api_url, query = query,
                 config = httr::config(connecttimeout = timeout)
                 )
  if (as == "raw") return(x)
  x <- httr::content(x, as, encoding = "UTF-8")
  return(x)
}

if (F) {
  x <- nrel_fetch_coord(lon = 80, lat = 15,
                       api_url = nrel_get_url("india-wind"),
                       attributes = "windspeed_40m,windspeed_80m,windspeed_100m,windspeed_120m",
                       interval = 15,
                       names = "2014", as = "parsed")
  x$status_code

  y <- httr::content(x, "text")
  write.csv(y, "tmp/test_file.csv")
  library(data.table)
  fread("tmp/test_file.csv", skip = 4)

}

#' Get NREL's API addresses by the database name
#'
#' @param x a short name of the database
#'
#' @return character string with the URL address
#' @export
#'
#' @examples
nrel_get_url <- function(x) {
  # browser()
  if (grepl("^site.count$", x, ignore.case = T))
    return("https://developer.nrel.gov/api/wind-toolkit/v2/site-count.json")
  if (grepl("^india.wind$", x, ignore.case = T))
    return("https://developer.nrel.gov/api/wind-toolkit/v2/wind/india-wind-download.csv")
  if (grepl("^wtk$", x, ignore.case = T))
    return("https://developer.nrel.gov/api/wind-toolkit/v2/wind/wtk-download.csv")
  if (grepl("^mexico.(wind|wtk)(|.download)$", x, ignore.case = T))
    return("https://developer.nrel.gov/api/wind-toolkit/v2/wind/mexico-wtk-download.csv")
  # if (grepl("^wtk$", x, ignore.case = T))
}

if (F) {
  nrel_get_url("site-count")
  nrel_get_url("site count")
  nrel_get_url("India wind")
  nrel_get_url("mexico.wtk")
  nrel_get_url("mexico wind")

}


#' Extract meta-data from NREL's API response
#'
#' @param x raw response object returned by `httr::GET` function used in
#'
#' @return
#' @export
#'
#' @examples
nrel_read_meta <- function(x) {
  # x - response object
  stopifnot(x$status_code == 200)
  y <- httr::content(x, "text", encoding = "UTF-8")
  z <- data.table::fread(y, nrows = 1, skip = 0)
  if (!any(grepl("itude", unlist(z[1,])))) {
    # try to read the second rowLatitude
    z2 <- data.table::fread(y, nrows = 1, skip = 1)
    z3 <- data.table::fread(y, nrows = 1, skip = 2)
    stopifnot(any(grepl("itude", unlist(z2[1,]))))
    stopifnot(any(grepl("itude", unlist(z3[1,]))))
    z <- cbind(z, z2, z3)
    nskip <- 3
  }
  ii <- rep(c(T, F), length.out = ncol(z))
  meta <- as.numeric(as.character(z)[!ii])
  names(meta) <- as.character(z)[ii]
  meta <- as.list(meta) %>% data.table::as.data.table()
  return(meta)
}

#' Extract content from NREL's API response
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
nrel_read_responce <- function(x) {
  # x - response object
  stopifnot(x$status_code == 200)
  # read meta data
  y <- httr::content(x, "text", encoding = "UTF-8")
  z <- data.table::fread(y, nrows = 1, skip = 0)
  if (!any(grepl("itude", unlist(z[1,])))) {
    # try to read the second rowLatitude
    z2 <- data.table::fread(y, nrows = 1, skip = 1)
    z3 <- data.table::fread(y, nrows = 1, skip = 2)
    stopifnot(any(grepl("itude", unlist(z2[1,]))))
    stopifnot(any(grepl("itude", unlist(z3[1,]))))
    z <- cbind(z, z2, z3)
    nskip <- 3
  } else {
    nskip <- 1
  }
  ii <- rep(c(T, F), length.out = ncol(z))
  meta <- as.numeric(as.character(z)[!ii])
  names(meta) <- as.character(z)[ii]
  meta <- as.list(meta) %>% data.table::as.data.table()
  # read data
  dat <- fread(text = y, skip = nskip)
  return(list(meta = meta, data = dat))
}


#' Download data from NREL sampling from spatial points/grid.
#'
#' @param grid_sf sf object with coordinates of points to download
#' @param randomize logical, random sampling of locations
#' @param continue logical, should the process continue (based on info in the `grid_sf`)
#' @param save_by integer, saving the data by sample of this size
#' @param save_dir directory to save the data
#' @param file_prefix prefix to file names
#' @param verbose should the process be reported to console
#' @param limit maximum number of queries to NREL
#' @param sleep_after_finish logical, system sleep/hibernation after finishing
#' @param api_url url of NREL's collection
#' @param collection collection short name (not all available yet)
#' @param ... parameters/attributes for the query (passed to `nrel_fetch_coord`)
#' @param plot_process logical, should the process be visualized
#' @param workers number of workers save downloaded data, default 2, one for fetching, second for saving.
#'
#' @return
#' @export
#'
#' @examples
nrel_fetch_points <- function(
    grid_sf = NULL,
    randomize = TRUE,
    continue = is.null(grid_sf),
    save_by = 100,
    save_dir = NULL,
    file_prefix = NULL,
    verbose = TRUE,
    plot_process = TRUE,
    limit = 1e4,
    workers = 2,
    sleep_after_finish = FALSE,
    api_url = NULL,
    collection = "wtk",
    ...
   ) {
  # browser()
  if (continue) {
     ff <- list.files(save_dir, paste0("^", file_prefix, ".+RData"),
                     ignore.case = TRUE, full.names = T)
    if (length(ff) == 0) {
      warning("No `", file_prefix,"*_*.RData` files have been found.")
    } else {
      f <- sort(ff, decreasing = T)[1]
      cat("Found", length(ff), "files.\nStarting from:\n", f, "\n")
      (load(f))

      if (!is.null(nrel_data$RND)) RND <- nrel_data$RND
      if (!is.null(grid_sf)) warning("Replacing given `grid_sf` object with saved in`",
                                     basename(f), "` file.")
      grid_sf <- nrel_data$grid_sf
    }
  }
  if (is.null(api_url)) {
    q_api_url <- nrel_get_url(collection)
  } else {
    q_api_url <- api_url
  }
  # browser()
  if (is.null(grid_sf$index)) {
    idx <- 1:nrow(grid_sf)
    grid_sf <- grid_sf %>% mutate(index = idx, .before = 1)
  }
  if (is.null(grid_sf$fetched)) {
    grid_sf <- grid_sf %>% mutate(fetched = FALSE, .after = "index")
  }
  if (is.null(grid_sf$nrel_lon)) {
    grid_sf <- grid_sf %>% mutate(nrel_lon = as.numeric(NA), .after = "fetched")
  }
  if (is.null(grid_sf$nrel_lat)) {
    grid_sf <- grid_sf %>% mutate(nrel_lat = as.numeric(NA), .after = "nrel_lon")
  }
  if (is.null(grid_sf$file)) {
    grid_sf <- grid_sf %>% mutate(file = as.character(NA), .after = "nrel_lat")
  }
  if (!continue) {
    grid_sf$fetched <- FALSE
    grid_sf$file <- as.character(NA)
  }
  N <- sum(!grid_sf$fetched)
  NF <- sum(grid_sf$fetched)
  # if (!exists("RND")) RND <- sample(1:N, N)
  # unique(RND) %>% length(); N

  if (!exists("RND", 1)) {
    RND <- grid_sf$index[!grid_sf$fetched]
    if (randomize) RND <- sample(RND, length(RND), replace = FALSE)
    RND <- RND[1:min(length(RND), limit)]
  }

  NN <- ceiling(length(RND) / save_by)
  # if (is.null(cells)) {
  #   RND <- 1:min(nmax, N)
  # } else {
  #   RND <- cells
  # }
  # N <- nmax <- length(RND)

  # if (!exists("ll"))

  if (verbose) {
    cat("Total points (locations) in the grid:", nrow(grid_sf), "\n")
    cat("Number of points to sample from:", N, "\n")
    cat("Estimated number of files:", NN, "\n")
  }

  if (!dir.exists(save_dir)) {
    cat("Creating directory:", save_dir, "\n")
    dir.create(save_dir)
  }

  future::plan(multisession, workers = workers)
  on.exit(future::plan(sequential))

  coo <- st_coordinates(grid_sf) %>% as.data.table()
  # coo
  if (plot_process) {
    par("mai") # 1.02 0.82 0.82 0.42
    par(mai = rep(0, 4))
    plot(st_geometry(grid_sf), pch = ".", col = "darkgrey")
    if (any(grid_sf$fetched)) {
      ii_p <- grid_sf$fetched & !grepl("failed$", grid_sf$file)
      ii_n <- grid_sf$fetched & grepl("failed$", grid_sf$file)
      plot(st_geometry(grid_sf[ii_p, ]), pch = 16, cex = .4, col = "orange", add = T)
      plot(st_geometry(grid_sf[ii_n, ]), pch = 16, cex = .3, col = "red", add = T)
    }
  }
  # coo <- coo[!grid_sf$fetched,]
  # browser()
  for (nn in 0:(NN - 1)) {
    ids <- (nn * save_by + 1):min(((nn + 1) * save_by), nrow(grid_sf), length(RND))
    fname <- file.path(save_dir,
                       paste0(file_prefix, "_",
                              formatC(min(ids) + NF, width = 5, flag = "0"),
                              "_",
                              formatC(max(ids) + NF, width = 5, flag = "0"),
                              ".RData"))
    if (file.exists(fname)) {
      fname <- gsub(".RData$", paste0(" (", format(Sys.time()), ").RData"), fname)
      fname <- gsub("[:; ]", "-", fname)
    }
    ll <- listenv()
    # for (i in (length(ll) + 1):N) {
    cat(" # | index | status | ratelimit-remaining | timing\n")
    for (i in ids) {
      n <- RND[i]
      cat(i, "|" , n, "|")
      # browser()
      tic <- Sys.time()
      ll[[i]] <- try({
        nrel_fetch_coord(
          lon = coo$X[n], lat = coo$Y[n], api_url = q_api_url, ..., as = "raw")
      })
      if (as.numeric(ll[[i]]$status_code) != 200) {
        # message(paste("status_code =", ll[[i]]$status_code))
        message(paste(ll[[i]]$status_code))
        Sys.sleep(10)
        cat(i, "|" , n, "|")

        ll[[i]] <- try({
          nrel_fetch_coord(
            lon = coo$X[n], lat = coo$Y[n], api_url = q_api_url,
            ..., as = "raw")
        })
      }
      grid_sf$fetched[n] <- T
      if (as.numeric(ll[[i]]$status_code) == 200) {
        toc <- Sys.time()
        lmt <- ll[[i]]$all_headers[[1]]$headers$`x-ratelimit-remaining`
        cat(ll[[i]]$status_code, " | ", lmt, " | ", format(toc - tic))
        if (lmt < 100) {
          Sys.sleep(10)
        } else {
          # Sys.sleep(1)
        }
        mi <- nrel_read_meta(ll[[i]])
        points(mi$Longitude, mi$Latitude, col = "blue", pch = 1, cex = .5)
        grid_sf$nrel_lon <- mi$Longitude
        grid_sf$nrel_lat <- mi$Latitude
        grid_sf$file[n] <- basename(fname)
      } else {
        # message(paste("status_code =", ll[[i]]$status_code))
        msg_failed <- paste(ll[[i]]$status_code, " - failed")
        message(msg_failed)
        grid_sf$file[n] <- msg_failed; rm(msg_failed)
        points(coo$X[n], coo$Y[n], col = "red", pch = 3, cex = .5)
      }
      cat("\n")
    }

    # length(ll)
    # sapply(ll, function(x) x$status_code) %>% unique()
    # browser()
    nrel_data <- list(raw = ll, grid_sf = grid_sf, RND = RND)
    message("saving: ", fname, " (in parallel session).")
    f <- future({save(nrel_data, file = fname)})
  }
  cat("done\n")
  if (sleep_after_finish) installr::os.sleep(first_turn_hibernate_off = F)
  return(invisible(nrel_data))
#
#   if (!is.null(file_name)) {
#     save(nm, file = file_name);
#     if (sleep_after_finish) installr::os.sleep(first_turn_hibernate_off = F)
#     return(invisible(nm))
#   } else {
#     return(invisible(nm))
#   }
}

