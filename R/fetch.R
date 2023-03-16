#' Fetch (request and download) data from NREL
#'
#' @param lon vector with longitude coordinate(s)
#' @param lat vector with latitude coordinate(s)
#' @param api_url character string with the url-address to a particular dataset
#' @param as httr parameter
#' @param ... attributes for the API query
#'
#' @return
#' @export
#'
#' @examples
fetch_nrel_data <- function(lon, lat, api_url = NULL, as = "raw", ...) {
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
  x <- httr::GET(url = api_url, query = query)

  if (as == "raw") return(x)
  x <- httr::content(x, as, encoding = "UTF-8")
  return(x)
}

if (F) {
  x <- fetch_nrel_data(lon = 80, lat = 15,
                       api_url = get_nrel_url("india-wind"),
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
get_nrel_url <- function(x) {
  # browser()
  if (grepl("^site.count$", x, ignore.case = T))
    return("https://developer.nrel.gov/api/wind-toolkit/v2/site-count.json")
  if (grepl("^india.wind$", x, ignore.case = T))
    return("https://developer.nrel.gov/api/wind-toolkit/v2/wind/india-wind-download.csv")
  if (grepl("^wtk$", x, ignore.case = T))
    return("https://developer.nrel.gov/api/wind-toolkit/v2/wind/wtk-download.csv")
  # if (grepl("^india.wind$", x, ignore.case = T))
  #   return("https://developer.nrel.gov/api/wind-toolkit/v2/wind/wtk-download.csv")
}

if (F) {
  get_nrel_url("site-count")
  get_nrel_url("site count")
  get_nrel_url("India wind")
}


#' Extract meta-data from NREL's API response
#'
#' @param x
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


#' Download data from NREL using grid of points.
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
#' @param ... parameters/attributes for the query (passed to `fetch_nrel_data`)
#' @param plot_process logical, should the process be visualized
#'
#' @return
#' @export
#'
#' @examples
fetch_data_from_grid <- function(grid_sf,
                                 randomize = TRUE,
                                 continue = TRUE,
                                 save_by = 100,
                                 save_dir = NULL,
                                 file_prefix = NULL,
                                 verbose = TRUE,
                                 plot_process = TRUE,
                                 limit = 1e4,
                                 sleep_after_finish = FALSE,
                                 api_url = NULL, collection = "wtk",
                                 ...
                                 ) {
  if (is.null(api_url)) {
    q_api_url <- get_nrel_url(collection)
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

  RND <- grid_sf$index[!grid_sf$fetched]
  if (randomize) RND <- sample(RND, length(RND), replace = FALSE)
  RND <- RND[1:min(length(RND), limit)]

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
    cat("Points to attempt to download:", N, "\n")
    cat("Estimated number of files:", NN, "\n")
  }

  if (!dir.exists(save_dir)) {
    cat("Creating directory:", save_dir, "\n")
    dir.create(save_dir)
  }

  coo <- st_coordinates(grid_sf) %>% as.data.table()
  # coo
  if (plot_process) {
    par("mai") # 1.02 0.82 0.82 0.42
    par(mai = rep(0, 4))
    plot(st_geometry(grid_sf), pch = ".", col = "darkgrey")
    if (any(grid_sf$fetched)) {
      plot(st_geometry(grid_sf[grid_sf$fetched, ]), pch = 16, cex = .4,
           col = "green3", add = T)
    }
  }
  # coo <- coo[!grid_sf$fetched,]
  # browser()
  for (nn in 0:(NN - 1)) {
    ids <- (nn * save_by + 1):min(((nn + 1) * save_by), nrow(grid_sf), length(RND))
    fname <- file.path(save_dir,
                       paste0(file_name, "_",
                              formatC(min(ids) + NF, width = 5, flag = "0"),
                              "_",
                              formatC(max(ids) + NF, width = 5, flag = "0"),
                              ".RData"))
    if (file.exists(fname)) {
      fname <- gsub(".RData$", paste0(" (", format(Sys.time()), ").RData"), fname)
    }
    ll <- list()
    # for (i in (length(ll) + 1):N) {
    cat(" # | index | status | ratelimit-remaining\n")
    for (i in ids) {
      n <- RND[i]
      cat(i, "|" , n, "|")
      # browser()
      ll[[i]] <- fetch_nrel_data(
        lon = coo$X[n], lat = coo$Y[n], api_url = q_api_url, ..., as = "raw")
      if (as.numeric(ll[[i]]$status_code) != 200) {
        message(paste("status_code =", ll[[i]]$status_code))
        Sys.sleep(10)
        ll[[i]] <- fetch_nrel_data(
          lon = coo$X[n], lat = coo$Y[n], api_url = q_api_url, ..., as = "raw")
      }
      if (as.numeric(ll[[i]]$status_code) == 200) {
        lmt <- ll[[i]]$all_headers[[1]]$headers$`x-ratelimit-remaining`
        cat(ll[[i]]$status_code, " | ", lmt)
        if (lmt < 100) {
          Sys.sleep(10)
        } else {
          # Sys.sleep(1)
        }
        mi <- nrel_read_meta(ll[[i]])
        points(mi$Longitude, mi$Latitude, col = "blue", pch = 1, cex = .5)
        grid_sf$fetched[n] <- T
        grid_sf$file[n] <- basename(fname)
        grid_sf$nrel_lon <- mi$Longitude
        grid_sf$nrel_lat <- mi$Latitude
      } else {
        message(paste("status_code =", ll[[i]]$status_code))
        points(coo$X[n], coo$Y[n], col = "red", pch = 3, cex = .5)
      }
      cat("\n")
    }

    # length(ll)
    # sapply(ll, function(x) x$status_code) %>% unique()

    nrel_data <- list(raw = ll, grid_sf = grid_sf, RND = RND)
    message("saving: ", fname)
    save(nrel_data, file = fname)
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

