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


scan_site_counts <- function(x, verbose = TRUE) {


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
#' @param cells integer sequence of row-numbers in `grid_sf` object
#' @param nmax integer maximum number of data-points to download
#' @param plot_process logical, should the process be visualized
#' @param file_name file name with path, if provided, the results will be saved in the file. Otherwise, results will be returned to global environment.
#' @param sleep_when_done logical, if TRUE, puts the computer to sleep when done
#'
#' @return
#' @export
#'
#' @examples
fetch_data_from_grid <- function(grid_sf, cells = NULL, nmax = 100, plot_process = T,
                            file_name = NULL,
                            sleep_after_finish = FALSE) {
  N <- nrow(grid_sf)
  # if (!exists("RND")) RND <- sample(1:N, N)
  # unique(RND) %>% length(); N
  if (is.null(cells)) {
    RND <- 1:min(nmax, nrow(grid_sf))
  } else {
    RND <- cells
  }

  # if (!exists("ll"))
  ll <- list()
  coo <- st_coordinates(grid_sf) %>% as.data.table()
  coo
  if (plot_process) {
    par("mai") # 1.02 0.82 0.82 0.42
    par(mai = rep(0, 4))
    plot(grid_sf, pch = ".")
  }

  for (i in (length(ll) + 1):N) {
    n <- RND[i]
    cat(i, n, "")
    ll[[i]] <- fetch_nrel_data(
      lon = coo$X[n], lat = coo$Y[n],
      api_url = q_api_url, attributes = q_atributes,
      interval = q_interval, names = q_names, as = "raw")
    if (as.numeric(ll[[i]]$status_code) != 200) {
      message(paste("status_code =", ll[[i]]$status_code))
      Sys.sleep(10)
      ll[[i]] <- fetch_nrel_data(
        lon = coo$X[n], lat = coo$Y[n],
        api_url = q_api_url, attributes = q_atributes,
        interval = q_interval, names = q_names, as = "raw")
    }
    if (as.numeric(ll[[i]]$status_code) == 200) {
      lmt <- ll[[i]]$all_headers[[1]]$headers$`x-ratelimit-remaining`
      cat(ll[[i]]$status_code, lmt)
      if (lmt < 100) {
        Sys.sleep(10)
      } else {
        # Sys.sleep(1)
      }
      mi <- nrel_read_meta(ll[[i]])
      points(mi$Longitude, mi$Latitude, col = "red", pch = 1, cex = .5)
    } else {
      message(paste("status_code =", ll[[i]]$status_code))
      points(coo$X[n], coo$Y[n], col = "blue", pch = 3, cex = .5)
    }
    cat("\n")
  }

  length(ll)
  sapply(ll, function(x) x$status_code) %>% unique()

  assign(nm, list(raw = ll, grid_sf = grid_sf, RND = RND)); nm

  if (!is.null(file_name)) {
    save(nm, file = file_name);
    if (sleep_after_finish) installr::os.sleep(first_turn_hibernate_off = F)
    return(invisible(nm))
  } else {
    return(invisible(nm))
  }
}

