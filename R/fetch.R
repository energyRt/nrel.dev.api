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
    return("https://developer.nrel.gov/api/wind-toolkit/v2/site-count.csv")
  if (grepl("^india.wind$", x, ignore.case = T))
    return("https://developer.nrel.gov/api/wind-toolkit/v2/wind/india-wind-download.csv")
}

if (F) {
  get_nrel_url("site-count")
  get_nrel_url("site count")
  get_nrel_url("India wind")
}
