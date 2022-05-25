.onLoad <- function(libname, pkgname) {
  if (file.exists("~/.nrel.dev.api")) {
    source("~/.nrel.dev.api")
  } else {
    warning("The API key is not set.\n
            Use '?set_nrel_api_key' for help")
  }
  options(nrel.dev.api.verbose = TRUE)
}

#' Set API key and save it in a hidden file. The key will be retrieved with loading the package.
#'
#' @param nrel.api.key the key received from NREL developer network.
#' @param nrel.api.email API-registered email address
#'
#' @return
#' `~/.nrel.dev.api` file with saved key and added to the environment options.
#' @export
#' @examples
#' DO NOT RUN
#' set_nrel_api_key("YOUR_API_KEY")
set_nrel_api_key <- function(nrel.api.email, nrel.api.key) {
  options(nrel.api.key = nrel.api.key)
  con <- file("~/.nrel.dev.api")
  writeLines(c(
    paste0("options(nrel.api.email = '", nrel.api.email, "')"),
    paste0("options(nrel.api.key = '", nrel.api.key, "')")),
    con)
  close(con)
}

#' Retrieve saved API key
#'
#' @param invisible Should the key be invisible (TRUE) or printed in the console (FALSE)?
#'
#' @return
#' The key
#' @export
#'
#' @examples
#'   get_nrel_api_key()
#'   get_nrel_api_key(F)
get_nrel_api_key <- function(invisible = TRUE) {
  x <- getOption("nrel.api.key")
  if (invisible) {
    invisible(x)
  } else {
    return(x)
  }
}

#' Retrieve saved API email address
#'
#' @param invisible
#'
#' @return
#' @export
#'
#' @examples
get_nrel_api_email <- function(invisible = TRUE) {
  x <- getOption("nrel.api.email")
  if (invisible) {
    invisible(x)
  } else {
    return(x)
  }
}


if (F) {
  set_nrel_api_key("YOUR_API_KEY")
  get_nrel_api_key()
  get_nrel_api_key(F)
  get_nrel_api_email(F)
}

