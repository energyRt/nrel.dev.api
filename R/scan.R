
fetch_site_counts <- function(gis_sf) { # rename

  crd <- st_coordinates(gis_sf)
  stopifnot(nrow(crd) == 5)

  crd <- paste(
    apply(crd, 1, function(x) {paste(x[1:2], collapse = " ")}),
    collapse = ",")

  query <- list(
    wkt = paste0("POLYGON((", crd, "))"),
    api_key = get_nrel_api_key()
  )

  x <- httr::GET(url = nrel_get_url("site.count"), query = query)
  y <- httr::content(x, "parsed", encoding = "UTF-8")
  y$outputs$total <- sum(unlist(y$outputs), na.rm = T)
  # y$total <- sum(unlist(y$outputs), na.rm = T)
  return(c(raw = list(x), y))
}

bbox2points <- function(b, closed = T, what = "polygon") {
  # b - bbox
  # browser()
  coo_0 <- rbind(b[c("xmin", "ymin")], b[c("xmin", "ymax")],
                 b[c("xmax", "ymax")], b[c("xmax", "ymin")],
                 b[c("xmin", "ymin")]) # coordinates
  colnames(coo_0) <- c("x", "y")
  if (what == "polygon") {
    bb <- st_sfc(st_polygon(list(coo_0)))
    return(bb)
  } else if (what == "points") {
    bb <- st_as_sf(as.data.frame(coo_0), coords = c("x", "y"))
  } else if (what == "coordinates") {
    bb <- coo_0
  } else {
    stop()
  }
  return(bb)
}


#' Guess grid with locations of data-points in NREL's collection
#'
#' @param gis_sf sf object (map) of the region to estimate a number of data-points in NREL's collection, and their coordinates (grid).
#' @param dx starting step along longitude
#' @param dy starting step along latitude
#' @param q_atributes attributes of the query (see `nrel_fetch_coord`)
#' @param collection name of NREL's data-collection (see `nrel_get_url`)
#' @param q_interval interval parameter in the query
#' @param q_names names parameter in the query
#' @param steps_max maximum steps in evaluation of the grid along each axis, 10 by default
#' @param verbose logical, should the process be reported
#'
#' @return list with the grid in sf points format, grid parameters, fetched initial data-points used for the estimation of the grid.
#' @export
#'
#' @examples
nrel_guess_grid <- function(
    gis_sf,
    dx = .01, dy = .01,
    api_url = NULL, collection = "wtk",
    q_atributes = "windspeed_100m",
    q_interval = 60, q_names = "2014", steps_max = 10,
    verbose = T) {
  # browser()
  if (is.na(st_crs(gis_sf))) {
    stop("CRS is not provided. The map is expected in 'WGS 84' (4326) coordinates.\n See ?sf::st_crs and ?sf::st_transform for details.")
  }
  if (is.null(api_url)) {
    q_api_url <- nrel_get_url(collection)
  } else {
    q_api_url <- api_url
  }

  # centroid as the starting point
  centr <- st_centroid(bbox2points(st_bbox(st_geometry(gis_sf))))
  centr_coo <- st_coordinates(centr)

  # request data for the centroid to get the nearest available coordinates
  rr <- list() # raw response
  mm <- list() # metadata
  dd <- list() # data from responses
  if (verbose) {
    cat("Fetching point #", 1, ", coordinates:",
        # "Coordinates: ",  move$x[i],", ", move$y[i], "\n", sep = "")
    # cat("Requesting data from NREL for coordinates: ",
        centr_coo[1, "X"], ", ", centr_coo[1, "Y"], sep = "")
  }
  rr[[1]] <- nrel_fetch_coord(lon = centr_coo[1, "X"], lat = centr_coo[1, "Y"],
                             api_url = q_api_url, attributes = q_atributes,
                             interval = q_interval, names = q_names, as = "raw")
  if (verbose) {
    cat(", status: ", rr[[1]]$status_code, "\n")
  }
  if (rr[[1]]$status_code != 200) {
    stop("Unsuccessfull request. Check parameters of the the query.")
  }

  y <- nrel_read_responce(rr[[1]])
  mm[[1]] <- y$meta
  dd[[1]] <- y$data
  cent0 <- st_as_sf(y$meta, coords = c("Longitude", "Latitude"))
  cent0_coord <- st_coordinates(cent0)
  cc0 <- as.list(as.data.frame(cent0_coord)) # shorter version

  # evaluate grid in the NREL's collection
  move <- data.frame(
    dir_x = c(0, 1, 0, -1), # direction along x-axis relative to previous
    dir_y = c(0, 0, 1, 0),  # direction along y-axis
    x = as.numeric(cc0$X),
    y = as.numeric(cc0$Y)
  )
  dy_v <- 0 # estimated drift of y while moving along x
  dx_h <- 0 # estimated drift of x while moving along y
  if (verbose) {
    cat("Evaluating the grid cell size and rotation. \n",
        "Initial cell size: dX = ", dx, ", dY = ", dy, "\n", sep = "")
  }
  ## samples to evaluate the grid
  dx_drift <- dy_drift <- 0
  for (i in 2:4) {
    nstep <- 1
    while (nstep <= steps_max) {
      # cat(i, nstep, dx, dy, dx_drift, dy_drift, "\n")
      move$x[i] <- move$x[i - 1] + dx * move$dir_x[i] + dx_drift * move$dir_y[i]
      move$y[i] <- move$y[i - 1] + dy * move$dir_y[i] + dy_drift * move$dir_x[i]
      if (verbose) {
        cat("Fetching point #", i, ", ",
            "coordinates: ",  move$x[i],", ", move$y[i], "\n", sep = "")
      }
      rr[[i]] <- nrel_fetch_coord(
        lon = move$x[i], lat = move$y[i], api_url = q_api_url,
        attributes = q_atributes, interval = q_interval, names = q_names, as = "raw")
      if (rr[[i]]$status_code != 200) {
        stop("Unsuccessfull request. Check parameters of the the query.")
      }
      # rr[[i]]$status_code
      y <- nrel_read_responce(rr[[i]])
      mm[[i]] <- y$meta
      dd[[i]] <- y$data
      move$x[i] <- y$meta$Longitude
      move$y[i] <- y$meta$Latitude

      if (move$dir_x[i] != 0) {
        if (mm[[i]]$Longitude == mm[[i - 1]]$Longitude) {
          dx <- dx + .01
          if (verbose) cat("Updating dx =", dx, "\n")
        } else {
          # hh[[2]] <- h
          dx <- (mm[[i]]$Longitude - mm[[i - 1]]$Longitude) * move$dir_x[i]
          dy_drift <- (mm[[i]]$Latitude - mm[[i - 1]]$Latitude) * move$dir_x[i]
          break
        }
      } else if (move$dir_y[i] != 0) {
        if (mm[[i]]$Latitude == mm[[i - 1]]$Latitude) {
          dy <- dy + .01
          if (verbose) cat("Updating dy =", dy, "\n")
        } else {
          dy <- (mm[[i]]$Latitude - mm[[i - 1]]$Latitude) * move$dir_y[i]
          dx_drift <- (mm[[i]]$Longitude - mm[[i - 1]]$Longitude) * move$dir_y[i]
          break
        }
      } else {
        browser() # should not be here - debug
      }
      nstep <- nstep + 1
    }
  }
  # making the grid for the bbox of given gis_sf
  ## fetched points for the grid
  h <- rbindlist(c(mm, mm[1]), use.names = F) # names can be inconsistent (dots and spaces)
  pts <- st_as_sf(h, coords = c("Longitude", "Latitude"), crs = st_crs(gis_sf))
  if (F) { # check / debug
    dx; dy
    unique(h)
    ggplot() +
      geom_sf(data = pts)
  }
  ## check that site counts == 4
  # st_buffer(pts, dist = 1)
  sc <- fetch_site_counts(pts)
  stopifnot(sc$outputs[[collection]] == 4) # TRUE

  # rotate the conner-points
  # # https://cran.r-project.org/web/packages/sf/vignettes/sf3.html
  # rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2) # rot(pi/4)
  gpts <- st_geometry(pts) # grid "conner" points

  ang <- diff(h$Latitude[1:2]) / diff(h$Longitude[1:2]) # angle
  if (verbose) cat("Estimated cell size: dx = ", dx, ", dy = ", dy,
                   ", angle = ", tan(ang), "\n", sep = "")
  # ang; tan(ang)
  pts2 <- rotate_geom(geom = gpts, ang = ang, centr = gpts[1])
  # pts2 <- (gpts - gpts[1]) * rot(tan(ang)) + gpts[1] # rotated points
  dxr <- diff(st_coordinates(pts2)[1:2,"X"]) # dx of roteted grid
  dyr <- diff(st_coordinates(pts2)[c(1,4),"Y"]) # dy of rotated grid
  drift <- diff(st_coordinates(pts2)[c(1,4),"X"]) # drift of Y in rotated grid

  # rotate the given gis_sf around the its centroid
  # if (verbose) cat("Rotating geometry", "\n", sep = "")
  # browser()
  gis_rot_geom <- rotate_geom(geom = st_geometry(gis_sf), #[,1:2]
                               ang = ang, centr = centr)
  # gis_rot_geom <- (st_geometry(gis_sf) - centr) * rot(tan(ang)) + centr
  st_crs(gpts) <- st_crs(gis_sf)
  st_crs(pts2) <- st_crs(gis_sf)
  st_crs(gis_rot_geom) <- st_crs(gis_sf)
  # extent bbox to cover the drift
  bx_rot <- gis_rot_geom %>%
    st_bbox() %>%
    bbox2points() %>%
    st_set_crs(st_crs(gis_sf))

  bx_coo <- bx_rot %>%
    st_coordinates() %>%
    as.data.frame()

  rx <- range(bx_coo$X) %>% diff()
  ry <- range(bx_coo$Y) %>% diff()
  sqrt(st_area(bx_rot))

  bx_rot_buff <- bx_rot %>%
    st_buffer(dist = (sqrt(st_area(bx_rot)) * abs(drift) * 5e1),
              nQuadSegs = 1,
              # endCapStyle = "FLAT", joinStyle = "BEVEL"
    ) %>%
    st_set_crs(st_crs(gis_sf))

  if (F) {
    ggplot() +
      geom_sf(data = gis_sf, fill = "wheat") +
      geom_sf(data = bx_rot_buff, fill = NA) +
      geom_sf(data = bx_rot, fill = NA, color = "red")
  }

  if (verbose) cat("Making the grid... ", sep = "")
  grd <- st_make_grid(st_bbox(bx_rot_buff), cellsize = c(dxr, dyr),
                      what = "corners")
  length(grd)
  if (F) { # debug check
    ggplot() +
      geom_sf(data = gis_sf, fill = "wheat") +
      geom_sf(data = gis_rot_geom, fill = NA, color = "blue") +
      geom_sf(data = grd, color = "blue", shape = ".") +
      geom_sf(data = gpts, color = "red", shape = ".") +
      theme_bw()
  }
  coo <- st_coordinates(grd)

  # align the grid with the actual point closest to the centroid of gis_sf
  i_centr <- which.min(abs(coo[,"X"] - cc0$X) + abs(coo[,"Y"] - cc0$Y))
  stopifnot(length(i_centr) == 1)
  corr_x <- coo[i_centr, "X"] - cc0$X
  corr_y <- coo[i_centr, "Y"] - cc0$Y
  coo[i_centr,] - c(corr_x, corr_y) - unlist(cc0) # check == 0,0
  coo[,"X"] <- coo[, "X"] - corr_x
  coo[,"Y"] <- coo[, "Y"] - corr_y

  # adjust for the drift
  coo[,"X"] <- coo[,"X"] + drift * round((coo[,"Y"] - cc0$Y) / dyr)
  # head(coo)
  grid0_sf <- st_as_sf(as.data.frame(coo), coords = c("X", "Y"), crs = st_crs(gis_sf))
  coo0_geom <- st_geometry(grid0_sf)
  # browser()
  if (F) { # debug check
    ggplot() +
      geom_sf(data = gis_sf, fill = "wheat") +
      geom_sf(data = gis_rot_geom, fill = NA, color = "blue") +
      geom_sf(data = grid0_sf, color = "blue", shape = ".") +
      geom_sf(data = gpts, color = "red", shape = ".") +
      theme_bw()
  }

  # rotate grid back around a point closest to its own centroid
  if (verbose) cat("rotating... ", "", sep = "")
  # coo0 <- st_coordinates(coo0_geom)
  # centr_coo <- st_coordinates(centr)
  # coo0[,1] <- coo0[,1] - centr_coo[1,1]
  # coo0[,2] <- coo0[,2] - centr_coo[1,2]
  # coo0 <- coo0 %*% rot(-tan(ang))
  # coo0[,1] <- coo0[,1] + centr_coo[1,1]
  # coo0[,2] <- coo0[,2] + centr_coo[1,2]
  # colnames(coo0) <- c("lon", "lat")
  # coo0 <- as.data.table(coo0)
  # coo_geom <- st_as_sf(coo0, coords = c("lon", "lat"))
  coo_geom <- rotate_geom(geom = coo0_geom, ang = ang, centr = centr)
  # coo_geom <- (coo0_geom - centr) * rot(-tan(ang)) + centr # rotated points
  # bx <- bbox2points(st_bbox(coo_geom))
  # check_sites <- fetch_site_counts(bx)
  # check_sites # 50000 is the upper value
  grid_sf <- st_as_sf(coo_geom, crs = st_crs(gis_sf))
  # gis_rot_geom_back <- (gis_rot_geom - centr) * rot(-tan(ang)) + centr
  gis_rot_geom_back <- rotate_geom(geom = coo0_geom, ang = -ang, centr = centr)
  st_crs(gis_rot_geom_back) <- st_crs(gis_sf)

  if (F) { # debug/check
    ggplot() +
      geom_sf(data = gis_sf, fill = "wheat") +
      # geom_sf(data = gis_rot_geom, fill = NA, color = "blue") +
      geom_sf(data = gis_rot_geom, fill = NA, color = "blue") +
      geom_sf(data = gis_rot_geom_back, fill = NA, color = "red") +
      # geom_sf(data = grd, color = "blue", shape = ".") +
      geom_sf(data = grid_sf, color = "red", shape = ".") +
      geom_sf(data = gpts, color = "red", shape = 3) +
      theme_bw()
  }

  if (verbose) cat("filtering... ", sep = "")
  ii <- st_covered_by(grid_sf, gis_sf, sparse = F, model = "open")

  grid_sf <- grid_sf[c(ii),]

  if (verbose) cat("done. \nTotal estimated locations: ", nrow(grid_sf), sep = "")

  if (F) { # debug/check
    ggplot() +
      geom_sf(data = gis_sf, fill = "wheat") +
      geom_sf(data = grid_sf, fill = NA, color = "red", shape = ".") +
      geom_sf(data = gis_rot_geom, fill = NA, color = "blue") +
      geom_sf(data = gpts, color = "brown", shape = 1) +
      theme_bw()
  }
  # if (verbose) cat("done", "\n", sep = "")

  return(list(
    grid_sf = grid_sf,
    par = list(
      dx = dx,
      dy = dy,
      angle = ang,
      dx_drift = dx_drift,
      dy_drift = dy_drift,
      # rotated parameters
      dxr = dxr,
      dyr = dyr,
      drift = drift
    ),
    data = list(
      points_geom = gpts,
      meta = mm,
      data = dd
    )
  ))
}


# https://cran.r-project.org/web/packages/sf/vignettes/sf3.html
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2) # rot(pi/4)

rotate_geom <- function(geom, ang = 0, centr = NULL) {
  # browser()
  coo <- st_coordinates(geom)[,1:2]
  if (is.null(centr)) {
    centr_coo <- matrix(0, nrow = 1, ncol = 2)
  } else {
    centr_coo <- st_coordinates(centr)
  }
  coo[,1] <- coo[,1] - centr_coo[1,1]
  coo[,2] <- coo[,2] - centr_coo[1,2]
  coo <- coo %*% rot(-tan(ang))
  coo[,1] <- coo[,1] + centr_coo[1,1]
  coo[,2] <- coo[,2] + centr_coo[1,2]
  colnames(coo) <- c("lon", "lat")
  coo <- as.data.table(coo)
  coo_geom <- st_as_sf(coo, coords = c("lon", "lat"))
  # if ()
  return(st_geometry(coo_geom))
  # coo_geom <- (coo0_geom - centr) * rot(-tan(ang)) + centr # rotated points
}
