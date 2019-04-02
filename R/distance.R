# track_distance <- function(lon, lat, group = NULL) {
#   if (is.null(group)) group <- "1"
#   x <- tibble::tibble(lon = lon, lat = lat, group = as.character(group))
#   unlist(lapply(split(x[c("lon", "lat")], x[["group"]])[unique(x[["group"]])], 
#                 function(atrip) geodist::geodist(atrip, sequential = TRUE, pad = TRUE, measure = "haversine")))
# }

##' @rdname trip-internal
.distances <- function(x) {
  proj <- is.projected(x)
  if (is.na(proj)) proj <- FALSE
  lapply(split(x, x[[getTORnames(x)[2]]]), function(x) trackDistance(coordinates(x), longlat = !proj))
  
}
## taken from package sp/src/gcdist.c
##' @rdname trip-internal
.gcdist.c <- function(lon1, lat1, lon2, lat2) {
  DE2RA <- pi / 180
  a <- 6378.137            # /* WGS-84 equatorial radius in km */
  f <- 1.0 / 298.257223563 # /* WGS-84 ellipsoid flattening factor */
  lat1R <- lat1 * DE2RA
  lat2R <- lat2 * DE2RA
  lon1R <- lon1 * DE2RA
  lon2R <- lon2 * DE2RA
  F <- ( lat1R + lat2R ) / 2.0
  G <- ( lat1R - lat2R ) / 2.0
  L <- ( lon1R - lon2R ) / 2.0
  sinG2 <- sin( G ) ^ 2
  cosG2 <- cos( G ) ^ 2
  sinF2 <- sin( F ) ^ 2
  cosF2 <- cos( F ) ^ 2
  sinL2 <- sin( L ) ^ 2
  cosL2 <- cos( L ) ^ 2
  S <- sinG2 * cosL2 + cosF2 * sinL2
  C <- cosG2 * cosL2 + sinF2 * sinL2
  w <- atan( sqrt( S / C ) )
  R <- sqrt( S * C ) / w
  D <- 2 * w * a
  H1 <- ( 3 * R - 1 ) / ( 2 * C )
  H2 <- ( 3 * R + 2 ) / ( 2 * S )
  dist <- D * ( 1 + f * H1 * sinF2 * cosG2 - f * H2 * cosF2 * sinG2 )
  ##dist <- ifelse((abs(lat1 - lat2) < .Machine$double.eps) & (abs(lon1 - lon2) < .Machine$double.eps), 0.0, dist)
  #dist <- ifelse(abs((abs(lon1) + abs(lon2)) - 360.0) < .Machine$double.eps, 0.0, dist)
  dist[is.na(dist)] <- 0
  dist
}

##trackDistance <- function(x) UseMethod("trackDistance")


#' Determine distances along a track
#'
#'
#' Calculate the distances between subsequent 2-D coordinates using Euclidean
#' or Great Circle distance (WGS84 ellipsoid) methods.
#'
#'
#' If \code{x1} is a trip object, arguments \code{x2}, \code{x3}, \code{y2} are
#' ignored and the return result has an extra element for the start point of
#' each individual trip, with value 0.0.
#'
#' The \code{prev} argument is ignore unless x1 is a trip.
#'
#' Distance values are in the units of the input coordinate system when longlat
#' is FALSE, and in kilometres when longlat is TRUE.
#'
#' This originally used \code{\link[sp]{spDistsN1}} but now implements the sp
#' \code{gcdist} source directly in R.
#'
#' @aliases trackDistance trackDistance.default trackDistance.trip
#' @param x1 trip object, matrix of 2-columns, with x/y coordinates OR a vector
#' of x start coordinates
#' @param x2 vector of x end coordinates, if x1 is not a matrix
#' @param y1 vector of y start coordinates, if x1 is not a matrix
#' @param y2 vector of y end coordinates, if x1 is not a matrix
#' @param longlat if FALSE, Euclidean distance, if TRUE Great Circle distance
#' @param prev if TRUE and x1 is a trip, the return value has a padded end
#' value (\"prev\"ious), rather than start (\"next\")
#' @return Vector of distances between coordinates.
#' @references Original source taken from sp package.
#' @author Roger Bivand and Michael Sumner
#' @examples
#'   ## Continuing the example from '?"trip-methods"':
#' utils::example("trip-methods", package="trip",
#'                ask=FALSE, echo=FALSE)
#'
#'  ## the method knows this is a trip, so there is a distance for every
#'  ## point, including 0s as the start and at transitions between
#'  ## individual trips
#' trackDistance(tr)
#'
#' ## the default method does not know about the trips, so this is
#' ##(n-1) distances between all points
#' ## trackDistance(coordinates(tr), longlat = FALSE)
#'
#' ## we get NA at the start, end and at transitions between trips
#'
#'  \dontrun{
#'  require(rgdal)
#'  trackAngle(tr)
#'  }
#' @export trackDistance
trackDistance <- function(x1, y1, x2, y2, longlat=TRUE, prev = FALSE) UseMethod("trackDistance")

##' @export
trackDistance.default <- function(x1, y1, x2, y2, longlat=TRUE, prev = FALSE) {
  if (missing(y1)) {
    if (!is.matrix(x1))
      stop("x1 is not a matrix and multiple arguments not specified")
    if (nrow(x1) < 2) stop("x1 has too few rows")
    if (ncol(x1) < 2) stop("x1 has too few columns")
    x2 <- x1[-1, 1]
    y1 <- x1[-nrow(x1), 2]
    y2 <- x1[-1, 2]
    x1 <- x1[-nrow(x1), 1]
  }
  nx <- length(x1)
  if (nx != length(y1) | nx != length(x2) | nx != length(y2))
    stop("arguments must have equal lengths")
  if (longlat) {
    # browser()
    # rbenchmark:::benchmark(geodist::geodist(cbind(lon = c(x1, tail(x2, 1)), lat = c(y1, tail(y2, 1))), paired = FALSE, sequential = TRUE, measure = "vincenty")/1000, 
    #                        geodist::geodist(cbind(x1, y1), cbind(x2, y2), paired = TRUE, sequential = TRUE, measure = "vincenty")/1000)
    # 
    ##.gcdist.c(x1, y1, x2, y2)  ## this is faster than geodist for small data
    geodist::geodist(cbind(lon = c(x1, tail(x2, 1)), lat = c(y1, tail(y2, 1))), sequential = TRUE, paired = FALSE, measure = "vincenty")/1000
  } else sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
}

##' @export
trackDistance.trip <- function(x1, y1, x2, y2, longlat = TRUE, prev = FALSE) {
  unlist(lapply(.distances(x1), function(x) if (prev) {c(x, 0)} else {c(0, x)}))
}


##' Calculate maximum distance from 'home' for each trip
##'
##' This function returns a distance from a given 'home' coordinate for each individual trip. 
##' Use the \code{home} argument to provide a single, common 2-element (x,y or lon,lat) coordinate. If \code{home}
##' is \code{NULL} (the default), then each individual trip's first location is used. 
##' @param x trip object
##' @param home see details
##' @seealso \code{\link[sp]{spDistsN1}}
##' @export
##' @return numeric vector of distances in km (for longlat), or in the units of the trip's projection 
##' @export
homedist <- function(x, home = NULL) {
  if (!is.null(home)) {
    if (is.numeric(home) & length(home) == 2) {
    home <- matrix(home, ncol = 2)
    } else {
      stop("stop, home must be a 2-element numeric")
    }
  }  
  ## iterate over individual trips
  tor <- trip::getTORnames(x)
  ids <- unique(x[[tor[2L]]])
  dists <- numeric(length(ids))
  names(dists) <- as.character(ids)
  longlat <- !is.projected(x)
  if (is.na(longlat)) {
    longlat <- TRUE
    warning("coordinate system is NA, assuming longlat . . .")
  }
  for (i in seq_along(ids)) {
    x0 <- coordinates(x[x[[tor[2L]]] == ids[i], ])
    if (is.null(home)) home <- x0[1, , drop = FALSE]
    dists[i] <- max(spDistsN1(x0, home, longlat = longlat))
  }
  dists
}
