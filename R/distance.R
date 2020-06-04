
##' @rdname trip-internal
.distances <- function(x) {
  proj <- is.projected(x)
  if (is.na(proj)) proj <- FALSE
  lapply(split(x, x[[getTORnames(x)[2]]]), function(x) trackDistance(coordinates(x), longlat = !proj))
  
}




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
#' This originally used \code{\link[sp]{spDistsN1}}, then implemented the sp
#' \code{gcdist} source directly in R, and now uses \code{\link[geodist]{geodist}}.
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
#' @references Original source taken from sp package, but now using Helmert from Karney (2013)
#' see the geodist package.
#' @examples
#'  d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
#' sp::coordinates(d) <- ~x+y
#' ## this avoids complaints later, but these are not real track data (!)
#' sp::proj4string(d) <- sp::CRS("+proj=laea +ellps=sphere", doCheckCRSArgs = FALSE)
#' tr <- trip(d, c("tms", "id"))
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
#'  angles <- trackAngle(walrus818)
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
    geodist::geodist(cbind(lon = c(x1, tail(x2, 1)), lat = c(y1, tail(y2, 1))), 
                     sequential = TRUE, paired = FALSE, measure = "geodesic")/1000
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
    colnames(home) <- c("x", "y")
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
    if (longlat) {
      #colnames(x0) <- c("x", "y")
      
      dists[i] <- suppressMessages(max(geodist::geodist(x0, home, measure = "geodesic"))/1000)
    } else {
      dists[i] <- max(sqrt((x0[,1] - home[1])^2 + (x0[,2] - home[2])^2))
    }
  }
  dists
}
