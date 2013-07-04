# $Id: trackDistance.R 74 2013-03-21 15:28:34Z sluque $

## taken from package sp/src/gcdist.c
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
    dist <- ifelse(abs(lat1 - lat2) < .Machine$double.eps, 0.0, dist)
    dist <- ifelse(abs(lon1 - lon2) < .Machine$double.eps, 0.0, dist)
    dist <- ifelse(abs((abs(lon1) + abs(lon2)) - 360.0) <
                   .Machine$double.eps, 0.0, dist)
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
#' @keywords manip
#' @export trackDistance
trackDistance <- function(x1, y1, x2, y2, longlat=TRUE, prev = FALSE) UseMethod("trackDistance")

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
        trip:::.gcdist.c(x1, y1, x2, y2)
    } else sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
}

trackDistance.trip <- function(x1, y1, x2, y2, longlat = TRUE, prev = FALSE) {
    unlist(lapply(.distances(x1), function(x) if (prev) {c(x, 0)} else {c(0, x)}))
}




#' Determine distances or angles along a track
#' 
#' 
#' Calculate the angles between subsequent 2-D coordinates using Great Circle
#' distance (spherical) methods.
#' 
#' If \code{x} is a trip object, the return result has an extra element for the
#' start and end point of each individual trip, with value NA.
#' 
#' This is an optimized hybrid of "raster::bearing" and
#' \code{\link[maptools]{gzAzimuth}}.
#' 
#' @aliases trackAngle trackAngle.default trackAngle.trip
#' @param x trip object, or matrix of 2-columns, with x/y coordinates
#' @return Vector of angles (degrees) between coordinates.
#' @keywords manip
#' @export trackAngle
trackAngle <- function(x) {
  UseMethod("trackAngle")
}

trackAngle.trip <- function(x) {
  isproj <- is.projected(x)
  if (is.na(isproj)) {
    warning("object CRS is NA, assuming longlat")
  } else {
    if (isproj) {
      x <- try(spTransform(x, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
      if (inherits(x, "try-error")) {
        stop("Object x is projected, attempts to transform to longlat failed. Is rgdal installed?")
      }
    }
  }
  st <- split(x, x[[getTORnames(x)[2]]])
  unlist(lapply(st, function(x1) c(NA, trackAngle(coordinates(x1)), NA)))

}

trackAngle.default <- function(x) {
  n <- nrow(x)
  ## MDSumner 2013-06-14 not sure what to expose here, will start with optimized gzAzimuth(abdali)/bearing() hybrid
  ##if (type == "geosphere") {
  ##  require(geosphere)
  ##  angle <- bearing(xy[2:(n-1),],xy[3:n,]) - bearing(xy[2:(n-1),],xy[1:(n-2),])
  ##} else {
  ##  if(!type == "abdali") stop(sprintf("type '%s' not implemented", type))
    angle <- .abdali(x[2:(n-1),],x[3:n,]) - .abdali(x[2:(n-1),],x[1:(n-2),])

  ##}
  angle <- (angle+180)%%360-180
  abs(angle)
}


## "abdali", replacement for raster::bearing

.abdali <- function (p1, p2)
{
  stopifnot(nrow(p1) == nrow(p2))

  toRad <- pi/180
  p1 <- p1 * toRad
  p2 <- p2 * toRad
  keep <- !(.rowSums(p1 == p2, nrow(p1), ncol(p1)) == 2L)
  res <- rep(as.numeric(NA), length = nrow(p1))
  if (sum(keep) == 0) {
    return(res)
  }
  p1 <- p1[keep, , drop = FALSE]
  p2 <- p2[keep, , drop = FALSE]
  dLon = p2[, 1] - p1[, 1]
  y = sin(dLon)
  x = cos(p1[, 2]) * tan(p2[, 2]) - sin(p1[, 2]) * cos(dLon)
  azm = atan2(y, x)/toRad
  res[keep] <- (azm + 360)%%360
  return(res)
}


##n <- 10000;x <- cbind(runif(n, -180, 180), runif(n, -90, 90));

##max(abs(trackAngle(x) - trackAngle(x, type = "abdali")))
## [1] 1.136868e-13

##library(rbenchmark)

##n <- 5000;x <- cbind(runif(n, -180, 180), runif(n, -90, 90));

##benchmark(trackAngle(x, type = "geosphere"), trackAngle(x, type = "abdali"), replications = 300)
##test replications elapsed relative user.self sys.self user.child sys.child
##2    trackAngle(x, type = "abdali")          300    1.62    1.000      1.62        0         NA        NA
##1 trackAngle(x, type = "geosphere")          300    8.49    5.241      8.49        0         NA        NA


###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
