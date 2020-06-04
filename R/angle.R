
#' Determine internal angles along a track
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
#' @rdname trackAngle
#' @param x trip object, or matrix of 2-columns, with x/y coordinates
#' @return Vector of angles (degrees) between coordinates.
#' @export trackAngle
trackAngle <- function(x) {
  UseMethod("trackAngle")
}

#' @rdname trackAngle
#' @method trackAngle trip

#' @export
trackAngle.trip <- function(x) {
  isproj <- is.projected(x)
  if (is.na(isproj)) {
    warning("object CRS is NA, assuming longlat")
  } else {
    if (isproj) {
      x <- reproj(x, .llproj())
    }
  }
  st <- split(x, x[[getTORnames(x)[2]]])
  unlist(lapply(st, function(x1) c(NA, trackAngle(coordinates(x1)), NA)))

}

#' @rdname trackAngle
#' @method trackAngle default
#' @export
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
##' @rdname trip-internal
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

