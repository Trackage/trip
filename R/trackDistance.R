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

trackDistance <- function(x1, y1, x2, y2, longlat=TRUE) {
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
  unlist(lapply(st, function(x1) trackAngle(coordinates(x1))))
                      
}
trackAngle.default <- function(xy) {
  angles <- abs(c(trackAzimuth(xy), 0) - c(0,
                                           rev(trackAzimuth(xy[nrow(xy):1L, ]))))
 
  angles <- ifelse(angles > 180, 360 - angles,
                   angles)
  angles[is.na(angles)] <- 180
  angles[c(1L, length(angles))] <- rep(as.numeric(NA), 2L)
  angles
}

###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
