## S Wotherspoon, ca. 2002 pre-tripEstimation

## Brownian motion tethered at each end
#' @importFrom stats rnorm rgamma
brownian_bridge <- function(n, r) {
  x <- cumsum(stats::rnorm(n, 0, 1))
  x <- x - (x[1] + seq(0, 1, length=n) * (x[n] - x[1]))
  r * x
}

simulate_tracks <- function(n = 1, npoints = 200, 
                            begin = ISOdatetime(2018, 1, 1, 0, 0, 0, tz = "UTC"), 
                            end = ISOdatetime(2018, 12, 31, 23, 59, 59, tz = "UTC"), 
                            ids = NULL,
                            returntrip = TRUE) {
    npoints <- rep(npoints, n)
    begin <- rep(begin, n)
    end <- rep(end, n)
    if (is.null(ids)) ids <- as.character(seq_len(n) )
    l <- vector("list", n)
    for (i in seq_len(n)) {
      ## Make separation between obs gamma distributed
      x <- rgamma(npoints[i], 3)
      x <- cumsum(x)
      x <- x/x[npoints[i]]
      
      ## Track is lissajous + brownian bridge
      b.scale <- 0.6
      r.scale <- sample(c(0.1, 2, 10.2), npoints, replace=TRUE,
                        prob=c(0.8, 0.18, 0.02))
      ## TODO add some variation to the time sequence
      tms <- seq(begin[i], end[i], length = npoints[i])
      #tms <- ISOdate(2001, 1, 1) + trunc(days * 24 * 60 * 60 *x)
      lon <- 120 + 20 * sin(2 * pi * x) +
        brownian_bridge(n, b.scale) + rnorm(n, 0, r.scale)
      lat <- -40 + 10 *(sin(3 * 2 * pi * x) + cos(2 * pi * x) - 1) +
        brownian_bridge(n, b.scale) + rnorm(n, 0, r.scale)
      l[[i]] <- data.frame(lon = lon, lat = lat, utc = tms, id = ids[i], stringsAsFactors = FALSE)
    }
    d <- do.call(rbind, l)
    if (returntrip) {
      coordinates(d) <- c("lon", "lat")
      proj4string(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs", doCheckCRSArgs = FALSE)
      d <- trip(d, c("utc", "id"))
    }
    d
  }