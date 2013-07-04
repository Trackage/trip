# $Id: filter.penSS.R 68 2013-03-20 03:11:06Z sluque $



#' Non-destructive smoothing filter
#' 
#' 
#' Non-destructive filter for track data using penalty smoothing on velocity.
#' 
#' 
#' Destructive filters such as \code{\link{speedfilter}} can be recast using a
#' penalty smoothing approach in the style of Green and Silverman (1994).
#' 
#' This filter works by penalizing the fit of the smoothed track to the
#' observed locations by the sum of squared velocities.  That is, we trade off
#' goodness of fit against increasing the total sum of squared velocities.
#' 
#' When lambda=0 the smoothed track reproduces the raw track exactly.
#' Increasing lambda favours tracks requiring less extreme velocities, at the
#' expense of reproducing the original locations.
#' @name filter.penSS
#' @param tr A \code{trip} object.
#' @param lambda Smoothing parameter, see Details.
#' @param first Fix the first location and prevent it from being updated by the
#' filter.
#' @param last Fix the last location and prevent it from being updated by the
#' filter.
#' @param \code{\dots}
#' @return
#' 
#' A trip object with updated coordinate values based on the filter - all the
#' data, including original coordinates which are maintained in the trip data
#' frame.
#' @author Simon Wotherspoon and Michael Sumner
#' @seealso \code{\link{speedfilter}}
#' @references
#' 
#' Green, P. J. and Silverman, B. W. (1994). Nonparametric regression and
#' generalized linear models: a roughness penalty approach. CRC Press.
#' @keywords manip misc
#' @examples
#' 
#' 
#' \dontrun{## Example takes a few minutes
#' 
#' ## Fake some data
#' 
#' ## Brownian motion tethered at each end
#' brownian.bridge <- function(n, r) {
#'   x <- cumsum(rnorm(n, 0, 1))
#'   x <- x - (x[1] + seq(0, 1, length=n) * (x[n] - x[1]))
#'   r * x
#' }
#' 
#' ## Number of days and number of obs
#' days <- 50
#' n <- 200
#' 
#' ## Make separation between obs gamma distributed
#' x <- rgamma(n, 3)
#' x <- cumsum(x)
#' x <- x/x[n]
#' 
#' ## Track is lissajous + brownian bridge
#' b.scale <- 0.6
#' r.scale <- sample(c(0.1, 2, 10.2), n, replace=TRUE,
#'                   prob=c(0.8, 0.18, 0.02))
#' set.seed(44)
#' 
#' tms <- ISOdate(2001, 1, 1) + trunc(days * 24 * 60 * 60 *x)
#' lon <- 120 + 20 * sin(2 * pi * x) +
#'     brownian.bridge(n, b.scale) + rnorm(n, 0, r.scale)
#' lat <- -40 + 10 *(sin(3 * 2 * pi * x) + cos(2 * pi * x) - 1) +
#'     brownian.bridge(n, b.scale) + rnorm(n, 0, r.scale)
#' 
#' tr <- new("trip",
#'           SpatialPointsDataFrame(cbind(lon, lat),
#'                                  data.frame(gmt=tms, id="lbb")),
#'                                  TimeOrderedRecords(c("gmt", "id")))
#' plot(tr)
#' 
#' ## the filtered version
#' trf <- filter.penSS(tr, lambda=1, iterlim=400, print.level=1)
#' 
#' lines(trf)
#' 
#' }
#' 
#' 
#' @export filter.penSS
filter.penSS <- function(tr, lambda, first=TRUE, last=TRUE,...) {

    penalized <- function(x) {
        ## Form smoothed track
        p <- p.obs
        p[sub, ] <- x
        ## Velocities between smoothed points
        ##v <- gc.dist(p[-n,],p[-1,])/dt
        v <- trackDistance(p[, 2:1]) / dt
        ## Distances from smoothed points to observations
        ##d <- gc.dist(p,p.obs)
        d <- trackDistance(p[, 2], p[, 1], p.obs[, 2], p.obs[, 1])
        ## This is the penalized sum of squares
        (sum(d ^ 2) + lambda * sum(v ^ 2)) / n ^ 2
    }

    if (length(summary(tr)$tripID) > 1) {
        msg <- paste("trip object contains multiple events,",
                     "only the first trip used")
        warning(msg)
        tr <- tr[tr[[getTORnames(tr)[2]]] == summary(tr)$tripID[1], ]
    }
    ## Number of points and subset
    n <- nrow(tr)
    sub <- (1 + first):(n - last)
    ## Observed points
    ##  p.obs <- as.matrix(tr[,c("Lat","Lon")])
    p.obs <- coordinates(tr)[, 2:1]
    ## Time intervals (in days) between obs
    ##dt <- diff(unclass(tr$Time)/(24*60*60))
    dt <- diff(unclass(tr[[getTORnames(tr)[1]]]) / (24 * 60 * 60))
    mn <- nlm(penalized, as.matrix(p.obs[sub, ]), ...)
    m <- n - (first + last)
    res <- coordinates(tr)
    ##  tr$Lat[sub] <- mn$estimate[1:m]
    ##  tr$Lon[sub] <- mn$estimate[m+1:m]
    res[sub, 2] <- mn$estimate[1:m]
    res[sub, 1] <- mn$estimate[m + 1:m]
    res <- SpatialPointsDataFrame(res, as.data.frame(tr),
                                  proj4string=CRS(proj4string(tr)))
    trip(res, getTORnames(tr))
}



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
