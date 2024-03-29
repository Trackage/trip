#' Filter track data for speed
#'
#' Create a filter of a track for "bad" points implying a speed of motion that
#' is unrealistic.
#'
#' Using an algorithm (McConnnell et al., 1992), points are tested for speed
#' between previous / next and 2nd previous / next points.  Contiguous sections
#' with an root mean square speed above a given maximum have their highest rms
#' point removed, then rms is recalculated, until all points are below the
#' maximum.  By default an (internal) root mean square function is used, this
#' can be specified by the user.
#'
#' If the coordinates of the \code{trip} data are not projected, or NA the
#' distance calculation assumes longlat and kilometres (great circle). For
#' projected coordinates the speed must match the units of the coordinate
#' system.  (The PROJ.4 argument "units=km" is suggested).
#'
#' @param x trip object
#' @param max.speed speed in kilometres (or other unit) per hour, the unit is kilometres
#' if the trip is in longitude latitude coordinates, or in the unit of the
#' projection projection (usually metres per hour)
#' @param test cut the algorithm short and just return first pass
#' @return
#'
#' Logical vector matching positions in the coordinate records that pass the
#' filter.
#' @note
#'
#' This algorithm was originally taken from IDL code by David Watts at the
#' Australian Antarctic Division, and used in various other environments before
#' the development of this version.
#' @section Warning:
#'
#' This algorithm is destructive, and provides little information about
#' location uncertainty.  It is provided because it's commonly used
#' and provides an illustrative benchmark for further work.
#'
#' It is possible for the filter to become stuck in an infinite loop, depending
#' on the function passed to the filter.  Several minutes is probably too long
#' for hundreds of points, test on smaller sections if unsure.
#' @author David Watts and Michael D. Sumner
#' @seealso \code{\link{sda}} for a fast distance angle filter to combine with speed filtering
#' @references
#'
#' The algorithm comes from McConnell, B. J. and Chambers, C. and Fedak, M. A.
#' (1992) Foraging ecology of southern elephant seals in relation to the
#' bathymetry and productivity of the southern ocean.  Antarctic Science
#' \emph{4} 393-398
#' @keywords manip
#' @export speedfilter
speedfilter <- function (x, max.speed=NULL, test=FALSE) {
    if (!is(x, "trip"))
        stop("only trip objects supported")
    projected <- sp_is_projected(x)
    if (is.na(projected) ) {
        projected <- FALSE
        warning("coordinate system is NA, assuming longlat . . .")
    }
    if (is.null(max.speed)) {
        print("no max.speed given, nothing to do here")
        return(x)
    }
    longlat <- !projected
    coords <- coordinates(x)
    tids <- getTimeID(x)
    time <- tids[, 1]
    id <- factor(tids[, 2])
    x <- coords[, 1]
    y <- coords[, 2]
    pprm <- 3
    grps <- levels(id)
    if (length(x) != length(y))
        stop("x and y vectors must be of same\nlength")
    if (length(x) != length(time))
        stop("Length of times not equal to number of points")
    okFULL <- rep(TRUE, nrow(coords))
    if (test)
        res <- list(speed=numeric(0), rms=numeric(0))
    for (sub in grps) {
        ind <- id == sub
        xy <- matrix(c(x[ind], y[ind]), ncol=2)
        tms <- time[ind]
        npts <- nrow(xy)
        if (pprm%%2 == 0 || pprm < 3) {
            msg <- paste("Points per running mean should be odd and",
                         "greater than 3, pprm=3")
            stop(msg)
        }
        RMS <- rep(max.speed + 1, npts)
        offset <- pprm - 1
        ok <- rep(TRUE, npts)
        if (npts < (pprm + 1) && !test) {
            warning("Not enough points to filter ID: \"", sub,
                    "\"\n continuing . . . \n")
            okFULL[ind] <- ok
            next
        }
        index <- 1:npts
        iter <- 1
        while (any(RMS > max.speed, na.rm=TRUE)) {
            n <- length(which(ok))
            x1 <- xy[ok, ]
            speed1 <- trackDistance(x1[-nrow(x1), 1], x1[-nrow(x1), 2],
                                    x1[-1, 1], x1[-1, 2],
                                    longlat=!projected) /
                                        (diff(unclass(tms[ok])) / 3600)
            speed2 <- trackDistance(x1[-((nrow(x1) - 1):nrow(x1)), 1],
                                    x1[-((nrow(x1) - 1):nrow(x1)), 2],
                                    x1[-(1:2), 1], x1[-(1:2), 2],
                                    longlat=!projected) /
                                        ((unclass(tms[ok][-c(1, 2)]) -
                                          unclass(tms[ok][-c(n - 1, n)])) /
                                         3600)
            thisIndex <- index[ok]
            npts <- length(speed1)
            if (npts < pprm)
                next
            sub1 <- rep(1:2, npts - offset) + rep(1:(npts - offset), each=2)
            sub2 <- rep(c(0, 2), npts - offset) +
                rep(1:(npts - offset), each=2)
            rmsRows <- cbind(matrix(speed1[sub1], ncol=offset, byrow=TRUE),
                             matrix(speed2[sub2], ncol=offset, byrow=TRUE))
            RMS <- c(rep(0, offset),
                     sqrt(rowSums(rmsRows ^ 2) / ncol(rmsRows)))
            if (test & iter == 1) {
                res$speed <- c(res$speed, 0, speed1)
                res$rms <- c(res$rms, 0, RMS)
                break
            }
            RMS[length(RMS)] <- 0
            bad <- RMS > max.speed
            segs <- cumsum(c(0, abs(diff(bad))))
            ## try wrapping ifelse here? no index is quicker
            rmsFlag <- unlist(lapply(split(RMS, segs), function(x) {
                ifelse((1:length(x)) == which.max(x), TRUE, FALSE)
            }), use.names=FALSE)
            rmsFlag[!bad] <- FALSE
            RMS[rmsFlag] <- -10
            ok[thisIndex][rmsFlag > 0] <- FALSE
        }
        okFULL[ind] <- ok
    }
    if (test)
        return(res)
    okFULL
}



##' Filter track for speed, distance and angle.
##'
##' Create a filter index of a track for "bad" points with a
##' combination of speed, distance and angle tests.
##' @name sda
##' @param x trip object
##' @param smax maximum speed, in km/h
##' @param ang minimum turning angle/s in degrees
##' @param distlim maximum step lengths in km
##' @param pre include this filter in the removal
##' @references Freitas, C., Lydersen, C., Fedak, M. A. and Kovacs,
##' K. M. (2008), A simple new algorithm to filter marine mammal Argos
##' locations. Marine Mammal Science, 24: 315?V325. doi:
##' 10.1111/j.1748-7692.2007.00180.x
##' @details This is an independent  implementation from that in the
##' package argosfilter by Freitas 2008.
##' @return logical vector, with \code{FALSE} values where the tests failed
##' @export
sda <- function(x, smax, ang = c(15, 25), distlim = c(2.5, 5.0), pre = NULL) {
    if (!is.null(pre)) x$prefilter <- pre
    xlist <- split(x, x[[getTORnames(x)[2L]]])
    bigok <- vector("list", length(xlist))
    for (i in seq_along(xlist)) {
        ok <- sda0(xlist[[i]], smax, ang, distlim, pre = xlist[[i]]$prefilter)
        bigok[[i]] <- ok
    }
    unlist(bigok)
}

sda0 <- function(x, smax, ang, distlim, pre = NULL) {
    x$speed.ok <- speedfilter(x, max.speed = smax)

    dsts <- trackDistance(x, longlat = TRUE)
    angs <- trackAngle(x)
    ## simple way to deal with missing angles
    ### (which don't make sense for first and last position or zero-movement)
    angs[is.na(angs)] <- 180

    dprev <- dsts
    dnext <- c(dsts[-1L], 0)

    ## No Argos quality filter, anyone can do that
    ok <- (x$speed.ok | dprev <= distlim[2]) ##&  (x$lc > -9)

    if (!is.null(pre)) ok <- ok & pre
    x$filt.row <- 1:nrow(x)

    x$ok <- rep(FALSE, nrow(x))
    df <- x


    ## first subset

    df <- df[ok, ]

    ## distlim and angles, progressively

    for (i in 1:length(distlim)) {
        dsts <- trackDistance(df)
        angs <- trackAngle(df)
        dprev <- dsts
        dnext <- c(dsts[-1L], 0)


        angs[is.na(angs)] <- 180
        ok <- (dprev <= distlim[i] | dnext <= distlim[i])  | angs > ang[i]
        ok[c(1:2, (length(ok)-1):length(ok))] <- TRUE
        df <- df[ok, ]
        ok <- rep(TRUE, nrow(df))
    }

    x$ok[ match(df$filt.row, x$filt.row)] <- ok

    x$ok
}


# $Id: filter.penSS.R 68 2013-03-20 03:11:06Z sluque $



filter_penSS <- function(tr, lambda, first=TRUE, last=TRUE,...) {

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
                                  proj4string=CRS(tr@proj4string@projargs, doCheckCRSArgs = FALSE))
    trip(res, getTORnames(tr))
}



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
