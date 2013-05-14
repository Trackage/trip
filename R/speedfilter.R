# $Id: speedfilter.R 99 2013-03-27 14:13:27Z sluque $

speedfilter <- function (x, max.speed=NULL, test=FALSE) {
    if (!is(x, "trip"))
        stop("only trip objects supported")
    projected <- is.projected(x)
    if (is.na(projected)) {
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
        if (npts < (pprm + 1)) {
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



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
