
##' Filter track data for speed, distance and angle.
##'
##' Create a filter index of a track for "bad" points with a
##' combination of speed, distance and angle tests.
##' @title sda
##' @param x trip object
##' @param smax maximum speed, in km/h
##' @param ang minimum turning angle/s in degrees
##' @param distlim maximum step lengths in km
##' @references Freitas, C., Lydersen, C., Fedak, M. A. and Kovacs,
##' K. M. (2008), A simple new algorithm to filter marine mammal Argos
##' locations. Marine Mammal Science, 24: 315¡V325. doi:
##' 10.1111/j.1748-7692.2007.00180.x
##' @details This is an independent  implementation from that in the
##' package argosfilter by Frietas 2008.
##' @return logical vector, with \code{FALSE} values where the tests failed
##' @export
sda <- function(x, smax, ang = c(15, 25), distlim = c(25, 50)) {
    xlist <- split(x, x[[getTORnames(x)[2L]]])
    bigok <- vector("list", length(xlist))
    for (i in seq_along(xlist)) {
        ok <- sda0(xlist[[i]], smax, ang, distlim)
        bigok[[i]] <- ok
    }
    unlist(bigok)
}
sda0 <- function(x, smax, ang, distlim) {
    x$speed.ok <- speedfilter(x, max.speed = smax)

    dsts <- trackDistance(coordinates(x), longlat = TRUE)
    angs <- trackAngle(coordinates(x))


    dprev <- c(0, dsts)
    dnext <- c(dsts, 0)

    ## No Argos quality fiter, anyone can do that
    ok <- (x$speed.ok | dprev <= distlim[2]) ###&  (x$lc > -9)


    x$filt.row <- 1:nrow(x)

    x$ok <- rep(FALSE, nrow(x))
    df <- x


    ## first subset

    df <- df[ok, ]

    ## distlim and angles, progressively

    for (i in 1:length(distlim)) {
        dsts <- trackDistance(coordinates(df))
        angs <- trackAngle(coordinates(df))
        dprev <- c(0, dsts)
        dnext <- c(dsts, 0)
        ok <- (dprev <= distlim[i] | dnext <= distlim[i])  | angs > ang[i]
        ok[c(1:2, (length(ok)-1):length(ok))] <- TRUE
        df <- df[ok, ]
        ok <- rep(TRUE, nrow(df))
    }

    x$ok[ match(df$filt.row, x$filt.row)] <- ok

    x$ok
}
