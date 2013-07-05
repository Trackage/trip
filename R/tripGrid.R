
## TODO:
## a version of tripGrid that takes Lines, so
## as.SpatialLinesDataFrame.trip, and then to grid

## allow sigmas argument for density version, for each line segment

## replaces tripGrid, old version is now called tripGrid.interp



#' Generate a grid of time spent by line-to-cell gridding
#' 
#' 
#' Create a grid of time spent from an object of class \code{trip} by exact
#' cell crossing methods, weighted by the time between locations for separate
#' trip events.
#' 
#' 
#' Zero-length lines cannot be summed directly, their time value is summed by
#' assuming the line is a point. A warning is given. The density method returns
#' proportionate values, not summed time durations.
#' 
#' See \code{pixellate.psp} and \code{pixellate.ppp} for the details on the
#' method used. See \code{density.psp} for method="density".
#' 
#' Trip events are assumed to start and end as per the object passed in. To
#' work with inferred "cutoff" positions see \code{split.trip.exact}.
#' 
#' @param x object of class \code{trip}
#' @param grid GridTopology - will be generated automatically if NULL
#' @param method pixellate or density
#' @param \dots pass arguments to density.psp if that method is chosen (and
#' temporary mechanism to direct users of legacy methods to
#' \code{\link{tripGrid.interp}})
#' @return
#' 
#' \code{tripGrid} returns an object of class \code{SpatialGridDataFrame}, with
#' one column "z" containing the time spent in each cell in seconds.
#' @keywords manip
#' @export tripGrid
tripGrid <- function (x, grid=NULL, method="pixellate", ...)
{
    if (method %in% c("kde", "count")) {
        msg <- paste("kde and count methods no longer supported",
                     "from trip_1.1-6 and will be ignored,",
                     "see ?tripGrid.interp for legacy function")
        warning(msg)
    }
    if (!is.null(list(...)$dur)) {
        msg <- paste("dur(ation) not necessary for this function from",
                     "trip_1.1-6 and will be ignored - time sum is now",
                     "exact\n see ?tripGrid.interp for legacy function")
        stop(msg)
    }
    if (is.null(grid))
        grid <- makeGridTopology(x)
    spgdf <- SpatialGridDataFrame(grid,
                                  data.frame(z=rep(0, prod(grid@cells.dim))))
    res <- as.image.SpatialGridDataFrame(spgdf)
    tor <- x@TOR.columns
    trip.list <- split.data.frame(x[, tor], x[[tor[2]]])
    ow <- .g2ow(grid)
    sm <- 0
    zero.lengths <- FALSE
    sz <- 0
    for (this in trip.list) {
        xs <- coordinates(this)[, 1]
        ys <- coordinates(this)[, 2]
        dt <- diff(unclass(this[[tor[1]]]))
        sm <- sm + sum(dt)
        x.psp <- spatstat::psp(xs[-length(xs)], ys[-length(ys)], xs[-1],
                               ys[-1], window=ow)
        lngths <- spatstat::lengths.psp(x.psp)
        if (any(!lngths > 0)) {
            ## trim psp objects (0-lines give NaNs)
            zero.lengths <- TRUE
            zeros <- which(!lngths > 0)
            cc <- coordinates(this)[zeros, , drop=FALSE]
            op <- options(warn=-1)
            x.ppp <- spatstat::ppp(cc[, 1], cc[, 2], window=ow)
            options(op)
            if (method == "pixellate") {
                v <- spatstat::pixellate(x.ppp, W=ow, weights=dt[zeros])$v
            }
            if (method == "density") {
                v <- density(x.ppp, ...)$v
            }
            res$z <- res$z + t(v)
            sz <- sz + sum(dt[zeros])
        }
        x.psp <- x.psp[lngths > 0]
        weights <- dt/ifelse(lngths > 0, lngths, .Machine$double.eps)
        weights <- weights[lngths > 0]
        if (method == "pixellate") {
            v <- spatstat::pixellate(x.psp, W=ow, weights=weights)$v
        }
        if (method == "density") {
            ## v <- density(x.psp, ...)$v
            for (li in 1:x.psp$n) {
                dens <- density(x.psp[li], ...)$v
                if (li == 1) {
                    v <- dens
                } else {
                    v <- v + dens * dt[li]
                }
            }
        }
        res$z <- res$z + t(v)
    }
    if (zero.lengths) {
        msg <- paste("zero length lines present, time durations binned",
                     "into zero length lines present, time durations",
                     "binned into cells assuming point-presence of",
                     "degenerate line segment")
        warning(msg)
        cat("\n")
        if (method == "pixellate") {
            cat(paste("Total time of trips:", sm, "\n"))
            cat(paste("Total time without zero length lines:",
                      sm - sz, "\n"))
        }
    }
    image2Grid(res, p4=proj4string(x))
}

##' @rdname trip-internal
.g2ow <- function(x) {
  mn <- x@cellcentre.offset - x@cellsize / 2
  mx <- mn + x@cells.dim * x@cellsize
  owin(c(mn[1], mx[1]), c(mn[2], mx[2]),
       mask=matrix(TRUE, x@cells.dim[2], x@cells.dim[1]),
       xy=list(x=seq(mn[1], mx[1], length=x@cells.dim[1]),
               y=seq(mn[2], mx[2], length=x@cells.dim[2])))
}


