## TODO:
## tidier!
##-----------------------------------------------------------------------------
## there is a bug here if times are integer and constant (or something)
## I think it has to do with boundary.lev creation, as subsequent trips are out of whack

## this fails (but ok if tms is + 1:10)
## d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + c(1:5, 1:5), id=gl(2, 5))
## coordinates(d) <- ~x+y
## tr <- trip(d, c("tms", "id"))

## bound.dates <- seq(min(tr$tms)-1, max(tr$tms)+1, length=5)
## trip.list <- trip.split.exact(tr, bound.dates)

##' @importFrom maptools spRbind
##' @rdname trip-internal
.tripRbind <- function (obj, x) {
    ## not needed, and not possible since classes imported using
    ## NAMESPACE MDS 2012-10-09
    ## suppressMessages(require(maptools))
    tor1 <- getTORnames(obj)
    tor2 <- getTORnames(x)
    if (! identical(tor1, tor2)) stop("trips are not equivalent for rbind")
    SP <- spRbind(as(obj, "SpatialPoints"), as(x, "SpatialPoints"))
    df <- rbind(slot(obj, "data"), slot(x, "data"))
    dupes <- duplicated(cbind(coordinates(SP), df))
    x <- SpatialPointsDataFrame(SP, data=df)[!dupes, ]
    trip(x, tor1)
}

##' @rdname trip-internal
.single.trip.split <- function(tr1, boundary.dates) {
    diff.d <- diff(unclass(boundary.dates))
    if (any(diff.d < 0))
        stop("boundary dates must must sort increasingly")
    if (any(!diff.d > 0))
        stop("duplicates in boundary dates")
    tor <- getTORnames(tr1)
    ## single id trip object
    x <- tr1[, tor]
    x <- data.frame(coordinates(x), x@data[,tor])
    if (min(boundary.dates) > min(x[, 3]))
        stop("boundary dates do not encompass trip range (MIN)")
    if (max(boundary.dates) < max(x[, 3]))
        stop("boundary dates do not encompass trip range (MAX)")
    which.dates <- boundary.dates[boundary.dates > min(x[, 3]) &
                                  boundary.dates < max(x[, 3])]
    which.dates <- rep(which.dates, each=2)
    if (!length(which.dates) > 0) {
        ## we are done
        tr1$boundary.lev <- 1
        res <- list(tr1)
        ind <- which.min(boundary.dates < min(x[, 3]) )
        boundary.names <- paste(boundary.dates[c(ind - 1, ind)],
                                collapse=" <-> ")
        names(res) <- boundary.names
        return(res)
    }
    boundary.ids <- which(boundary.dates > min(x[, 3]) &
                          boundary.dates < max(x[, 3]))
    boundary.ids <- c(boundary.ids[1] - 1, boundary.ids,
                      boundary.ids[length(boundary.ids)] + 1)
    boundary.names <- paste(boundary.dates[boundary.ids[-length(boundary.ids)]],
                            boundary.dates[boundary.ids[-1]], sep=" <-> ")
    fx <- approxfun(x[, 3], x[, 1])
    fy <- approxfun(x[, 3], x[, 2])
    new.x <- fx(which.dates)
    new.y <- fy(which.dates)
    new.1 <- data.frame(new.x, new.y, which.dates,
                        rep(x[1, 4], length(which.dates)))
    names(new.1) <- names(x)
    x.new <- rbind(x, new.1)
    ## sort records
    x.new <- x.new[order(x.new[, 3]), ]
    edges <- which(x.new[, 3] %in% which.dates)
    ## boundary.lev
    boundary.lev <- cumsum(x.new[, 3] %in% which.dates)
    boundary.lev[boundary.lev %% 2 > 0] <-
        boundary.lev[boundary.lev %% 2 > 0] - 1
    x.new$boundary.lev <- unclass(factor(boundary.lev))
    t.list <- split(x.new, x.new$boundary.lev)
    if (!length(t.list) == length(boundary.names))
        stop("names and split do not match")
    names(t.list) <- boundary.names
    ## deal with trips that are too short
    for (i in 1:length(t.list)) {
        if (nrow(t.list[[i]]) < 2)
            stop("this should never happen")
        if (nrow(t.list[[i]]) < 3) {
            x <- t.list[[i]]
            fx <- approxfun(x[, 3], x[, 1])
            fy <- approxfun(x[, 3], x[, 2])
            which.dates <- seq(min(x[, 3]), max(x[, 3]), length=3)
            x1 <- data.frame(fx(which.dates), fy(which.dates),
                             which.dates, rep(x[1, 4], 3), rep(x[1, 5], 3))
            names(x1) <- names(x)
            t.list[[i]] <- x1
        }
    }
    res <- lapply(t.list, function(x) {
        SpatialPointsDataFrame(as.matrix(x[, 1:2]),
                               x[, -c(1, 2)],
                               proj4string=CRS(proj4string(tr1)))
    })
                                        #browser()
    lapply(res, trip, tor)
}




#' 
#' Split trip events into exact time-based boundaries.
#' 
#' 
#' Split trip events within a single object into exact time boundaries, adding
#' interpolated coordinats as required.
#' 
#' 
#' Motion between boundaries is assumed linear and extra coordinates are added
#' at the cut points.
#' 
#' @param x A trip object.
#' @param dates A vector of date-time boundaries. These must encompass all the
#' time range of the entire trip object.
#' @param \dots Unused arguments.
#' @return
#' 
#' A list of trip objects, named by the time boundary in which they lie.
#' @author Michael D. Sumner and Sebastian Luque
#' @seealso See also \code{\link{tripGrid}}.
#' @keywords manip chron
#' @examples
#' 
#' \dontrun{
#' set.seed(66)
#' d <- data.frame(x=1:100, y=rnorm(100, 1, 10),
#'                 tms=Sys.time() + c(seq(10, 1000, length=50),
#'                 seq(100, 1500, length=50)), id=gl(2, 50))
#' coordinates(d) <- ~x+y
#' tr <- trip(d, c("tms", "id"))
#' 
#' bound.dates <- seq(min(tr$tms) - 1, max(tr$tms) + 1, length=5)
#' trip.list <- cut(tr, bound.dates)
#' bb <- bbox(tr)
#' cn <- c(20, 8)
#' g <- GridTopology(bb[, 1], apply(bb, 1, diff) / (cn - 1), cn)
#' 
#' tg <- tripGrid(tr, grid=g)
#' tg <- as.image.SpatialGridDataFrame(tg)
#' tg$x <- tg$x - diff(tg$x[1:2]) / 2
#' tg$y <- tg$y - diff(tg$y[1:2]) / 2
#' 
#' op <- par(mfcol=c(4, 1))
#' for (i in 1:length(trip.list)) {
#'   plot(coordinates(tr), pch=16, cex=0.7)
#'   title(names(trip.list)[i], cex.main=0.9)
#'   lines(trip.list[[i]])
#'   abline(h=tg$y, v=tg$x, col="grey")
#'   image(tripGrid(trip.list[[i]], grid=g), interpolate=FALSE,
#'   col=c("white", grey(seq(0.2, 0.7,  length=256))),add=TRUE)
#'   abline(h=tg$y, v=tg$x,  col="grey")
#'   lines(trip.list[[i]])
#'   points(trip.list[[i]], pch=16, cex=0.7)
#' }
#' 
#' par(op)
#' print("you may need to resize the window to see the grid data")
#' 
#' cn <- c(200, 80)
#' g <- GridTopology(bb[, 1], apply(bb, 1, diff) / (cn - 1), cn)
#' 
#' tg <- tripGrid(tr, grid=g)
#' tg <- as.image.SpatialGridDataFrame(tg)
#' tg$x <- tg$x - diff(tg$x[1:2]) / 2
#' tg$y <- tg$y - diff(tg$y[1:2]) / 2
#' 
#' op <- par(mfcol=c(4, 1))
#' for (i in 1:length(trip.list)) {
#'   plot(coordinates(tr), pch=16, cex=0.7)
#'   title(names(trip.list)[i], cex.main=0.9)
#'   image(tripGrid(trip.list[[i]], grid=g, method="density", sigma=1),
#'         interpolate=FALSE,
#'         col=c("white", grey(seq(0.2, 0.7, length=256))),
#'         add=TRUE)
#'   lines(trip.list[[i]])
#'   points(trip.list[[i]], pch=16, cex=0.7)
#' }
#' 
#' par(op)
#' print("you may need to resize the window to see the grid data")
#' 
#' }
#' 
#' @method cut trip
#' @S3method cut trip
#' @export cut.trip
cut.trip <- function(x, dates, ...) {
    tor <- getTORnames(x)
    ids <- unique(x[[tor[2]]])
    all.list <- vector("list", length(ids))
    names(all.list) <- ids
    for (id in ids) {
        x1 <- x[x[[tor[2]]] == id, ]
        all.list[[id]] <- trip:::.single.trip.split(x1, dates)
    }
    all.names <- unique(unlist(lapply(all.list, names)))
    ord <- order(as.POSIXct(all.names))
    all.names <- all.names[ord]
    res.list <- vector("list", length(all.names))
    names(res.list) <- all.names
    for (i in 1:length(all.names)) {
        this.name <- all.names[i]
        this.res <- list()
        for (j in 1:length(all.list)) {
            matches <- match(this.name,  names(all.list[[j]]))
            if (!is.na(matches)) {
                this.res <- c(this.res, all.list[[j]][[this.name]])
            }
        }
        res.list[[this.name]] <- this.res
    }
    nlist <- vector("list", length(res.list))
    names(nlist) <- names(res.list)
    for (i in 1:length(res.list)) {
        nlist[[i]] <- res.list[[i]][[1]]
        if (length(res.list[[i]]) > 1) {
            for (j in 2:length(res.list[[i]])) {
                nlist[[i]] <- trip:::.tripRbind(nlist[[i]],
                                                res.list[[i]][[j]])
            }
        }
    }
    nlist
}


