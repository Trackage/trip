# $Id: trip.split.R 89 2013-03-24 16:34:38Z sluque $

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

trip.split.exact <- function(x, dates) {
    .Deprecated("cut.trip")
    cut(x, dates)
}
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



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
