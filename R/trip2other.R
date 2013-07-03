# $Id: trip2other.R 109 2013-03-28 03:09:51Z sluque $

## ltraj from adehabitat

as.ltraj.trip <- function(xy, typeII=TRUE, slsp="remove") {
    require(adehabitatLT) ||
        stop("adehabitatLT package is required, but unavailable")
    tor <- getTORnames(xy)
    crds <- coordinates(xy)
    adehabitatLT::as.ltraj(as.data.frame(crds), date=xy[[tor[1]]],
                           id=xy[[tor[2]]], typeII=typeII, slsp=slsp)
}

setAs("trip", "ltraj", function(from) as.ltraj.trip(from))

## cases

##  lines, dTime, pixellate - tripGrid
##  lines, sigma, density - tripGrid
##  ??lines, weights, pixellate - as.psp.trip (default is dTime)
##  points, weights, pixellate - as.ppp.trip
##  points, sigma, density - as.ppp.trip

## do we want IDs or times? (let the user do it?)
as.ppp.trip <- function(X, ..., fatal) {
    as.ppp.SpatialPointsDataFrame(X)
}

setAs("trip", "ppp", function(from) as.ppp.trip(from))

as.psp.trip <- function(x, ..., from, to) {
    split.X <- split(x, x[[getTORnames(x)[2]]])
    ow <- spatstat::owin(bbox(x)[1,], bbox(x)[2,])
    as.psp.trip1 <- function(this, ow=NULL) {
        if (is.null(ow)) ow <- owin(bbox(this)[1,], bbox(this)[2,])
        tor <- getTORnames(this)
        cc <- coordinates(this)
        xs <- coordinates(this)[, 1]
        ys <- coordinates(this)[, 2]
        dt <- diff(unclass(this[[tor[1]]]))
        psp(xs[-length(xs)], ys[-length(ys)],
            xs[-1], ys[-1], window=ow, marks=dt)
    }
    do.call("superimpose", lapply(split.X, as.psp.trip1, ow=ow))
}

setAs("trip", "psp", function(from) as.psp.trip(from))

as.trip.SpatialLinesDataFrame <- function(from) {
    .Deprecated("as.SpatialLinesDataFrame.trip")
    as.SpatialLinesDataFrame.trip(from)
}
as.SpatialLinesDataFrame.trip <- function(from) {
    split.from <- split(from, from[[getTORnames(from)[2]]])
    sdf <- suppressWarnings(summary(from))
    df <- data.frame(tripID=sdf$tripID, tripStart=sdf$tmins,
                     tripEnd=sdf$tmaxs,
                     tripDur=as.vector(sdf$tripDurationSeconds),
                     row.names=sdf$tripID)
    lns <- vector("list", nrow(df))
    for (i in 1:length(lns)) {
        lns[[i]] <- Lines(list(Line(coordinates(split.from[[i]]))),
                          ID=sdf$tripID[i])
    }
    SpatialLinesDataFrame(SpatialLines(lns,
                                       proj4string=CRS(proj4string(from))),
                          df)
}

setAs("trip", "SpatialLinesDataFrame", as.SpatialLinesDataFrame.trip)



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
