#' As ("trip", other-classes)
#'
#' Coercing \code{trip} objects to other classes.
#'
#' @name as.Other
#' @aliases  as.psp.trip as.track_xyt.trip as.ppp as.psp
# section Methods:
#
# ##\describe{
#
# ##\item{coerce}{\code{signature(from="trip", to="SpatialLinesDataFrame")}}
# ##}
setAs("trip", "SpatialLinesDataFrame", function(from) {
  split.from <- split(from, from[[getTORnames(from)[2]]])
  sdf <- suppressWarnings(summary(from))
  df <- data.frame(tripID=sdf$tripID, tripStart=sdf$tmins,
                   tripEnd=sdf$tmaxs,
                   tripDur=as.vector(sdf$tripDurationSeconds),
                   stringsAsFactors = FALSE)
  lns <- vector("list", nrow(df))
  for (i in 1:length(lns)) {
    lns[[i]] <- Lines(list(Line(coordinates(split.from[[i]]))),
                      ID=sdf$tripID[i])
  }
  SpatialLinesDataFrame(SpatialLines(lns,
                                     proj4string=CRS(from@proj4string@projargs, doCheckCRSArgs=FALSE)),
                        df)
})

#' @importFrom stats setNames
setAs("trip", "sf", function(from) {
  split.from <- split(from, from[[getTORnames(from)[2]]])
  sdf <- suppressWarnings(summary(from))
  df <- data.frame(tripID=sdf$tripID, tripStart=sdf$tmins,
                   tripEnd=sdf$tmaxs,
                   tripDur=as.vector(sdf$tripDurationSeconds),
                   row.names=sdf$tripID)
  lns <- vector("list", nrow(df))
  for (i in 1:length(lns)) {
    ## keep time as numeric
    lns[[i]] <- cbind(coordinates(split.from[[i]]), 
                      as.numeric(split.from[[i]][[getTORnames(from)[1]]]))
  }
  bb <- c(t(apply(do.call(rbind, lns)[, 1:2], 2, range)))

  mk_linestring <- function(x) structure(x, class = c("XYM", "LINESTRING", "sfg"))
  lns <- lapply(lns, mk_linestring)
  mk_sfc <- function(x, bb, crs) structure(x, n_empty = 0, precision = 0, bbox = bb, crs = crs, class = c("sfc_LINESTRING", "sfc"))
  df[["geometry"]] <- mk_sfc(lns, bb, crs = structure(list(epsg = NA_integer_, proj4string = sp::proj4string(from)), class = "crs"))
  class(df) <- c("sf", "data.frame")
  attr(df, "sf_column") <- "geometry"
  attr(df, "agr") <- stats::setNames(rep(NA, ncol(from)), names(from))
  df
})


setAs("trip", "ltraj", function(from) {
  if(!requireNamespace("adehabitatLT")) stop("adhabitatLT not available")
  tor <- getTORnames(from)
  crds <- coordinates(from)
  adehabitatLT::as.ltraj(as.data.frame(crds), date=from[[tor[1]]],
                         id=from[[tor[2]]], typeII=TRUE, slsp="remove")
})



## @importClassesFrom maptools owin ppp psp
#' @importFrom spatstat as.ppp
#' @export as.ppp
#' @importFrom maptools as.ppp.SpatialPointsDataFrame
#' @param X \code{trip} object.
#' @param fatal Logical value, see Details of \code{\link[spatstat]{as.ppp}}
#' @return ppp object
#' @rdname as.Other
#' @method as.ppp trip
#' @examples
#' \dontrun{
#'  d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
#' sp::coordinates(d) <- ~x+y
#' ## this avoids complaints later, but these are not real track data (!)
#' sp::proj4string(d) <- sp::CRS("+proj=laea +ellps=sphere",  doCheckCRSArgs = FALSE)
#' tr <- trip(d, c("tms", "id"))
#' 
#'  as(tr, "ppp")
#' }
as.ppp.trip <- function(X, ..., fatal) {
  as.ppp.SpatialPointsDataFrame(X)
}
setAs("trip", "ppp", function(from) as.ppp.trip(from))

#' @export
#' @importFrom spatstat as.psp owin psp superimpose
#' @export as.psp
#' @param x \code{trip} object
#' @param from see \code{\link[spatstat]{as.psp}} for that method.
#' @param to See \code{\link[spatstat]{as.psp}}.
#' @return psp object
#' @rdname as.Other
#' @method as.psp trip
#' @examples
#' \dontrun{
#'  d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
#' sp::coordinates(d) <- ~x+y
#' ## this avoids complaints later, but these are not real track data (!)
#' sp::proj4string(d) <- sp::CRS("+proj=laea +ellps=sphere", doCheckCRSArgs = FALSE)
#' tr <- trip(d, c("tms", "id"))
#' 
#'  as.psp.trip(tr)
#' }
as.psp.trip <- function(x, ..., from, to) {
  tor <- getTORnames(x)
  split.X <- split(x, x[[tor[2L]]])
  bb <- bbox(x)
  ow <- owin(bb[1L,], bb[2L,])
  as.psp.trip1 <- function(this) {
    #if (is.null(ow)) ow <- owin(bbox(this)[1,], bbox(this)[2,])
    #tor <- getTORnames(this)
    cc <- coordinates(this)
    xs <- cc[, 1L]
    ys <- cc[, 2L]
    dt <- diff(unclass(this[[tor[1L]]]))
    psp(xs[-length(xs)], ys[-length(ys)],
        xs[-1L], ys[-1L], window=ow, marks=dt)
  }
  do.call("superimpose", lapply(split.X, as.psp.trip1))
}
setAs("trip", "psp", function(from) as.psp.trip(from))

#' @export
#' @rdname as.Other
as.track_xyt.trip <- function(x, ..., from, to) {
  tor <- getTORnames(x)
  xy <- sp::coordinates(x)
  tori <- match(tor, names(x@data))
  xd <- x@data[ , -tori, drop = FALSE]
  structure(cbind(data.frame(x_ = xy[,1], y_ = xy[,2], t_ = x[[tor[1]]], id = x[[tor[2]]]), xd), 
            class = c("track_xyt",  "track_xy",   "tbl_df",     "tbl",        "data.frame"), 
            crs = x@proj4string)
  
}
setAs("trip", "track_xyt", function(from) as.track_xyt.trip(from))



#' Break a trip into its component line segments
#'
#' Function to create a SpatialLinesDataFrame from a trip object, resulting in
#' a line segment for each implicit segment along the tracks. The object stores
#' the start and end times, duration and the ID of the segment.
#'
#' @param ... reserved for future methods
#' @return SpatialLinesDataFrame
#' @examples
#'  d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
#' sp::coordinates(d) <- ~x+y
#' ## this avoids complaints later, but these are not real track data (!)
#' sp::proj4string(d) <- sp::CRS("+proj=laea +ellps=sphere", doCheckCRSArgs = FALSE)
#' tr <- trip(d, c("tms", "id"))
#' 
#' spldf <- explode(tr)
#' summary(tr)
#' @return SpatialLinesDataFrame object with each individual line segment identified by start/end time and trip ID
#' @rdname as.Other
#' @export explode
explode <- function(x, ...) {
  tor <- getTORnames(x)
  id <- x[[tor[2]]]
  xs <- split(x, id)
  df <- do.call("rbind",
                lapply(xs, function(x) {
                  n <- nrow(x)
                  tms <- x[[tor[1]]]
                  data.frame(starttime = tms[-n], endtime = tms[-1], timedur = diff(unclass(tms)), id = x[[tor[2]]][-1])
                }
                )
  )
  Linelist <- vector("list", nrow(df))
  cnt <- 0
  for (i in seq_along(xs)) {
    this.x <- xs[[i]]
    this.coords <- coordinates(this.x)
    for (j in seq_len(nrow(this.x)-1)) {
      cnt <- cnt + 1
      Linelist[[cnt]] <- Lines(list(Line(this.coords[j:(j+1), ])), rownames(df)[cnt])
    }

  }
  splines <- SpatialLines(Linelist, proj4string = CRS(x@proj4string@projargs, doCheckCRSArgs = FALSE))
  SpatialLinesDataFrame(splines, df)
}


# setMethod("lines", signature(x="trip"),
#           function(x,
#                    col=hsv(seq(0, 0.9, length = length(unique(x[[getTORnames(x)[2]]]))),
#                            0.8, 0.95),
#                    ...) {
#             x <- .explode(x)
#             col <- heat_hcl(25, h = c(0, -100), l = c(55, 40), c = c(40, 80), power = 3)
#             times <- x$time
#             val <- scl(unclass(times)) * (length(col)-1) + 1
#
#             plot(x,  col=col[val], add=TRUE, ...)
#
#           })
