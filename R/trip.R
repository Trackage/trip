
#' Function to ensure dates and times are in order with trip ID
#'
#'
#' A convenience function, that removes duplicate rows, sorts by the date-times
#' within ID, and removes duplicates from a data frame or
#' SpatialPointsDataFrame.
#'
#'
#' @param x \code{\link{data.frame}} or
#' \code{\link[sp]{SpatialPointsDataFrame}}
#' @param tor character vector of names of date-times and trip ID columns
#' @return \code{\link{data.frame}} or
#' \code{\link[sp]{SpatialPointsDataFrame}}.
#' @note
#'
#' It's really important that data used are of a given quality, but this
#' function makes the most common trip problems easy to apply.
#' @seealso \code{\link{trip}}
#' @export forceCompliance
forceCompliance <- function(x, tor) {
  isSpatial <- is(x, "SpatialPointsDataFrame")
 x <- force_internal(x, tor)
  if (isSpatial) {
    x <- trip(x, tor)
  }
  x
}

force_internal <- function(x, tor) {
  isSpatial <- is(x, "SpatialPointsDataFrame")
  if (isSpatial) {
    prenames <- names(x)
    proj <- sp::proj4string(x)
    x <- as.data.frame(x)
    crdnames <- setdiff(names(x), prenames)
 
  }
  levs <- unique(x[[tor[2]]])
  tooshort <- tapply(x[[1]], x[[tor[2]]], function(x) length(x) < 3)
  if (any(tooshort)) {
    warning(sprintf("removing trip records that have too few elements (<3):  \n'%s'", paste(names(tooshort)[tooshort], collapse = ",")))
  }
  x <- x[x[[tor[2]]] %in% levs[!tooshort], ]
  duperecords <- duplicated(x)
  if (any(duperecords)) {
    warning(sprintf("removing records that are complete duplicates at rows:  \n'%s'", paste(which(duperecords), collapse = ",")))
  }
  x <- x[!duperecords, ]
  triporder <- order(x[[tor[2]]], x[[tor[1]]])
  if (!all(triporder == seq_len(nrow(x)))) {
    warning("ordering input records by trip ID, then time")
    x <- x[triporder, ]
  }
  
  adjusted <- adjust.duplicateTimes(x[[tor[1]]], x[[tor[2]]])

  if (any(abs(unclass(adjusted) - unclass(x[[tor[1]]])) > 0)) {
    warning("updating  duplicated time records by a small adjustment")
    x[[tor[1]]] <- adjusted
  }
  if (isSpatial) {
    coordinates(x) <- crdnames
    sp::proj4string(x) <- sp::CRS(proj)
  }
  x
}
 



#' Adjust duplicate DateTime values
#'
#'
#' Duplicated DateTime values within ID are adjusted forward (recursively) by
#' one second until no duplicates are present. This is considered reasonable
#' way of avoiding the nonsensical problem of duplicate times.
#'
#'
#' This function is used to remove duplicate time records in animal track data,
#' rather than removing the record completely.
#'
#' @param time vector of DateTime values
#' @param id vector of ID values, matching DateTimes that are assumed sorted
#' within ID
#' @return
#'
#' The adjusted DateTime vector is returned.
#' @section Warning:
#'
#' I have no idea what goes on at CLS when they output data that are either not
#' ordered by time or have duplicates. If this problem exists in your data it's
#' probably worth finding out why.
#' @seealso \code{\link{readArgos}}
#' @examples
#'
#'
#' ## DateTimes with a duplicate within ID
#' tms <- Sys.time() + c(1:6, 6, 7:10) *10
#' id <- rep("a", length(tms))
#' range(diff(tms))
#'
#' ## duplicate record is now moved one second forward
#' tms.adj <- adjust.duplicateTimes(tms, id)
#' range(diff(tms.adj))
#'
#'
#' @export adjust.duplicateTimes
adjust.duplicateTimes <- function (time, id) {
    dups <- unlist(tapply(time, id, duplicated), use.names=FALSE)
    if (any(dups)) {
        time[dups] <- time[dups] + 1
        time <- Recall(time, id)
    }
    time
}



#' Assign numeric values for Argos "class"
#'
#'
#' Assign numeric values for Argos "class" by matching the levels available to
#' given numbers. An adjustment is made to allow sigma to be specified in
#' kilometres, and the values returned are the approximate values for longlat
#' degrees.  It is assumed that the levels are part of an "ordered" factor from
#' least precise to most precise.
#'
#'
#' The available levels in Argos are \code{levels=c("Z", "B", "A", "0", "1",
#' "2", "3")}.
#'
#' The actual sigma values given by default are (as far as can be determined) a
#' reasonable stab at what Argos believes.
#'
#' @param x factor of Argos location quality "classes"
#' @param sigma numeric values (by default in kilometres)
#' @param adjust a numeric adjustment to convert from kms to degrees
#' @return
#'
#' Numeric values for given levels.
#' @keywords manip
#' @examples
#'
#'
#' cls <- ordered(sample(c("Z", "B", "A", "0", "1", "2", "3"), 30,
#'                       replace=TRUE),
#'                levels=c("Z", "B", "A", "0", "1", "2", "3"))
#' argos.sigma(cls)
#'
#'
#' @export argos.sigma
argos.sigma <- function(x, sigma=c(100, 80, 50, 20, 10, 4,  2),
                        adjust=111.12) {
    sigma <- sigma 
    names(sigma) <- levels(x)
    sigma[x]/adjust  ## vectorize on x
}





#' Separate a set of IDs based on gaps
#'
#'
#' A new set of ID levels can be created by separating those given based on a
#' minimum gap in another set of data. This is useful for separating
#' instruments identified only by their ID into separate events in time.
#'
#'
#' The assumption is that a week is a long time for a tag not to record
#' anything.
#'
#' @param id existing ID levels
#' @param gapdata data matching \code{id} with gaps to use as separators
#' @param minGap the minimum "gap" to use in gapdata to create a new ID level
#' @return
#'
#' A new set of ID levels, named following the pattern that "ID" split into 3
#' would provided "ID", "ID\_2" and "ID\_3".
#' @section Warning:
#'
#' It is assumed that each vector provides is sorted by \code{gapdata} within
#' \code{id}. No checking is done, and so it is suggested that this only be
#' used on ID columns within existing, validated \code{trip} objects.
#' @seealso \code{\link{trip}}
#' @keywords manip
#' @examples
#'
#'
#' id <- gl(2, 8)
#' gd <- Sys.time() + 1:16
#' gd[c(4:6, 12:16)] <- gd[c(4:6, 12:16)] + 10000
#' sepIdGaps(id, gd, 1000)
#'
#'
#' @export sepIdGaps
sepIdGaps <- function(id, gapdata, minGap=3600 * 24 * 7) {
    toSep <- tapply(gapdata, id,
                    function(x) which(diff(unclass(x) ) > minGap))
    tripID <- split(as.character(id), id)
    for (i in 1:length(tripID)) {
        this <- toSep[[i]]
        thisID <- tripID[[i]][1]
        if (length(this) > 0) {
            for (n in 1:length(this)) {
                tripID[[i]][(this[n]+1):length(tripID[[i]])] <-
                    paste(thisID, n + 1, sep="_")
            }
        }
    }
    unsplit(tripID, id)
}





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
#' interpolated coordinates as required.
#'
#'
#' Motion between boundaries is assumed linear and extra coordinates are added
#' at the cut points.
#'
#' @param x A trip object.
#' @param breaks A character string such as the \code{breaks} argument
#' for \code{\link{cut.POSIXt}}, or alternatively a vector of
#' date-time boundaries. (If the latter hese must encompass all the time range of
#' the entire trip object.)
#' @param \dots Unused arguments.
#' @return
#'
#' A list of trip objects, named by the time boundary in which they lie.
#' @author Michael D. Sumner and Sebastian Luque
#' @details This function was completely rewritten in version 1.1-20. 
#' @seealso See also \code{\link{tripGrid}}.
#' @keywords manip chron
#' @examples
#'
#' \dontrun{
#' set.seed(66)
#' d <- data.frame(x=1:100, y=rnorm(100, 1, 10),
#'                 tms= as.POSIXct(as.character(Sys.time()), tz = "GMT") + c(seq(10, 1000, length=50),
#'                 seq(100, 1500, length=50)), id=gl(2, 50))
#' coordinates(d) <- ~x+y
#' tr <- trip(d, c("tms", "id"))
#'
#' cut(tr, "200 sec")
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
#' data("walrus818", package = "trip")
#' library(lubridate)
#' walrus_list <- cut(walrus818, seq(floor_date(min(walrus818$DataDT), "month"), 
#' ceiling_date(max(walrus818$DataDT), "month"), by = "1 month"))
#' g <- rasterize(walrus818) * NA_real_
#' stk <- raster::stack(lapply(walrus_list, rasterize, grid = g))
#' st <- raster::aggregate(stk, fact = 4, fun = sum, na.rm = TRUE)
#' st[!st > 0] <- NA_real_
#' 
#' plot(st, col = oc.colors(52))
#' @method cut trip
#' @export
cut.trip <-
function (x, breaks, ...)
{
    if ("dates" %in% names(list(...))) warning("please use \'breaks\' not \'dates\'")
  tor <- getTORnames(x)
    if (is.character(breaks)) {
        if (length(breaks) > 1) stop("if breaks is character, length(breaks) should be 1L")
        levs <- levels(cut(x[[tor[1]]], breaks))
        datebounds <- seq(as.POSIXct(levs[1L], tz = "GMT"), by = breaks, length = length(levs) + 1)
        breaks <- datebounds
    }
    
   
      uid <- unique(x[[tor[2]]])
      l <- vector("list", length(uid))
      for (i in seq_along(l)) l[[i]] <- cut.one.trip(x[x[[tor[2]]] == uid[i], ], breaks)
      l2 <- vector("list", length(l[[1]]))
      for (j in seq_along(l2)) l2[[j]] <- do.call(rbind, lapply(l, function(x) x[[j]]) )
      lapply(l2[!sapply(l2, is.null)], function(xx) trip(SpatialPointsDataFrame(SpatialPoints(as.matrix(xx[,1:2]), proj4string = CRS(proj4string(x))), xx[,-c(1, 2)]), c("time", "id")))
      
    }
    
    
    

    cut.one.trip <- function(x, breaks, ...) {
      tor <- getTORnames(x)
      coords <- coordinates(x)
      time <- x[[tor[1]]]
      id <- x[[tor[2]]]
      unbrks <- as.numeric(breaks)
      untime <- as.numeric(time)
      no_insert <- unbrks %in% untime
      unbrks2 <- unbrks[!no_insert]
      newbreaks <- sort(c(unbrks[-c(1, length(unbrks))], unbrks2[-c(1, length(unbrks2))], untime))
      newx <- approxfun(untime, coords[,1], rule = 2)(newbreaks)
      newy <- approxfun(untime, coords[,2], rule = 2)(newbreaks)
      
      ntrack <- list(x = newx, y = newy, time = ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "UTC") + newbreaks, 
                     id = rep(id[1], length = length(newx)))
      
      out <- split(as.data.frame(ntrack), cumsum(duplicated(newbreaks)))
      short <- sapply(out, nrow) < 3
      out <- lapply(out, function(x) if (nrow(x) < 3) NULL else x)
      last <- NULL ## woh fix for bitten by dplyr adding a last() function
      for (i in seq_along(out)[-1]) {
        if (short[i] & !short[i-1]) last <- tail(out[[i-1]], 1)
        if (!short[i] & short[i-1] & !is.null(last)) out[[i]][1,] <- last
      }
      out
    }
    
    

    
