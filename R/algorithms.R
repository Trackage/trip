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
## sp::coordinates(d) <- ~x+y
## tr <- trip(d, c("tms", "id"))

## bound.dates <- seq(min(tr$tms)-1, max(tr$tms)+1, length=5)
## trip.list <- trip.split.exact(tr, bound.dates)





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
#' date-time boundaries. (If the latter these must encompass all the time range of
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
#' sp::coordinates(d) <- ~x+y
#' tr <- trip(d, c("tms", "id"))
#'
#' cut(tr, "200 sec")
#'
#' bound.dates <- seq(min(tr$tms) - 1, max(tr$tms) + 1, length=5)
#' trip.list <- cut(tr, bound.dates)
#' bb <- bbox(tr)
#' cn <- c(20, 8)
#' g <- sp::GridTopology(bb[, 1], apply(bb, 1, diff) / (cn - 1), cn)
#'
#' tg <- tripGrid(tr, grid=g)
#' tg <- sp::as.image.SpatialGridDataFrame(tg)
#' tg$x <- tg$x - diff(tg$x[1:2]) / 2
#' tg$y <- tg$y - diff(tg$y[1:2]) / 2
#'
#' op <- par(mfcol=c(4, 1))
#' for (i in 1:length(trip.list)) {
#'   plot(sp::coordinates(tr), pch=16, cex=0.7)
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
#' g <- sp::GridTopology(bb[, 1], apply(bb, 1, diff) / (cn - 1), cn)
#'
#' tg <- tripGrid(tr, grid=g)
#' tg <- sp::as.image.SpatialGridDataFrame(tg)
#' tg$x <- tg$x - diff(tg$x[1:2]) / 2
#' tg$y <- tg$y - diff(tg$y[1:2]) / 2
#'
#' op <- par(mfcol=c(4, 1))
#' for (i in 1:length(trip.list)) {
#'   plot(sp::coordinates(tr), pch=16, cex=0.7)
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
      lapply(l2[!sapply(l2, is.null)], function(xx) trip(SpatialPointsDataFrame(SpatialPoints(as.matrix(xx[,1:2]), proj4string = CRS(x@proj4string@projargs, doCheckCRSArgs = FALSE)), xx[,-c(1, 2)]), c("time", "id")))
      
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


##' @rdname trip-internal
.intpFun <- function(x) {
   len <- round(x[3] + 1)
   new <- seq(x[1], x[2], length=len)
   if (len > 1)
      new[-len]
   else new
}

interpequal <- function(x, dur=NULL, quiet=FALSE) {
   if (!is(x, "trip"))
      stop("only trip objects supported")
   if (is.null(dur))
      stop("equal time duration must be specified \"dur=?\"")
   ## x must be a single trip
   tor <- getTORnames(x)
   tids <- getTimeID(x)
   time <- tids[, 1]
   id <- factor(tids[, 2])
   coords <- coordinates(x)
   x <- coords[,1]
   y <- coords[,2]
   levs <- levels(id)
   newPts <- vector("list", length(levs))
   ##if (is.null(dur))
   ##   dur <- as.numeric(min(unlist(tapply(as.integer(time),
   ##            id, diff))))
   for (isub in seq_along(levs)) {
      ind <- id == levs[isub]
      xx <- x[ind]
      yy <- y[ind]
      tms <- time[ind]
      dt <- diff(as.numeric(tms))
      dtn <- dt/dur
      ax <- cbind(xx, c(xx[-1], xx[length(xx)]), c(dtn, 0))
      ay <- cbind(yy, c(yy[-1], yy[length(yy)]), c(dtn, 0))
      intime <- as.numeric(tms) - min(as.numeric(tms))
      at <- cbind(intime, c(intime[-1], intime[length(intime)]),
                  c(dtn, 0))
      nx <- unlist(apply(ax, 1, .intpFun))
      ny <- unlist(apply(ay, 1, .intpFun))
      nt <- unlist(apply(at, 1, .intpFun)) + min(tms)
      ni <- factor(rep(levs[isub], length=length(nt)))
      ##    newPts <- rbind(newPts,
      newPts[[isub]] <- data.frame(x=nx, y=ny, time=nt, id=ni)
   }
   newPts <- do.call("rbind", newPts)
   
   origTotal <- sum(tapply(time, id, function(x) {
      diff(range(as.numeric(x)))
   }))
   newTotal <- nrow(newPts) * dur
   uType <- "hours"
   hTotal <- sum(tapply(time, id, function(x) {
      difftime(range(x)[2], range(x)[1], units=uType)
   }))
   if (!quiet) {
      cat("lost seconds=", as.integer(origTotal - newTotal),
          " out of a total ", hTotal, " ", uType, "\n")
   }
   coordinates(newPts) <- c("x", "y")
   names(newPts) <- tor
   newPts
}


##interGC <- function(x, dur = NULL, intfun = NULL) {
##    if (is.null(intfun)) {
##        require(geosphere)
##        intfun <- gcIntermediate
##    }

##    tor <- getTORnames(x)
##    ids <- x[[tor[2]]]
##    times <- x[[tor[1]]]
##    if (is.null(dur))  stop("dur must be specified for minimum interpolation duration")
    ##dur <- min(sapply(split(times, ids), function(x) min(diff(unclass(x)))))

##    triplist <- split(x, ids)
##    newtriplist <- vector("list", length(triplist))
##    for (itrip in seq_along(triplist)) {
##        icoords <- coordinates(triplist[[itrip]])
 ##       itimes <- triplist[[itrip]][[tor[1]]]
 ##       newcoords <- vector("list", nrow(icoords) - 1)
 ##       dntime <- pmax(ceiling(diff(unclass(itimes)) / dur), 3)
 ##       for (ipt in seq_len(nrow(icoords)-1)) {
 ##           newcoords[[ipt]] <- data.frame(intfun(icoords[ipt, ], icoords[ipt+1,], n = dntime[ipt] - 2, addStartEnd = TRUE),
 ##                              gmt = seq(itimes[ipt], itimes[ipt+1], length = dntime[ipt]), id = rep(ids[itrip], dntime[ipt]))

   ##     }
    ##    newtriplist[[itrip]] <- do.call(rbind, newcoords)
   ## }
   ## newtrip <- do.call(rbind, newtriplist)
   ## names(newtrip) <- c(colnames(icoords), tor)
   ## newtrip <- newtrip[!duplicated(newtrip), ]
   ## coordinates(newtrip) <- 1:2
   ## proj4string(newtrip) <- CRS(proj4string(x))
   ## trip(newtrip, tor)
##}


