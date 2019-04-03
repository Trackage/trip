
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


