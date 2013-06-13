# $Id$

## removed depends sp, suggests rgdal deprecate this, replace with
## spTransform method below
tripTransform <- function(x, crs, ...) {
    .Deprecated("spTransform")
    if (! inherits(crs, "CRS")) crs <- CRS(crs)
    spTransform(x, crs, ...)
}
setMethod("spTransform", signature("trip", "CRS"),
          function(x, CRSobj, ...) {
              if (!("rgdal" %in% loadedNamespaces())) {
                  ns <- try(loadNamespace("rgdal"))
                  if (isNamespace(ns)) {
                      message("[loaded the rgdal namespace]")
                  } else {
                      msg <- paste("This method requires the rgdal package",
                                   "but is unable to load rgdal namespace",
                                   sep=",")
                      stop(msg)
                  }
              }
              pts <- spTransform(as(x, "SpatialPointsDataFrame"),
                                 CRSobj, ...)
              trip(pts, getTORnames(x))
          })


###_ + Functions

## Need to clean up the "internal" functions, and ensure the arguments
## are passed in correctly - and figure out which arguments are really
## useful anyway

forceCompliance <- function(x, tor) {
    isSpatial <- is(x, "SpatialPointsDataFrame")
    if (isSpatial) {
        crd.nrs <- x@coords.nrs
        x <- as.data.frame(x)
    }
    levs <- unique(x[[tor[2]]])
    tooshort <- tapply(x[[1]], x[[tor[2]]], function(x) length(x) < 3)
    x <- x[x[[tor[2]]] %in% levs[!tooshort], ]
    x <- x[!duplicated(x), ]
    x <- x[order(x[[tor[2]]], x[[tor[1]]]), ]
    x[[tor[1]]] <- adjust.duplicateTimes(x[[tor[1]]], x[[tor[2]]])
    if (isSpatial) {
        coordinates(x) <- crd.nrs
        x <- trip(x, tor)
    }
    x
}

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
    newPts <- NULL
    ##if (is.null(dur))
    ##   dur <- as.numeric(min(unlist(tapply(as.integer(time),
    ##            id, diff))))
    for (sub in levs) {
        ind <- id == sub
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
        nx <- unlist(apply(ax, 1, trip:::.intpFun))
        ny <- unlist(apply(ay, 1, trip:::.intpFun))
        nt <- unlist(apply(at, 1, trip:::.intpFun)) + min(tms)
        ni <- factor(rep(sub, length=length(nt)))
        newPts <- rbind(newPts,
                        data.frame(x=nx, y=ny, time=nt, id=ni))
    }
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

tripGrid.interp <- function(x, grid=NULL, method="count", dur=NULL, ...) {
    method <- paste(method, "Points", sep="")
    if (!exists(method)) stop("no such method: ", method)
    cat("Using method ", method, "\n\n")
    if (is.null(grid)) grid <- makeGridTopology(x)
    res <- SpatialGridDataFrame(grid,
                                data.frame(z=rep(0, prod(grid@cells.dim))),
                                CRS(proj4string(x)))
    tor <- getTORnames(x)
    trip.list <- split(x[, tor], x[[tor[2]]])
    cnt <- 0
    for (this in trip.list) {
        this <- interpequal(this, dur=dur, quiet=TRUE)
        cnt <- cnt + nrow(this)
        res$z <- res$z + do.call(method,
                                 list(x=trip(this, tor), grid=grid, ...))$z
    }
    if (method == "countPoints") res$z <- res$z * dur
    res
}

kdePoints <- function (x, h=NULL, grid=NULL, resetTime=TRUE, ...) {
    coords <- coordinates(x)
    xx <- coords[ , 1]
    yy <- coords[ , 2]
    tids <- getTimeID(x)
    time <- tids[, 1]
    id <- tids[, 2]
    timesum <- sum(tapply(time, id, function(x) {
        diff(range(unclass(x)))
    }))
    ## must acknowledge MASS for this
    if (missing(h)) {
        h <- c(bandwidth.nrd(xx), bandwidth.nrd(yy))/10
    }
    if (is.null(grid))  grid <- makeGridTopology(coords, ...)
    ## use bbox here
    dimXY <- grid@cells.dim
    nx <- nrow(x)
    gcs <- coordinatevalues(grid)
    gx <- gcs$s1 + grid@cellsize[1]
    gy <- gcs$s2 + grid@cellsize[2]
    ax <- outer(gx, xx, "-")/h[1]
    ay <- outer(gy, yy, "-")/h[2]
    z <- (matrix(dnorm(ax), dimXY[1], nx) %*%
          t(matrix(dnorm(ay), dimXY[2], nx))) / (nx * h[1] * h[2])
    if (resetTime) z <- (z * timesum/sum(z)) / 3600
    SpatialGridDataFrame(grid, data.frame(z=as.vector(z)), CRS(proj4string(x)))
}

countPoints <- function (x, dur=1, grid=NULL)
{
    coords <- coordinates(x)
    xx <- coords[, 1]
    yy <- coords[, 2]
    if (is.null(grid))  grid <- makeGridTopology(coords)
    orig <- grid@cellcentre.offset - grid@cellsize / 2
    ## scl <- c(diff(grd$x)[1], diff(grd$y)[1])
    scl <- grid@cellsize
    xdim <- grid@cells.dim[1]
    ydim <- grid@cells.dim[2]
    xmin <- orig[1]
    xmax <- orig[1] + (xdim + 1) * scl[1]
    ymin <- orig[2]
    ymax <- orig[2] + (ydim + 1) * scl[2]
    xlim <- c(xmin, xmax)
    ylim <- c(ymin, ymax)
    if (xlim[1] < xmin || xlim[2] > (xmax) ||
        ylim[1] < ymin || ylim[2] > (ymax)) {
        stop("Data are out of bounds")
    }
    cps <- ceiling(cbind((xx - orig[1]) / scl[1], (yy - orig[2]) / scl[2]))
    tps <- tabulate((cps[, 1] - 1) * ydim + cps[, 2], xdim * ydim)
    mps <- matrix(tps, ydim, xdim)
    z <- t(mps)
    SpatialGridDataFrame(grid, data.frame(z=as.vector(z[, ncol(z):1])),
                         CRS(proj4string(x)))
}


makeGridTopology <- function (obj, cells.dim=c(100, 100),
                              xlim=NULL, ylim=NULL, buffer=0, cellsize=NULL,
                              adjust2longlat=FALSE) {
    if ((is.null(xlim) | is.null(ylim)) & missing(obj))
        stop("require at least a Spatial object, matrix object, or xlim and ylim")
    if (!missing(obj)) bb <- bbox(obj)
    if (!is.null(xlim) & !is.null(ylim)) buffer <- 0
    if (is.null(xlim)) xlim <- bb[1,]
    if (is.null(ylim)) ylim <- bb[2,]
    ## PROBLEMS
    ## determination is boundary based, but grid is cell based
    ## break down into simpler functions, then recombine including longlat adjust
    ## gridFromNothing - world1 ?
    ## gridFromLimits
    ## gridFromLimits/dims
    ## gridFromLimits/cellsize
    ##
    ## gridFromDims?
    ## gridFromCellsize?
    ## gridFromDims/Cellsize?
    ## proj <- NA
    ## if (!missing(obj)) proj <- is.projected(obj)
    ## if (is.na(proj)) {
    ## 	warning("coordinate system unknown, assuming longlat")
    ## 	proj <- FALSE
    ## }
    if (is.null(cellsize) & adjust2longlat)
        warning("cellsize not provided with adjust2longlat, ignoring")
    if (!is.null(cellsize)) {
        if (!length(cellsize) == 2)
            stop("cellsize must be of length 2")
        if (adjust2longlat) {
            cellsize <- c(cellsize[1] /
                          (cos((pi / 180) * mean(ylim)) * 1.852 * 60),
                          cellsize[2] / (1.852 * 60))
            if (any(!cellsize > 0)) {
                msg <- paste("longlat adjustment resulted in invalid",
                             "cellsize. Does it really make sense for",
                             "these latitude limits? \n")
                stop(msg, paste(format(ylim), collapse=","))
            }
        }
        xvalues <- seq(xlim[1], xlim[2] + cellsize[1], by=cellsize[1])
        yvalues <- seq(ylim[1], ylim[2] + cellsize[2], by=cellsize[2])
        xlim <- range(xvalues)
        ylim <- range(yvalues)
        cells.dim <- c(length(xvalues), length(yvalues))
    } else cellsize <- c(diff(xlim), diff(ylim)) / (cells.dim - 1)
    if (buffer > 0) {
        addXY <- ceiling(cellsize * buffer)
        xlim <- xlim + c(-addXY[1], addXY[1])
        ylim <- ylim + c(-addXY[2], addXY[2])
        cellsize <- c(diff(xlim), diff(ylim)) / (cells.dim - 1)
    }
    new("GridTopology", cellcentre.offset=c(min(xlim), min(ylim)),
        cellsize=cellsize, cells.dim=as.integer(cells.dim))
}

adjust.duplicateTimes <- function (time, id) {
    dups <- unlist(tapply(time, id, duplicated), use.names=FALSE)
    if (any(dups)) {
        time[dups] <- time[dups] + 1
        time <- Recall(time, id)
    }
    time
}

argos.sigma <- function(x, sigma=c(100, 80, 50, 20, 10, 4,  2),
                        adjust=111.12) {
    sigma <- sigma / adjust
    names(sigma) <- levels(x)
    sigma[x]
}

readArgos <- function (x, correct.all=TRUE, dtFormat="%Y-%m-%d %H:%M:%S",
                       tz="GMT", duplicateTimes.eps=1e-2,
                       p4="+proj=longlat +ellps=WGS84", verbose=FALSE) {
    ## add "correct.all" argument - just return data frame if it fails, with
    ## suggestions of how to sort/fix it
    dout <- NULL
    for (con in x) {
        old.opt <- options(warn=-1)
        dlines <- strsplit(readLines(con), "\\s+", perl=TRUE)
        options(old.opt)
        loclines <- sapply(dlines, length) == 12
        if (any(loclines)) {
            dfm <- matrix(unlist(dlines[sapply(dlines, length) == 12]),
                          ncol=12, byrow=TRUE)
            if (dfm[1,7] == "LC") {
                msg <- paste(" appears to be a diag file, skipping.",
                             "Use readDiag to obtain a dataframe. \n\n")
            	cat("file ", con, msg)
            	next
            }
            df <- vector("list", 12)
            names(df) <- c("prognum", "ptt", "nlines", "nsensor",
                           "satname", "class", "date", "time", "latitude",
                           "longitude", "altitude", "transfreq")
            for (i in c(1:4, 9:12)) df[[i]] <- as.numeric(dfm[, i])
            for (i in 5:6) df[[i]] <- factor(dfm[, i])
            for (i in 7:8) df[[i]] <- dfm[, i]
            df <- as.data.frame(df)
            df$gmt <- as.POSIXct(strptime(paste(df$date, df$time),
                                          dtFormat), tz)
            dout <- rbind(dout, df)
        } else {
            cat("Problem with file: ", con, " skipping\n")
        }
    }
    if (is.null(dout))
        stop("No data to return: check the files")
    if (correct.all) {
        ## should add a reporting mechanism for these as well
        ##  and return a data.frame if any of the tests fail
        ## sort them
        dout <- dout[order(dout$ptt, dout$gmt), ]
        ## remove duplicate rows
        dout <- dout[!duplicated(dout), ]
        ## adjust duplicate times (now that they are sorted properly)
        dt.by.id <- unlist(tapply(dout$gmt, dout$ptt,
                                  function(x) c(-1, diff(x))))
        dup.by.eps <- which(abs(dt.by.id) < duplicateTimes.eps)
        if (length(dup.by.eps) >= 1) {
            if (verbose) {
                cat("Adjusting duplicate times\n.....\n")
                for (i in  dup.by.eps) {
                    ind <- i + (-2:1)
                    print(cbind(dout[ind,c("ptt", "gmt", "class")],
                                row.number=ind))
                }
            }
            dout$gmt <- adjust.duplicateTimes(dout$gmt, dout$ptt)
            if (verbose) {
                cat("\n  Adjusted records now: \n\n")
                for (i in  dup.by.eps) {
                    ind <- i + (-2:1)
                    print(cbind(dout[ind,c("ptt", "gmt", "class")],
                                row.number=ind))
                }
            }
        }
        if(any(dout$longitude > 180)) {
            msg <- paste("\nLongitudes contain values greater than 180,",
                         "assuming proj.4 +over\n\n")
            cat(msg)
            p4 <- "+proj=longlat +ellps=WGS84 +over"
        }
        dout$class <- ordered(dout$class,
                              levels=c("Z", "B", "A", "0", "1", "2", "3"))
        coordinates(dout) <- c("longitude", "latitude")
        proj4string(dout) <- CRS(p4)
        ##tor <- TimeOrderedRecords(c("gmt", "ptt"))
        test <- try(dout <- trip(dout, c("gmt", "ptt")))
        if (!is(test, "trip")) {
            cat("\n\n\n Data not validated: returning object of class ",
                class(dout), "\n")
            return(dout)
        }
        ## for now, only return spdftor if correct.all is TRUE
        cat("\n\n\n Data fully validated: returning object of class ",
            class(dout), "\n")
        return(dout)
    }
    cat("\n\n\n Data not validated: returning object of class ",
        class(dout), "\n")
    dout
}


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


.distances <- function(x) {
  proj <- is.projected(x)
  if (is.na(proj)) proj <- FALSE

  
  lapply(split(x, x[[getTORnames(x)[2]]]), function(x) trackDistance(coordinates(x), longlat = !proj))
  
}

###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:


