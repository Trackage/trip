# $Id$


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

## method to allow transformation with character only
setMethod("spTransform", signature("Spatial", "character"), 
          function(x, CRSobj, ...) {
            
            .local <- function (object, pstring, ...) 
            {
              crs <- try(CRS(pstring))
              if (inherits(crs, "try-error")) { stop(sprintf("cannot determine valid CRS from %s", pstring))
              } else {
                spTransform(x, crs)
              }
            }
            
            .local(x, pstring = CRSobj, ...)
            
          })

###_ + Functions

## Need to clean up the "internal" functions, and ensure the arguments
## are passed in correctly - and figure out which arguments are really
## useful anyway



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



#' Generate a grid of time spent using approximate methods
#' 
#' 
#' Create a grid of time spent from an object of class \code{trip} by
#' approximating the time between locations for separate trip events.
#' 
#' 
#' This set of functions was the the original tripGrid from prior to version
#' 1.1-6. \code{tripGrid} should be used for more exact and fast calculations
#' assuming linear motion between fixes.
#' 
#' The intention is for \code{tripGrid.interp} to be used for exploring
#' approximate methods of line-to-cell gridding.
#' 
#' Trip locations are first interpolated, based on an equal-time spacing
#' between records. These interpolated points are then "binned" to a grid of
#' cells.  The time spacing is specified by the "dur"ation argument to
#' \code{interpequal} in seconds (i.e. \code{dur=3600} is used for 1 hour).
#' Shorter time periods will require longer computation with a closer
#' approximation to the total time spent in the gridded result.
#' 
#' Currently there are methods "count" and "kde" for quantifying time spent,
#' corresponding to the functions "countPoints" and "kdePoints". "kde" uses
#' kernel density to smooth the locations, "count" simply counts the points
#' falling in a grid cell.
#' 
#' @aliases tripGrid.interp interpequal countPoints kdePoints
#' @param x object of class trip
#' @param grid GridTopology - will be generated automatically if NULL
#' @param method name of method for quantifying time spent, see Details
#' @param dur The \"dur\"ation of time used to interpolate between available
#' locations (see Details)
#' @param \dots other arguments passed to \code{interpequal} or \code{kdePoints}
#' @return
#' 
#' \code{tripGrid} returns an object of class \code{SpatialGridDataFrame}, with
#' one column "z" containing the time spent in each cell in seconds. If
#' kdePoints is used the units are not related to the time values and must be
#' scaled for further use.
#' @keywords manip
#' @export tripGrid.interp
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

#' @param h kernel bandwidth
#' @param resetTime rescale result back to the total duration of the input
#' @rdname tripGrid.interp
#' @seealso \code{\link[MASS]{bandwidth.nrd}} for the calculation of bandwidth values used internally when not supplied by the user
##' @importFrom MASS bandwidth.nrd
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

#' @rdname tripGrid.interp
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




#' Generate a GridTopology from a Spatial object
#' 
#' 
#' Sensible defaults are assumed, to match the extents of data to a manageable
#' grid.
#' 
#' Approximations for kilometres in longlat can be made using \code{cellsize}
#' and \code{adjust2longlat}.
#' 
#' 
#' @param obj any Spatial object, or other object for which \code{bbox} will
#' work
#' @param cells.dim the number of cells of the grid, x then y
#' @param xlim x limits of the grid
#' @param ylim y limits of the grid
#' @param buffer proportional size of the buffer to add to the grid limits
#' @param cellsize pixel cell size
#' @param adjust2longlat assume cell size is in kilometres and provide simple
#' adjustment for earth-radius cells at the north-south centre of the grid
#' @keywords manip
#' @export makeGridTopology
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
#' kilometeres, and the values returned are the approximate values for longlat
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
    sigma <- sigma / adjust
    names(sigma) <- levels(x)
    sigma[x]
}



#' Read Argos "DAT" or "DIAG" files
#' 
#' 
#' Return a (Spatial) data frame of location records from raw Argos files.
#' Multiple files may be read, and each set of records is appended to the data
#' frame in turn.  Basic validation of the data is enforced by default.
#' 
#' 
#' \code{readArgos} performs basic validation checks for class \code{trip} are
#' made, and enforced based on \code{correct.all}:
#' 
#' No duplicate records in the data, these are simply removed.  Records are
#' ordered by DateTime ("date", "time", "gmt") within ID ("ptt").  No duplicate
#' DateTime values within ID are allowed: to enforce this the time values are
#' moved forward by one second - this is done recursively and is not robust.
#' 
#' If validation fails the function will return a
#' \code{\link[sp]{SpatialPointsDataFrame}}.  Files that are not obviously of
#' the required format are skipped.
#' 
#' Argos location quality data "class" are ordered, assuming that the available
#' levels is \code{levels=c("Z", "B", "A", "0", "1", "2", "3")}.
#' 
#' A projection string is added to the data, assuming the PROJ.4 longlat - if
#' any longitudes are greater than 360 the PROJ.4 argument "+over" is added.
#' 
#' \code{readDiag} simply builds a \code{data.frame}.
#' 
#' @aliases readArgos readDiag
#' @param x vector of file names of Argos "DAT" or "DIAG" files.
#' @param correct.all logical - enforce validity of data as much as possible?
#' (see Details)
#' @param dtFormat the DateTime format used by the Argos data "date" and "time"
#' pasted together
#' @param tz timezone - GMT/UTC is assumed
#' @param duplicateTimes.eps what is the tolerance for times being duplicate?
#' @param p4 PROJ.4 projection string, "+proj=longlat +ellps=WGS84" is assumed
#' @param verbose if TRUE, details on date-time adjustment is reported
#' @return
#' 
#' \code{readArgos} returns a \code{trip} object, if all goes well, or simply a
#' \code{\link[sp]{SpatialPointsDataFrame}}.
#' 
#' \code{readDiag} returns a \code{data.frame} with 8 columns:
#' \itemize{
#' \item {\code{lon1},\code{lat1} first pair of coordinates}
#' \item {\code{lon1},\code{lat1} second pair of coordinates}
#' \item {gmt DateTimes as POSIXct}
#' \item {id Platform Transmitting Terminal (PTT) ID}
#' \item {lq Argos location quality class}
#' \item {iq some other thing}
#' }
#' @section Warning :
#' 
#' This works on some Argos files I have seen, it is not a guaranteed method
#' and is in no way linked officially to Argos.
#' @seealso
#' 
#' \code{\link{trip}}, \code{\link[sp]{SpatialPointsDataFrame}},
#' \code{\link{adjust.duplicateTimes}}, for manipulating these data, and
#' \code{\link{argos.sigma}} for relating a numeric value to Argos quality
#' "classes".
#' 
#' \code{\link{sepIdGaps}} for splitting the IDs in these data on some minimum
#' gap.
#' 
#' \code{\link{order}}, \code{\link{duplicated}}, , \code{\link{ordered}} for
#' general manipulation of this type.
#' @references
#' 
#' The Argos data documentation is at
#' \url{http://www.argos-system.org/manual/}.  Specific details on the PRV
#' ("provide data") format were found here
#' \url{http://www.cls.fr/manuel/html/chap4/chap4_4_8.htm}.
#' @keywords IO manip
#' @export readArgos
readArgos <- function (x, correct.all=TRUE, dtFormat="%Y-%m-%d %H:%M:%S",
                       tz="GMT", duplicateTimes.eps=1e-2,
                       p4="+proj=longlat +ellps=WGS84", verbose=FALSE) {
    ## add "correct.all" argument - just return data frame if it fails, with
    ## suggestions of how to sort/fix it
  
  
  ## this should be heaps faster
    dout <- vector("list", length(x))
    for (icon in seq_along(x)) {
        old.opt <- options(warn=-1)
        dlines <- strsplit(readLines(x[icon]), "\\s+", perl=TRUE)
        options(old.opt)
        loclines <- sapply(dlines, length) == 12
        if (any(loclines)) {
            dfm <- matrix(unlist(dlines[sapply(dlines, length) == 12]),
                          ncol=12, byrow=TRUE)
            if (dfm[1,7] == "LC") {
                msg <- paste(" appears to be a diag file, skipping.",
                             "Use readDiag to obtain a dataframe. \n\n")
            	cat("file ", icon, msg)
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
            dout[[icon]] <- df
        } else {
            cat("Problem with file: ", x[icon], " skipping\n")
            
        }
    }
    if (all(sapply(dout, is.null)))
        stop("No data to return: check the files")
     
    dout <- do.call(rbind, dout)
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

##' @rdname readArgos
##' @export
readDiag <- function (x) {
  data <- NULL
  for (fl in x) {
    d <- readLines(fl)
    locs <- d[grep("LON1", d, ignore.case=TRUE)]
    tms <- d[grep("DATE", d, ignore.case=TRUE)]
    bad <- (grep("\\?", locs))
    if (length(bad) > 0) {
      if (length(locs[-bad]) == 0) {
        warning(paste("no valid locations in:", fl, "\n ...ignoring"))
        next
      }
      locs <- locs[-bad]
      tms <- tms[-(bad)]
    }
    dlines <- paste(locs, tms)
    dlines <- strsplit(dlines, "\\s+", perl=TRUE)
    reclen <- length(dlines[[1]])
    dfm <- matrix(unlist(dlines[sapply(dlines, length) ==
                                  reclen]), ncol=reclen, byrow=TRUE)
    lonlat <- dfm[, c(4, 7, 10, 13)]
    dic <- dfm[, c(14, 17, 18, 21, 24), drop=FALSE]
    id <- dic[, 1]
    gmt <- as.POSIXct(strptime(paste(dic[, 2], dic[, 3]),
                               "%d.%m.%y %H:%M:%S"), tz="GMT")
    lq <- dic[, 4]
    iq <- dic[, 5]
    ll <- as.vector(lonlat)
    ll[grep("S", ll)] <- paste("-", ll[grep("S", ll)], sep="")
    ll <- gsub("S", "", ll)
    ll[grep("N", ll)] <- paste("", ll[grep("N", ll)], sep="")
    ll <- gsub("N", "", ll)
    ll[grep("E", ll)] <- paste("", ll[grep("E", ll)], sep="")
    ll <- gsub("E", "", ll)
    ll[grep("W", ll)] <- paste("-", ll[grep("W", ll)], sep="")
    ll <- gsub("W", "", ll)
    ll <- matrix(as.numeric(ll), ncol=4)
    lon <- ll[, 2]
    lon2 <- ll[, 4]
    lq <- factor(lq, ordered=TRUE,
                 levels=c("Z", "B", "A", "0", "1", "2", "3"))
    data <- rbind(data,
                  data.frame(lon1=lon, lat1=ll[, 1],
                             lon2=lon2, lat2=ll[, 3],
                             gmt=gmt, id=id, lq=lq, iq=iq))
  }
  data
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

