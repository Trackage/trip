
<!-- saved from url=(0096)https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/trip/R/trip.R?revision=130&root=trip -->
<html><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"></head><body><pre style="word-wrap: break-word; white-space: pre-wrap;"># $Id$

## removed depends sp, suggests rgdal deprecate this, replace with
## spTransform method below
tripTransform &lt;- function(x, crs, ...) {
    ##require(rgdal) || stop("rgdal package is required, but unavailable")
    .Deprecated("spTransform")
    if (! inherits(crs, "CRS")) crs &lt;- CRS(crs)
    spTransform(x, crs, ...)
}

setMethod("spTransform", signature("trip", "CRS"),
          function(x, CRSobj, ...) {
              if (!("rgdal" %in% loadedNamespaces())) {
                  ns &lt;- try(loadNamespace("rgdal"))
                  if (isNamespace(ns)) {
                      message("[loaded the rgdal namespace]")
                  } else {
                      msg &lt;- paste("This method requires the rgdal package",
                                   "but is unable to load rgdal namespace",
                                   sep=",")
                      stop(msg)
                  }
              }
              pts &lt;- spTransform(as(x, "SpatialPointsDataFrame"),
                                 CRSobj, ...)
              trip(pts, getTORnames(x))
              ## mystuff(coordinates(y), proj = proj4string(y))
          })


###_ + Functions

## Need to clean up the "internal" functions, and ensure the arguments
## are passed in correctly - and figure out which arguments are really
## useful anyway

forceCompliance &lt;- function(x, tor) {
    isSpatial &lt;- is(x, "SpatialPointsDataFrame")
    if (isSpatial) {
        crd.nrs &lt;- x@coords.nrs
        x &lt;- as.data.frame(x)
    }
    levs &lt;- unique(x[[tor[2]]])
    tooshort &lt;- tapply(x[[1]], x[[tor[2]]], function(x) length(x) &lt; 3)
    x &lt;- x[x[[tor[2]]] %in% levs[!tooshort], ]
    x &lt;- x[!duplicated(x), ]
    x &lt;- x[order(x[[tor[2]]], x[[tor[1]]]), ]
    x[[tor[1]]] &lt;- adjust.duplicateTimes(x[[tor[1]]], x[[tor[2]]])
    if (isSpatial) {
        coordinates(x) &lt;- crd.nrs
        x &lt;- trip(x, tor)
    }
    x
}

.intpFun &lt;- function(x) {
    len &lt;- round(x[3] + 1)
    new &lt;- seq(x[1], x[2], length=len)
    if (len &gt; 1)
        new[-len]
    else new
}

interpequal &lt;- function(x, dur=NULL, quiet=FALSE) {
    if (!is(x, "trip"))
        stop("only trip objects supported")
    if (is.null(dur))
        stop("equal time duration must be specified \"dur=?\"")
    ## x must be a single trip
    tor &lt;- getTORnames(x)
    tids &lt;- getTimeID(x)
    time &lt;- tids[, 1]
    id &lt;- factor(tids[, 2])
    coords &lt;- coordinates(x)
    x &lt;- coords[,1]
    y &lt;- coords[,2]
    levs &lt;- levels(id)
    newPts &lt;- NULL
    ##if (is.null(dur))
    ##   dur &lt;- as.numeric(min(unlist(tapply(as.integer(time),
    ##            id, diff))))
    for (sub in levs) {
        ind &lt;- id == sub
        xx &lt;- x[ind]
        yy &lt;- y[ind]
        tms &lt;- time[ind]
        dt &lt;- diff(as.numeric(tms))
        dtn &lt;- dt/dur
        ax &lt;- cbind(xx, c(xx[-1], xx[length(xx)]), c(dtn, 0))
        ay &lt;- cbind(yy, c(yy[-1], yy[length(yy)]), c(dtn, 0))
        intime &lt;- as.numeric(tms) - min(as.numeric(tms))
        at &lt;- cbind(intime, c(intime[-1], intime[length(intime)]),
	            c(dtn, 0))
        nx &lt;- unlist(apply(ax, 1, trip:::.intpFun))
        ny &lt;- unlist(apply(ay, 1, trip:::.intpFun))
        nt &lt;- unlist(apply(at, 1, trip:::.intpFun)) + min(tms)
        ni &lt;- factor(rep(sub, length=length(nt)))
        newPts &lt;- rbind(newPts,
                        data.frame(x=nx, y=ny, time=nt, id=ni))
    }
    origTotal &lt;- sum(tapply(time, id, function(x) {
        diff(range(as.numeric(x)))
        }))
    newTotal &lt;- nrow(newPts) * dur
    uType &lt;- "hours"
    hTotal &lt;- sum(tapply(time, id, function(x) {
        difftime(range(x)[2], range(x)[1], units=uType)
        }))
    if (!quiet) {
        cat("lost seconds=", as.integer(origTotal - newTotal),
            " out of a total ", hTotal, " ", uType, "\n")
    }
    coordinates(newPts) &lt;- c("x", "y")
    names(newPts) &lt;- tor
    newPts
}

tripGrid.interp &lt;- function(x, grid=NULL, method="count", dur=NULL, ...) {
    method &lt;- paste(method, "Points", sep="")
    if (!exists(method)) stop("no such method: ", method)
    cat("Using method ", method, "\n\n")
    if (is.null(grid)) grid &lt;- makeGridTopology(x)
    res &lt;- SpatialGridDataFrame(grid,
                                data.frame(z=rep(0, prod(grid@cells.dim))),
                                CRS(proj4string(x)))
    tor &lt;- getTORnames(x)
    trip.list &lt;- split(x[, tor], x[[tor[2]]])
    cnt &lt;- 0
    for (this in trip.list) {
        this &lt;- interpequal(this, dur=dur, quiet=TRUE)
        cnt &lt;- cnt + nrow(this)
        res$z &lt;- res$z + do.call(method,
                                 list(x=trip(this, tor), grid=grid, ...))$z
    }
    if (method == "countPoints") res$z &lt;- res$z * dur
    res
}

kdePoints &lt;- function (x, h=NULL, grid=NULL, resetTime=TRUE, ...) {
    coords &lt;- coordinates(x)
    xx &lt;- coords[ , 1]
    yy &lt;- coords[ , 2]
    tids &lt;- getTimeID(x)
    time &lt;- tids[, 1]
    id &lt;- tids[, 2]
    timesum &lt;- sum(tapply(time, id, function(x) {
        diff(range(unclass(x)))
    }))
    ## must acknowledge MASS for this
    if (missing(h)) {
        h &lt;- c(bandwidth.nrd(xx), bandwidth.nrd(yy))/10
    }
    if (is.null(grid))  grid &lt;- makeGridTopology(coords, ...)
    ## use bbox here
    dimXY &lt;- grid@cells.dim
    nx &lt;- nrow(x)
    gcs &lt;- coordinatevalues(grid)
    gx &lt;- gcs$s1 + grid@cellsize[1]
    gy &lt;- gcs$s2 + grid@cellsize[2]
    ax &lt;- outer(gx, xx, "-")/h[1]
    ay &lt;- outer(gy, yy, "-")/h[2]
    z &lt;- (matrix(dnorm(ax), dimXY[1], nx) %*%
          t(matrix(dnorm(ay), dimXY[2], nx))) / (nx * h[1] * h[2])
    if (resetTime) z &lt;- (z * timesum/sum(z)) / 3600
    SpatialGridDataFrame(grid, data.frame(z=as.vector(z)), CRS(proj4string(x)))
}

countPoints &lt;- function (x, dur=1, grid=NULL)
{
    coords &lt;- coordinates(x)
    xx &lt;- coords[, 1]
    yy &lt;- coords[, 2]
    if (is.null(grid))  grid &lt;- makeGridTopology(coords)
    orig &lt;- grid@cellcentre.offset - grid@cellsize / 2
    ## scl &lt;- c(diff(grd$x)[1], diff(grd$y)[1])
    scl &lt;- grid@cellsize
    xdim &lt;- grid@cells.dim[1]
    ydim &lt;- grid@cells.dim[2]
    xmin &lt;- orig[1]
    xmax &lt;- orig[1] + (xdim + 1) * scl[1]
    ymin &lt;- orig[2]
    ymax &lt;- orig[2] + (ydim + 1) * scl[2]
    xlim &lt;- c(xmin, xmax)
    ylim &lt;- c(ymin, ymax)
    if (xlim[1] &lt; xmin || xlim[2] &gt; (xmax) ||
        ylim[1] &lt; ymin || ylim[2] &gt; (ymax)) {
        stop("Data are out of bounds")
    }
    cps &lt;- ceiling(cbind((xx - orig[1]) / scl[1], (yy - orig[2]) / scl[2]))
    tps &lt;- tabulate((cps[, 1] - 1) * ydim + cps[, 2], xdim * ydim)
    mps &lt;- matrix(tps, ydim, xdim)
    z &lt;- t(mps)
    SpatialGridDataFrame(grid, data.frame(z=as.vector(z[, ncol(z):1])),
                         CRS(proj4string(x)))
}


makeGridTopology &lt;- function (obj, cells.dim=c(100, 100),
                              xlim=NULL, ylim=NULL, buffer=0, cellsize=NULL,
                              adjust2longlat=FALSE) {
    if ((is.null(xlim) | is.null(ylim)) &amp; missing(obj))
        stop("require at least a Spatial object, matrix object, or xlim and ylim")
    if (!missing(obj)) bb &lt;- bbox(obj)
    if (!is.null(xlim) &amp; !is.null(ylim)) buffer &lt;- 0
    if (is.null(xlim)) xlim &lt;- bb[1,]
    if (is.null(ylim)) ylim &lt;- bb[2,]
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
    ## proj &lt;- NA
    ## if (!missing(obj)) proj &lt;- is.projected(obj)
    ## if (is.na(proj)) {
    ## 	warning("coordinate system unknown, assuming longlat")
    ## 	proj &lt;- FALSE
    ## }
    if (is.null(cellsize) &amp; adjust2longlat)
        warning("cellsize not provided with adjust2longlat, ignoring")
    if (!is.null(cellsize)) {
        if (!length(cellsize) == 2)
            stop("cellsize must be of length 2")
        if (adjust2longlat) {
            cellsize &lt;- c(cellsize[1] /
                          (cos((pi / 180) * mean(ylim)) * 1.852 * 60),
                          cellsize[2] / (1.852 * 60))
            if (any(!cellsize &gt; 0)) {
                msg &lt;- paste("longlat adjustment resulted in invalid",
                             "cellsize. Does it really make sense for",
                             "these latitude limits? \n")
                stop(msg, paste(format(ylim), collapse=","))
            }
        }
        xvalues &lt;- seq(xlim[1], xlim[2] + cellsize[1], by=cellsize[1])
        yvalues &lt;- seq(ylim[1], ylim[2] + cellsize[2], by=cellsize[2])
        xlim &lt;- range(xvalues)
        ylim &lt;- range(yvalues)
        cells.dim &lt;- c(length(xvalues), length(yvalues))
    } else cellsize &lt;- c(diff(xlim), diff(ylim)) / (cells.dim - 1)
    if (buffer &gt; 0) {
        addXY &lt;- ceiling(cellsize * buffer)
        xlim &lt;- xlim + c(-addXY[1], addXY[1])
        ylim &lt;- ylim + c(-addXY[2], addXY[2])
        cellsize &lt;- c(diff(xlim), diff(ylim)) / (cells.dim - 1)
    }
    new("GridTopology", cellcentre.offset=c(min(xlim), min(ylim)),
        cellsize=cellsize, cells.dim=as.integer(cells.dim))
}

adjust.duplicateTimes &lt;- function (time, id) {
    dups &lt;- unlist(tapply(time, id, duplicated), use.names=FALSE)
    if (any(dups)) {
        time[dups] &lt;- time[dups] + 1
        time &lt;- Recall(time, id)
    }
    time
}

argos.sigma &lt;- function(x, sigma=c(100, 80, 50, 20, 10, 4,  2),
                        adjust=111.12) {
    sigma &lt;- sigma / adjust
    names(sigma) &lt;- levels(x)
    sigma[x]
}

readArgos &lt;- function (x, correct.all=TRUE, dtFormat="%Y-%m-%d %H:%M:%S",
                       tz="GMT", duplicateTimes.eps=1e-2,
                       p4="+proj=longlat +ellps=WGS84", verbose=FALSE) {
    ## add "correct.all" argument - just return data frame if it fails, with
    ## suggestions of how to sort/fix it
    dout &lt;- NULL
    for (con in x) {
        old.opt &lt;- options(warn=-1)
        dlines &lt;- strsplit(readLines(con), "\\s+", perl=TRUE)
        options(old.opt)
        loclines &lt;- sapply(dlines, length) == 12
        if (any(loclines)) {
            dfm &lt;- matrix(unlist(dlines[sapply(dlines, length) == 12]),
                          ncol=12, byrow=TRUE)
            if (dfm[1,7] == "LC") {
                msg &lt;- paste(" appears to be a diag file, skipping.",
                             "Use readDiag to obtain a dataframe. \n\n")
            	cat("file ", con, msg)
            	next
            }
            df &lt;- vector("list", 12)
            names(df) &lt;- c("prognum", "ptt", "nlines", "nsensor",
                           "satname", "class", "date", "time", "latitude",
                           "longitude", "altitude", "transfreq")
            for (i in c(1:4, 9:12)) df[[i]] &lt;- as.numeric(dfm[, i])
            for (i in 5:6) df[[i]] &lt;- factor(dfm[, i])
            for (i in 7:8) df[[i]] &lt;- dfm[, i]
            df &lt;- as.data.frame(df)
            df$gmt &lt;- as.POSIXct(strptime(paste(df$date, df$time),
                                          dtFormat), tz)
            dout &lt;- rbind(dout, df)
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
        dout &lt;- dout[order(dout$ptt, dout$gmt), ]
        ## remove duplicate rows
        dout &lt;- dout[!duplicated(dout), ]
        ## adjust duplicate times (now that they are sorted properly)
        dt.by.id &lt;- unlist(tapply(dout$gmt, dout$ptt,
                                  function(x) c(-1, diff(x))))
        dup.by.eps &lt;- which(abs(dt.by.id) &lt; duplicateTimes.eps)
        if (length(dup.by.eps) &gt;= 1) {
            if (verbose) {
                cat("Adjusting duplicate times\n.....\n")
                for (i in  dup.by.eps) {
                    ind &lt;- i + (-2:1)
                    print(cbind(dout[ind,c("ptt", "gmt", "class")],
                                row.number=ind))
                }
            }
            dout$gmt &lt;- adjust.duplicateTimes(dout$gmt, dout$ptt)
            if (verbose) {
                cat("\n  Adjusted records now: \n\n")
                for (i in  dup.by.eps) {
                    ind &lt;- i + (-2:1)
                    print(cbind(dout[ind,c("ptt", "gmt", "class")],
                                row.number=ind))
                }
            }
        }
        if(any(dout$longitude &gt; 180)) {
            msg &lt;- paste("\nLongitudes contain values greater than 180,",
                         "assuming proj.4 +over\n\n")
            cat(msg)
            p4 &lt;- "+proj=longlat +ellps=WGS84 +over"
        }
        dout$class &lt;- ordered(dout$class,
                              levels=c("Z", "B", "A", "0", "1", "2", "3"))
        coordinates(dout) &lt;- c("longitude", "latitude")
        proj4string(dout) &lt;- CRS(p4)
        ##tor &lt;- TimeOrderedRecords(c("gmt", "ptt"))
        test &lt;- try(dout &lt;- trip(dout, c("gmt", "ptt")))
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


sepIdGaps &lt;- function(id, gapdata, minGap=3600 * 24 * 7) {
    toSep &lt;- tapply(gapdata, id,
                    function(x) which(diff(unclass(x) ) &gt; minGap))
    tripID &lt;- split(as.character(id), id)
    for (i in 1:length(tripID)) {
        this &lt;- toSep[[i]]
        thisID &lt;- tripID[[i]][1]
        if (length(this) &gt; 0) {
            for (n in 1:length(this)) {
                tripID[[i]][(this[n]+1):length(tripID[[i]])] &lt;-
                    paste(thisID, n + 1, sep="_")
            }
        }
    }
    unsplit(tripID, id)
}



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
</pre></body></html>