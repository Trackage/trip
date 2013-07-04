# $Id: AllMethod.R 115 2013-04-25 17:26:50Z sluque $

###_ + TimeOrderedRecords

TimeOrderedRecords <- function(x) {
    new("TimeOrderedRecords", TOR.columns=x)
}







#' 
#' Functions to retrieve DateTime and ID data from within (Spatial) data
#' frames.
#' 
#' 
#' Functions for retrieving the names of the columns used for DateTime and ID,
#' as well as the data.
#' 
#' @name trip-accessors
#' @aliases trip-accessors getTORnames getTimeID
#' @param obj \code{trip} object.
#' @return
#' 
#' \code{getTORnames} retrieves the column names from an object extending the
#' class \code{TimeOrderedRecords}, and \code{getTimeID} returns the data as a
#' data frame from an object extending the class \code{TimeOrderedRecords}.
#' @seealso
#' 
#' \code{\link{trip-class}}, for the use of this class with
#' \code{\link[sp]{SpatialPointsDataFrame}}.
#' 
#' \code{\link{trip}}
#' @keywords manip
#' @examples
#' 
#' 
#' tor <- TimeOrderedRecords(c("time", "id"))
#' getTORnames(tor)
#' 
#' 
getTORnames <- function(obj) obj@TOR.columns

##' @rdname trip-accessors
getTimeID <- function(obj) as.data.frame(obj)[, getTORnames(obj)]


###_ + trip

setMethod("trip", signature(obj="SpatialPointsDataFrame", TORnames="ANY"),
          function(obj, TORnames) {
              if (is.factor(obj[[TORnames[2]]]))
                  obj[[TORnames[2]]] <- factor(obj[[TORnames[2]]])
              new("trip", obj, TimeOrderedRecords(TORnames))
          })

setMethod("trip", signature(obj="ANY", TORnames="TimeOrderedRecords"),
          function(obj, TORnames) {
              new("trip", obj, TORnames)
          })

setMethod("trip", signature(obj="trip", TORnames="TimeOrderedRecords"),
          function(obj, TORnames) {
              new("trip",
                  as(obj, "SpatialPointsDataFrame"),
                  TORnames)
          })

setMethod("trip", signature(obj="trip", TORnames="ANY"),
          function(obj, TORnames) {
              trip(as(obj, "SpatialPointsDataFrame"), TORnames)
          })

setReplaceMethod("[[",
                 signature(x="trip", i="ANY", j="missing", value="ANY"),
                 function(x, i, j, value) {
                     tor <- getTORnames(x)
                     x <- as(x, "SpatialPointsDataFrame")
                     x[[i]] <- value
                     trip(x, tor)
                 })

## S3 versions
dim.trip <- function(x) dim(as(x, "SpatialPointsDataFrame"))

as.data.frame.trip <- function(x, ...) {
    as.data.frame(as(x, "SpatialPointsDataFrame"), ...)
}

names.trip <- function(x) names(as(x, "SpatialPointsDataFrame"))

"names<-.trip" <- function(x, value) {
    names(x@data) <- value
    x@TOR.columns <- value
    x
}


###_ + sp methods

setMethod("points", signature(x="trip"),
          function(x, ...) points(as(x, "SpatialPointsDataFrame"), ...))
setMethod("text", signature(x="trip"),
          function(x, ...) text(as(x, "SpatialPointsDataFrame"), ...))

#setMethod("split", "SpatialPointsDataFrame", split.data.frame)

## setMethod("spTransform", signature=signature(x="trip", CRSobj="CRS"),
##           function(x, CRSobj, ...) tripTransform(x, CRSobj, ...))

## setMethod("spTransform", signature=signature(x="trip", CRSobj="character"),
##           function(x, CRSobj, ...) tripTransform(x, CRSobj, ...))

## MDS 2010-07-06
setMethod("lines", signature(x="trip"),
          function(x,
                   col=hsv(seq(0, 0.9, length = length(unique(x[[getTORnames(x)[2]]]))),
                     0.8, 0.95),
                   ...) {
              plot(as(x, "SpatialLinesDataFrame"),  col=col, add=TRUE, ...)
           
          })

setMethod("plot", signature(x="trip", y="missing"),
          function(x, y, ...) {
              plot(as(x, "SpatialPoints"), ...)
          })


###_ + Subsetting trip

setMethod("subset", signature(x="trip"),
          function(x,  ...) {
              spdf <- subset(as(x, "SpatialPointsDataFrame"), ...)
              tor <- getTORnames(x)
              if ( is.factor(spdf[[tor[2]]]))
                  spdf[[tor[2]]] <- factor(spdf[[tor[2]]])
              if (any(is.na(match(tor, names(spdf))))) {
                  msg <- paste("trip-defining Date or ID columns dropped,",
                               "reverting to SpatialPointsDataFrame\n\n")
                  cat(msg)
                  return(spdf)
              } else {
                  tst <- any(tapply(spdf[[tor[1]]],
                                    spdf[[tor[2]]], length) < 3)
                  if (tst) {
                      msg <- paste("subset loses too many locations,",
                               "reverting to SpatialPointsDataFrame\n\n")
                      cat(msg)
                      return(spdf)
                  } else return(trip(spdf, tor))
              }
          })

setMethod("[", signature(x="trip"),
          function(x, i, j, ... , drop=TRUE) {
              missing.i <- missing(i)
              missing.j <- missing(j)
              nargs <- nargs() # e.g., a[3,] gives 2 for nargs, a[3] gives 1.
              if (missing.i && missing.j) {
                  i <- j <- TRUE
              } else if (missing.j && !missing.i) {
                  if (nargs == 2) {
                      j <- i; i <- TRUE
                  } else j <- TRUE
              } else if (missing.i && !missing.j) i <- TRUE
              if (is.matrix(i)) {
                  msg <- paste("matrix argument not supported in",
                               "SpatialPointsDataFrame selection")
                  stop(msg)
              }
              if (any(is.na(i)))
                  stop("NAs not permitted in row index")
              spdf <- as(x, "SpatialPointsDataFrame")[i, j, ..., drop=drop]
              tor <- getTORnames(x)
              if (is.factor(spdf[[tor[2]]]))
                  spdf[[tor[2]]] <- factor(spdf[[tor[2]]])
              if (any(is.na(match(tor, names(spdf))))) {
                  msg <- paste("trip-defining Date or ID columns dropped,",
                               "reverting to SpatialPointsDataFrame\n\n")
                  cat(msg)
                  return(spdf)
              } else {
                  tst <- any(tapply(spdf[[tor[1]]],
                                    spdf[[tor[2]]], length) < 3)
                  if (tst) {
                      msg <- paste("subset loses too many locations,",
                                   "reverting to SpatialPointsDataFrame\n\n")
                      cat(msg)
                      return(spdf)
                  } else {
                      return(trip(spdf, tor))
                  }
              }
          })


###_ + Summary, print, and show

setMethod("summary", signature(object="trip"),
          function(object, ...) {
              obj <- list(spdf=summary(as(object,
                            "SpatialPointsDataFrame")))
              tids <- getTimeID(object)
              time <- tids[, 1]
              ids <- tids[, 2]
              ## list of distances only, km/hr or units of projection
              dists <- .distances(object)
              rmsspeed <- split(speedfilter(object, max.speed = 1, test = TRUE)$rms, ids)

              ## list of time diferences only, in hours
              dtimes <- lapply(split(time, ids), function(x) diff(unclass(x)/3600))
              speeds <- vector("list", length(dtimes))
              for (i in seq_along(speeds)) speeds[[i]] <- dists[[i]] / dtimes[[i]]

              obj <- within(obj, {
                  class <- class(object)
                  tmins <- tapply(time, ids, min) +
                      ISOdatetime(1970, 1, 1, 0, 0,0, tz="GMT")
                  tmaxs <- tapply(time, ids, max) +
                      ISOdatetime(1970, 1, 1, 0, 0,0, tz="GMT")
                  tripID <- levels(factor(ids))
                  nRecords <- tapply(time, ids, length)
                  TORnames <- getTORnames(object)
                  tripDuration <- tapply(time, ids, function(x) {
                      x <- format(diff(range(x)))
                  })
                  tripDurationSeconds <- tapply(time, ids, function(x) {
                      x <- diff(range(unclass(x)))
                  }
                                                )
                  tripDistance <- sapply(dists, sum)
                  meanSpeed <- sapply(speeds, mean)
                  maxSpeed <- sapply(speeds, max)
                  meanRMSspeed <- sapply(rmsspeed, mean, na.rm = TRUE)
                  maxRMSspeed <- sapply(rmsspeed, max, na.rm = TRUE)
              })
              class(obj) <- "summary.TORdata"
              ## invisible(obj)
              obj
          })

as.data.frame.summary.TORdata <- function(x, row.names = NULL, optional = FALSE, ...) {
        dsumm <- data.frame(tripID=x$tripID,
                        No.Records=x$nRecords,
                        startTime=x$tmins,
                        endTime=x$tmaxs,
                        tripDuration=x$tripDuration,
                        tripDistance=x$tripDistance,
                        meanSpeed = x$meanSpeed,
                        maxSpeed = x$maxSpeed,
                        meanRMSspeed = x$meanRMSspeed,
                        maxRMSspeed = x$maxRMSspeed)
  dsumm
}

print.summary.TORdata <- function(x, ...) {
    dsumm <- as.data.frame(x)
  torns <- x[["TORnames"]]
    names(dsumm)[1] <- paste(names(dsumm)[1],
                             " (\"", torns[2], "\")", sep="")
    names(dsumm)[3] <- paste(names(dsumm)[3],
                             " (\"", torns[1], "\")", sep="")
    names(dsumm)[4] <- paste(names(dsumm)[4],
                             " (\"", torns[1], "\")", sep="")


    rownames(dsumm) <- seq(nrow(dsumm))
    ## dsumm <- as.data.frame(lapply(dsumm, as.character))
    cat(paste("\nObject of class ", x[["class"]], "\n", sep=""))
    print(format(dsumm, ...))
    tripDurationSeconds <- sum(x$tripDurationSeconds)
    tripDurationHours <- sum(x$tripDurationSeconds) / 3600
    cat(paste("\nTotal trip duration: ",
              tripDurationSeconds, " seconds (",
              as.integer(tripDurationHours), " hours, ",
              round((tripDurationHours -
                     as.integer(tripDurationHours)) * 3600),
              " seconds)\n", sep=""))
    cat(paste("\nDerived from Spatial data:\n\n", sep=""))
    print(x$spdf)
    cat("\n")
}

setMethod("show", signature(object="summary.TORdata"),
          function(object) print.summary.TORdata(object))

print.trip <- function(x, ...) {
    xs <- summary(x)
    dsumm <- data.frame(tripID=xs$tripID,
                        No.Records=xs$nRecords,
                        startTime=xs$tmins,
                        endTime=xs$tmaxs,
                        tripDuration=xs$tripDuration)
    torns <- xs[["TORnames"]]
    names(dsumm)[1] <- paste(names(dsumm)[1], " (\"",
                             torns[2], "\")", sep="")
    names(dsumm)[3] <- paste(names(dsumm)[3], " (\"",
                             torns[1], "\")", sep="")
    names(dsumm)[4] <- paste(names(dsumm)[4], " (\"",
                             torns[1], "\")", sep="")
    rownames(dsumm) <- 1:nrow(dsumm)
    ## dsumm <- as.data.frame(lapply(dsumm, as.character))
    cat(paste("\nObject of class ", xs[["class"]], "\n", sep=""))
    print(format(dsumm, ...))
    cat("\n")
    nms <- names(x)
    clss <- unlist(lapply(as.data.frame(x@data), function(x) class(x)[1]))
    sdf <- data.frame(data.columns=nms, data.class=clss)
    sdf[[" "]] <- rep("", nrow(sdf))
    sdf[[" "]][nms == torns[1]] <- "**trip DateTime**"
    sdf[[" "]][nms == torns[2]] <- "**trip ID**      "
    row.names(sdf) <- seq(nrow(sdf))
    print(sdf)
    cat("\n")
}

setMethod("show", signature(object="trip"),
          function(object) print.trip(object))

setMethod("recenter", signature(obj="trip"),
          function(obj) {
              proj <- is.projected(obj)
              if (is.na(proj)) {
                  msg <- paste("unknown coordinate reference system:",
                               "assuming longlat")
                  warning(msg)
                  ## projargs <- CRS("+proj=longlat")
              }
              if (!is.na(proj) & proj) {
                  msg <- paste("cannot recenter projected coordinate",
                               "reference system")
                  stop(msg)
              }
              projargs <- CRS(proj4string(obj))
              crds <- coordinates(obj)
              inout <- (crds[, 1] < 0)
              if (all(inout)) {
                  crds[, 1] <- crds[, 1] + 360
                  if (!is.na(proj)) projargs <- CRS(paste(proj4string(obj),
                                                          "+over"))
              } else {
                  if (any(inout)) {
                      crds[, 1] <- ifelse(inout, crds[, 1] + 360,
                                          crds[, 1])
                      if (!is.na(proj))
                          projargs <- CRS(paste(proj4string(obj), "+over"))
                  }
              }
              trip(new("SpatialPointsDataFrame",
                       SpatialPoints(crds, projargs),
                       data=obj@data, coords.nrs=obj@coords.nrs),
                   obj@TOR.columns)
          })


###_ + Coercions

ltraj2trip <- function (ltr)
{
    require(adehabitatLT) ||
        stop("adehabitatLT package is required, but unavailable")
    if (!inherits(ltr, "ltraj"))
        stop("ltr should be of class \"ltraj\"")
    ltr <-  lapply(ltr, function(x) {
        x$id=attr(x,  "id")
        x$burst=attr(x,  "burst")
        x})
    tr <- do.call("rbind", ltr)
    class(tr) <- "data.frame"
    xy <- tr[!is.na(tr$x), c("x", "y")]
    tr <- tr[!is.na(tr$x), ]
    tr$y <- tr$x <- NULL
    res <- SpatialPointsDataFrame(xy, tr)
    trip(res, c("date", "id"))
}

setMethod("as.trip", signature(x="ltraj"),
          function(x, ...) ltraj2trip(x))

setAs("ltraj", "trip", function(from) as.trip(from))


###_ + Tests



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
