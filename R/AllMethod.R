#' Function to handle animal track data, organized as \code{trip} objects
#'
#'
#' Create an object of class \code{trip}, extending the basic functionality
#' of \code{\link[sp]{SpatialPointsDataFrame-class}} by specifying the data columns
#' that define the "TimeOrdered" quality of the records.
#'
#' The original form of `trip()` required very strict input as a 'SpatialPointsDataFrame' and
#' specifying which were the time and ID columns, but the input can be more flexible. If the object is a
#' grouped data frame ('dplyr-style') then the (first) grouping is assumed to define individual trips and that
#' columns 1, 2, 3 are the x-, y-, time-coordinates in that order. It can also be a \code{trip} object for
#' redefining \code{TORnames}.
#'
#' The [trip()] function can ingest `track_xyt`, `telemetry`, `SpatialPointsDataFrame`, `sf`,
#' `trackeRdata`, `grouped_df`, `data.frame`, `tbl_df`, `mousetrap`, and in some cases
#' lists of those objects. Please get in touch if you think something that should work does not.
#'
#' Track data often contains problems, with missing values in location or time,
#' times out of order or with duplicated times. The `correct_all` argument is
#' set to `TRUE` by default and will report any inconsistencies. Data really should
#' be checked first rather than relying on this auto-cleanup. The following problems are common:
#' * duplicated records (every column with the same value in another row)
#' * duplicated date-time values
#' * missing date-time values, or missing x or y coordinates
#' * records out of order within trip ID
#'
#' For some data types there's no formal structure, but a simple convention such as
#' a set of names in a data frame. For example, the VTrack package has `AATAMS1` which may be
#' turned into a trip with
#' `trip(AATAMS1 %>% dplyr::select(longitude, latitude, timestamp, tag.ID, everything())`
#' In time we can add support for all kinds of variants, detected by the names and contents.
#'
#'
#' See [Chapter 2 of the trip thesis](https://eprints.utas.edu.au/12273/) for more details.
#' @name trip-methods
#' @aliases trip-methods trip trip,SpatialPointsDataFrame,ANY-method
#' trip,SpatialPointsDataFrame,TimeOrderedRecords-method
#' trip,ANY,TimeOrderedRecords-method trip,trip,ANY-method
#' trip,grouped_df,ANY-method trip,data.frame,ANY-method trip,track_xyt,ANY-method
#' trip,trackeRdata,ANY-method trip,mousetrap,ANY-method trip,sf,ANY-method
#' trip,telemetry,ANY-method trip,list,ANY-method
#' trip,trip,TimeOrderedRecords-method split,trip,ANY-method [,trip-method [,trip,ANY,ANY,ANY-method
#' [[<-,trip,ANY,missing-method trip<-,data.frame,character-method
#' @param obj A data frame, a grouped data frame or a \code{\link[sp]{SpatialPointsDataFrame-class}}
#' containing at least two columns with the DateTime and ID data as per \code{TORnames}.  See
#' Details.
#' @param TORnames Either a \code{TimeOrderedRecords} object, or a 2-element
#' character vector specifying the DateTime and ID column of \code{obj}
#' @param value A 4-element character vector specifying the X, Y, DateTime coordinates
#' and ID of \code{obj}.
#' @param correct_all logical value, if `TRUE` the input data is corrected for common problems
#' @param f grouping vector as per [split()]
#' @return
#'
#' A trip object, with the usual slots of a
#' \code{\link[sp]{SpatialPointsDataFrame-class}} and the added
#' \code{TimeOrderedRecords}. For the most part this can be treated as a
#' \code{data.frame} with \code{Spatial} coordinates.
#' @section Methods:
#'
#' Most of the methods available are by virtue of the sp package.  Some, such
#' as \code{split.data.frame} have been added to SPDF so that trip has the same
#' functionality.
#'
#' \describe{
#'
#' \item{trip}{\code{signature(obj="SpatialPointsDataFrame",
#' TORnames="ANY")}}The main construction.
#' \item{trip}{\code{signature(obj="SpatialPointsDataFrame",
#' TORnames="TimeOrderedRecords")}} Object and TimeOrdered records class
#' \item{trip}{\code{signature(obj="ANY", TORnames="TimeOrderedRecords")}:
#' create a \code{trip} object from a data frame.}
#'
#' \item{trip}{\code{signature(obj="trip", TORnames="ANY")}: (Re)-create a
#' \code{trip} object using a character vector for \code{TORnames}.}
#'
#' \item{trip}{\code{signature(obj="trip", TORnames="TimeOrderedRecords")}:
#' (re)-create a trip object using a \code{TimeOrderedRecords} object.}
#'
#' }
#' @seealso
#'
#' \code{\link{speedfilter}}, and \code{\link{tripGrid}} for simplistic
#' speed filtering and spatial time spent gridding.
#' @export
#' @importFrom sp coordinates
#' @export coordinates
#' @examples
#'
#'
#' d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
#'
#' ## the simplest way to create a trip is by order of columns
#'
#' trip(d)
#'
#' ## or a grouped data frame can be used, the grouping is used as the trip ID
#' ## library(dplyr)
#' ## # use everything() to keep all other columns
#' ## d %>% group_by(id) %>% select(x, y, tms, everything())
#'
#' sp::coordinates(d) <- ~x+y
#' ## this avoids complaints later, but these are not real track data (!)
#' sp::proj4string(d) <- sp::CRS("+proj=laea +ellps=sphere", doCheckCRSArgs = FALSE)
#' (tr <- trip(d, c("tms", "id")))
#'
#'  ## real world data in CSV
#' mi_dat <- read.csv(system.file("extdata/MI_albatross_sub10.csv", package = "trip"),
#'             stringsAsFactors = FALSE)
#' ## installed subset because the data is quite dense
#' ## mi_dat <- mi_dat[seq(1, nrow(mi_dat), by = 10), ]
#' mi_dat$gmt <- as.POSIXct(mi_dat$gmt, tz = "UTC")
#' mi_dat$sp_id <-  sprintf("%s%s_%s_%s", mi_dat$species,
#'          substr(mi_dat$breeding_status, 1, 1), mi_dat$band, mi_dat$tag_ID)
#' sp::coordinates(mi_dat) <- c("lon", "lat")
#' ## there are many warnings, but the outcome is fine
#' ## (sp_id == 'WAi_14030938_2123' has < 3 locations as does LMi_12143650_14257)
#' mi_dat <- trip(mi_dat, c("gmt", "sp_id") )
#' plot(mi_dat, pch = ".")
#' #lines(mi_dat)  ## ugly
#'
#' mi_dat_polar <- reproj(mi_dat, "+proj=stere +lat_0=-90 +lon_0=154 +datum=WGS84")
#' plot(mi_dat_polar, pch = ".")
#' lines(mi_dat_polar)
#' \dontrun{
#' ## a simple example with the common fixes required for basic track data
#'
#' dat <- read.csv("trackfile.csv")
#' names(dat)  ## e.g. [1] "long" "lat" "seal" "date" "local" "lq"
#' library(sp)
#' coordinates(dat) <- c("long", "lat")
#'
#' ## date/times may be in a particular time zone, please check
#' dat$gmt <- as.POSIXct(strptime(paste(dat$date, dat$local),
#'                       "%d-%b-%y %H:%M:%S"), tz="GMT")
#'
#' ## if there are problems in the data, this will error
#' tr <- trip(dat, c("gmt", "seal"))
#'
#' ## the following code tries to fix common problems
#'
#' ## remove completely-duplicated rows
#' dat <- dat[!duplicated(dat), ]
#' ## order the rows by seal, then by time
#' dat <- dat[order(dat$seal, dat$gmt), ]
#' ## fudge duplicated times
#' dat$gmt <- adjust.duplicateTimes(dat$gmt, dat$seal)
#'
#' ## finally, convert to Spatial and create trip object
#' coordinates(dat) <- c("long", "lat")
#' tr <- trip(dat, c("gmt", "seal"))
#' }
#'
#'
setGeneric("trip",
             function(obj, TORnames, correct_all = TRUE) standardGeneric("trip"))

if (!isGeneric("points"))
  setGeneric("points",
             function(x, ...) standardGeneric("points"))

if (!isGeneric("lines"))
  setGeneric("lines",
             function(x, ...) standardGeneric("lines"))

if (!isGeneric("text"))
  setGeneric("text",
             function(x, ...) standardGeneric("text"))

if (!isGeneric("subset"))
  setGeneric("subset",
             function(x, ...) standardGeneric("subset"))




##' TimeOrderedRecords
##'
##' Object to identify DateTimes and IDs in a Spatial object.
##'
##' @param x Character vector of 2 elements specifying the data columns of DateTimes and IDs
##' @return  \code{TimeOrderedRecords} holds a 2-element character vector, naming the data columns
##' of DateTimes and IDs.
##' @export
##' @examples
##' ##' tor <- TimeOrderedRecords(c("datetime", "ID"))
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
#' \code{\link[sp]{SpatialPointsDataFrame-class}}.
#'
#' \code{\link{trip}}
#' @keywords manip
#' @examples
#'
#'
#' tor <- TimeOrderedRecords(c("time", "id"))
#' getTORnames(tor)
#'
NULL

setOldClass("data.frame")

#' @rdname trip-accessors
#' @export
getTORnames <- function(obj) obj@TOR.columns

##' @rdname trip-accessors
##' @export
getTimeID <- function(obj) as.data.frame(obj)[, getTORnames(obj)]
assume_if_longlat <- function(x) {
  if (is.na(x@proj4string@projargs) && raster::couldBeLonLat(x, warnings = FALSE)) {
    warning("input looks like longitude/latitude data, assuming +proj=longlat +datum=WGS84")
    x@proj4string@projargs <- .llproj()
  }
  x
}
trip.grouped_df <- function(obj, ..., crs = NULL) {
  group_var <- setdiff(names(attr(obj, "groups")), ".rows")
  if (length(group_var) > 1) {
    group_var <- group_var[1]
    warning(sprintf("data is grouped by more than one variable, assuming '%s' as the correct one", group_var))
  }
  tor <- c(names(obj)[3], group_var)
  if (!inherits(obj[[tor[1]]], "POSIXct")) stop(sprintf("3rd column [%s] must be date-time", tor[1]))
  obj <- as.data.frame(as.list(obj), stringsAsFactors = FALSE) ## remove grouping
  sp::coordinates(obj) <- names(obj)[1:2]
  if (!is.null(crs)) sp::proj4string(obj)
  trip(obj, tor, ...)
}

setMethod("trip", signature(obj = "list", TORnames = "ANY"),
          function(obj, TORnames, correct_all = TRUE) {
            ## a dirty trick but will work for some stuffs
            chk <- try(trip(obj[[1]]), silent = TRUE)
            ## this a bit slow because trips get created, need rbind for trip
            if (!inherits(chk, "try-error")) {
              out <- do.call(rbind, lapply(obj, trip))
              tor <- getTORnames(chk)

            } else {
              print("problem with list of this type")
              stop(chk) ##sprintf("cannot interpret (list of) type %s", paste(class(obj[[1]]), collapse = ", ")))
            }
            trip(out, tor)
          })
setMethod("trip", signature(obj = "telemetry", TORnames = "ANY"),
          function(obj, TORnames, correct_all = TRUE) {
            telemetry2trip(obj)
          })
setMethod("trip", signature(obj="sf", TORnames="ANY"),
          function(obj, TORnames, correct_all = TRUE) {
            if (missing(TORnames)) TORnames <- names(obj)[1:2]
            gcol <- attr(obj, "sf_column")
            cls <- class(obj[[gcol]])[1]
            stopifnot(cls %in% c("sfc_POINT", "sfc_MULTIPOINT"))
            xy <- do.call(rbind, unclass(obj[[gcol]]))
            p4 <- attr(obj[[gcol]], "crs")$proj4string
            idx <- NULL
            if (cls == "sfc_MULTIPOINT") {
              stop("MULTIPOINT not yet supported")  ## unclear what to do, unless tor[1] is the *offset* for XYZ[,3]?
              ni <- unlist(lapply(obj[[gcol]], function(a) dim(a)[1]))
              idx <- rep(seq_len(nrow(obj)), ni)
            }
            obj[[gcol]] <- NULL
            obj <- as.data.frame(as.list(unclass(obj)), stringsAsFactors = FALSE)
            if (!is.null(idx)) obj <- obj[idx, ]
            if (correct_all) {

              obj <- force_internal(obj, TORnames)
            }
            coordnames <- utils::tail(make.names(c(names(obj), c("X", "Y")), unique = TRUE), 2)
            obj[[coordnames[1L]]] <- xy[,1L]
            obj[[coordnames[2L]]] <- xy[,2L]

            sp::coordinates(obj) <- coordnames
            sp::proj4string(obj) <- sp::CRS(p4, doCheckCRSArgs = FALSE)
            out <- new("trip", obj, TimeOrderedRecords(TORnames))
            assume_if_longlat(out)
          })

setMethod("trip", signature(obj = "mousetrap"),
          function(obj, TORnames, correct_all = TRUE) {
            dat <- data.frame(xpos = as.vector(t(obj$trajectories[,,2L])),
                              ypos = as.vector(t(obj$trajectories[,,3L])),
                        timestamps = as.vector(t(obj$trajectories[,,1L])))
            dat$timestamps <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "UTC") + dat$timestamps
            idx <- rep(seq_len(nrow(obj$data)), ncol(obj$trajectories))
            dat$id <- rownames(obj$data)[idx]
            warning("assuming UNIX epoch for timestamp, where zero is 1970-01-01 00:00:00 UTC")
            dat <- cbind(dat, obj$data[idx, ])
            bad <- is.na(dat$xpos) | is.na(dat$ypos) | is.na(dat$timestamps) | is.na(dat$id)
            if (sum(bad) > 0) {
              warning(sprintf("removing %i records with missing coordinate values", sum(bad)))
              dat <- dat[!bad, ]
            }
            sp::coordinates(dat) <- c("xpos", "ypos")
            trip(dat, c("timestamps", "id"))
          })
setMethod("trip", signature(obj = "trackeRdata"),
          function(obj, TORnames, correct_all = TRUE) {
            ns <- unlist(lapply(obj, function(df) dim(df)[1]))

            time <- do.call(c, lapply(obj, function(a) attr(a, "index")))
            d <- data.frame(sport = rep(attr(obj, "sport"),  ns),
                            utc = time, run_id = rep(seq_along(ns), ns), stringsAsFactors = FALSE)
            dat <- cbind(d, do.call(rbind, lapply(obj, unclass)))
            ## remove any NA coords ...
            bad <- (is.na(dat$longitude) | is.na(dat$longitude) |  is.na(dat$utc))
            if (sum(bad) > 0) {
              warning(sprintf("removing %i records with missing coordinate values", sum(bad)))
              dat <- dat[!bad, ]
            }
            sp::coordinates(dat) <- c("longitude", "latitude")
            sp::proj4string(dat) <- sp::CRS(.llproj(), doCheckCRSArgs = FALSE)

            trip(dat, c("utc", "run_id"))
          })
setMethod("trip", signature(obj="track_xyt", TORnames= "ANY"),
          function(obj, TORnames, correct_all = TRUE) {
            if (missing(TORnames)) TORnames <- c("t_", "id")
            TOR <- TimeOrderedRecords(TORnames)
            proj <- attr(obj, "crs")
            obj <- as.data.frame(as.list(obj), stringsAsFactors = FALSE)
            sp::coordinates(obj) <- c("x_", "y_")
            sp::proj4string(obj) <- proj
            trip(obj, TOR, correct_all = correct_all)
          })
setMethod("trip", signature(obj="grouped_df", TORnames= "ANY"),
          function(obj, TORnames, correct_all = TRUE) {
            trip.grouped_df(obj, correct_all = correct_all)
          })
setMethod("trip", signature(obj="data.frame",  TORnames= "ANY"),
          function(obj, TORnames, correct_all = TRUE) {
            ## asumme input is x, y, time, ID

            tor <- names(obj)[3:4]
            if (is.factor(obj[[tor[1]]])) {
              obj[[tor[1]]] <- levels(obj[[tor[1]]])[obj[[tor[1]]]]
            }
            if (is.character(obj[[tor[1]]])) {
              obj[[tor[1]]] <- as.POSIXct(obj[[tor[1]]], tz = "UTC")
            }
            TORnames <- TimeOrderedRecords(tor)

            if (!is.numeric(obj[[1]]) || !is.numeric(obj[[2]])) stop("first two columns must be numeric, x,y or longitude,latitude")
            sp::coordinates(obj) <- 1:2

            if (correct_all) {

              obj <- force_internal(obj, TORnames@TOR.columns)
            }

            out <- new("trip", obj, TORnames)
            assume_if_longlat(out)
          })
setMethod("trip", signature(obj="SpatialPointsDataFrame", TORnames="TimeOrderedRecords"),
          function(obj, TORnames, correct_all = TRUE) {

            if (correct_all) {

              obj <- force_internal(obj, TORnames@TOR.columns)
            }

            out <- new("trip", obj, TORnames)
            assume_if_longlat(out)
          })
setMethod("trip", signature(obj="SpatialPointsDataFrame", TORnames="ANY"),
          function(obj, TORnames, correct_all = TRUE) {
              if (is.factor(obj[[TORnames[2]]]))
                  obj[[TORnames[2]]] <- factor(obj[[TORnames[2]]])
              if (correct_all) {
                #print(bbox(obj))
                obj <- force_internal(obj, TORnames)
              }
              #print(bbox(obj))
              out <- new("trip", obj, TimeOrderedRecords(TORnames))
              #print(bbox(out))
              assume_if_longlat(out)
          })

setMethod("trip", signature(obj="ANY", TORnames="TimeOrderedRecords"),
          function(obj, TORnames, correct_all = TRUE) {
            if (correct_all) {
              obj <- force_internal(obj, TORnames@TOR.columns)
            }
              out <- new("trip", obj, TORnames)
              assume_if_longlat(out)
          })

setMethod("trip", signature(obj="trip", TORnames="TimeOrderedRecords"),
          function(obj, TORnames, correct_all = TRUE) {
            if (correct_all) {
              obj <- force_internal(obj, TORnames@TOR.columns)
            }
              out <- new("trip",
                  as(obj, "SpatialPointsDataFrame"),
                  TORnames)
              assume_if_longlat(out)
          })

setMethod("trip", signature(obj="trip", TORnames="ANY"),
          function(obj, TORnames, correct_all = TRUE) {
            if (correct_all) {
              obj <- force_internal(obj, TORnames)
            }
              out <- trip(as(obj, "SpatialPointsDataFrame"), TORnames)
              assume_if_longlat(out)
          })

triprepmethod <-   function(obj, value) {
  coordinates(obj) <- value[1:2]
  trip(obj, value[3:4])
}

#' @rdname trip-methods
#' @export
setGeneric("trip<-",
           function(obj, value) standardGeneric("trip<-"))


setReplaceMethod("trip",
                 signature(obj = "data.frame", value = "character"),
                 triprepmethod
               )

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



split.trip <-  function(x, f, drop = FALSE, ...) {
  lapply(split(x = seq_len(nrow(x)), f = f, drop = drop, ...),
         function(ind) x[ind, , drop = FALSE])
}
#' @rdname trip-methods
#' @exportMethod split
setMethod("split", signature(x = "trip", f = "ANY"),
         split.trip
          )

#' @exportMethod lines
setMethod("lines", signature(x="trip"),
          function(x,
                   col=hsv(seq(0, 0.9, length = length(unique(x[[getTORnames(x)[2]]]))),
                     0.8, 0.95),
                   ...) {
              plot(as(x, "SpatialLinesDataFrame"),  col=col, add=TRUE, ...)

          })
#' @exportMethod  plot
setMethod("plot", signature(x="trip", y="missing"),
          function(x, y, ...) {
              plot(as(x, "SpatialPoints"), ...)
          })


###_ + Subsetting trip

#' @exportMethod subset
setMethod("subset", signature(x="trip"),
          function(x,  ...) {
              spdf <- subset(as(x, "SpatialPointsDataFrame"), ...)
              tor <- getTORnames(x)
              if ( is.factor(spdf[[tor[2]]]))
                  spdf[[tor[2]]] <- factor(spdf[[tor[2]]])
              if (any(is.na(match(tor, names(spdf))))) {
                  msg <- paste("trip-defining Date or ID columns dropped,",
                               "reverting to SpatialPointsDataFrame\n\n")
                  warning(msg)
                  return(spdf)
              } else {
                  tst <- any(tapply(spdf[[tor[1]]],
                                    spdf[[tor[2]]], length) < 3)
                  if (tst) {
                      msg <- paste("subset loses too many locations,",
                               "reverting to SpatialPointsDataFrame\n\n")
                      warning(msg)
                      return(spdf)
                  } else return(trip(spdf, tor))
              }
          })

##' @param x trip object
##' @param i,j,\dots indices specifying elements to extract
##' @param drop unused but necessary for method consistency
##' @rdname trip-methods
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
                      return(trip(spdf, tor, correct_all = F))
                  }
              }
          })


###_ + Summary, print, and show

#' @exportMethod summary
setMethod("summary", signature(object="trip"),
          function(object, ...) {
              obj <- list(spdf=summary(as(object,
                            "SpatialPointsDataFrame")))
              tids <- getTimeID(object)
              time <- tids[, 1]
              ids <- tids[, 2]
              ## list of distances only, km/hr or units of projection
              dists <- .distances(object)
              #rmsspeed <- split(speedfilter(object, max.speed = 1, test = TRUE)$rms, ids)

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
                        maxSpeed = x$maxSpeed, stringsAsFactors = FALSE)
                        #meanRMSspeed = x$meanRMSspeed,
                        #maxRMSspeed = x$maxRMSspeed)
  dsumm
}

#' @rdname trip-accessors
#' @method print summary.TORdata
#' @param x trip object
#' @param \dots currently ignored
#' @export
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

#' @exportMethod show
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
              projargs <- CRS(proj4string(obj), doCheckCRSArgs = FALSE)
              crds <- coordinates(obj)
              inout <- (crds[, 1] < 0)
              if (all(inout)) {
                  crds[, 1] <- crds[, 1] + 360
                  if (!is.na(proj)) projargs <- CRS(proj4string(obj), doCheckCRSArgs = FALSE)
              } else {
                  if (any(inout)) {
                      crds[, 1] <- ifelse(inout, crds[, 1] + 360,
                                          crds[, 1])
                      if (!is.na(proj))
                          projargs <- CRS(proj4string(obj), doCheckCRSArgs = FALSE)
                  }
              }
              trip(new("SpatialPointsDataFrame",
                       SpatialPoints(crds, projargs),
                       data=obj@data, coords.nrs=obj@coords.nrs),
                   obj@TOR.columns)
          })


#' @importFrom sp spTransform
setMethod("spTransform", signature=signature(x="trip", CRSobj="character"),
         function(x, CRSobj, ...) {
           .Deprecated("reproj", msg = "trip doesn't use sp/rgdal spTransform now, done with 'reproj')")
           reproj(x, CRSobj)
         })


setMethod("spTransform", signature("trip", "CRS"),
          function(x, CRSobj, ...) {
            .Defunct("reproj", msg = "trip doesn't use sp/rgdal spTransform now\n (warning in case you should do your own reprojection with sf or whatever)")
           reproj(x, CRSobj@proj4string@projargs)
          })

