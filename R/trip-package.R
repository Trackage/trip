

#' Coercion from other classes to \code{trip} objects
#' 
#' Coercing objects to \code{trip} class
#' 
#' 
#' @name as.trip-methods
#' @aliases as.trip-methods as.trip as.trip,ltraj-method ltraj2trip
#' coerce,trip,ltraj-method
#' @docType methods
#' @param x,ltr ltraj object
#' @param list() Arguments passed to other methods. Ignored for \code{ltraj}
#' method.
#' @section Methods:
#' 
#' \describe{
#' 
#' \item{coerce}{\code{signature(from="ltraj", to="trip")}}
#' 
#' \item{as.trip}{\code{signature(x="ltraj")}}
#' 
#' }
#' @author Michael D. Sumner
#' @keywords methods spatial manip
#' @examples
#' 
#' 
#' ## Continuing the example from '?trip-methods:
#' utils::example("trip-methods", package="trip",
#'                ask=FALSE, echo=FALSE)
#' 
#' if (require(adehabitatLT)) {
#'     l <- as.ltraj.trip(tr)
#'     ltraj2trip(l)
#'     as.trip(l)
#' }
#' 
#' 
NULL





#' 
#' Functions to retrieve DateTime and ID data from within (Spatial) data
#' frames.
#' 
#' 
#' Functions for retrieving the names of the columns used for DateTime and ID,
#' as well as the data.
#' 
#' 
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
NULL





#' Class \code{"trip"}
#' 
#' 
#' An extension of \code{\link[sp]{SpatialPointsDataFrame}} by including
#' \code{"TimeOrderedRecords"}.  The records within the data frame are
#' explicitly ordered by DateTime data within IDs.
#' 
#' 
#' @name trip-class
#' @aliases trip-class dim.trip names.trip names<-.trip
#' [,trip,ANY,ANY,ANY-method [[<-,trip,ANY,missing-method lines,trip-method
#' plot,trip,missing-method points,trip-method recenter,trip-method
#' show,trip-method show,summary.TORdata-method spTransform,trip,CRS-method
#' spTransform,trip,character-method print.trip print.summary.TORdata
#' as.data.frame.summary.TORdata summary,trip-method text,trip-method
#' subset,trip-method as.data.frame.trip
#' @docType class
#' @param x,object,obj A \code{trip} object.
#' @param i,j For "[", rows and columns, respectively, of SpatialDataFrame to
#' subset.  For "[[<-", integer corresponding to the Datetime or ID vector, or
#' the string naming it.
#' @param list() Arguments passed to other methods.
#' @param ,drop,y,col Arguments passed to other methods.
#' @param value Object to replace time or ID column with.
#' @param CRSobj See \code{\link[sp]{spTransform}}.
#' @section Objects from the Class:
#' 
#' Objects can be created by calls of the form
#' \code{trip(obj="SpatialPointsDataFrame", TORnames="TimeOrderedRecords")}.
#' The object contains all the slots present within a
#' \code{\link[sp]{SpatialPointsDataFrame}}, particularly \code{data} which
#' contains columns of at least those specified by \code{TOR.columns}.
#' @author Michael D. Sumner
#' @seealso
#' 
#' \code{\link{trip}} for examples of directly using the class.
#' 
#' \code{\link{trip-accessors}} describes methods for accessing information on
#' \code{trip} objects.
#' @keywords classes
#' @examples
#' 
#' 
#' showClass("trip")
#' 
#' ## Examples of general methods
#' ## Continuing the example from '?trip-methods:
#' utils::example("trip-methods", package="trip",
#'                ask=FALSE, echo=FALSE)
#' summary(tr)
#' plot(tr)
#' lines(tr)
#' 
#' dim(tr)
#' names(tr)
#' subset(tr, id == "2")
#' as.data.frame(tr)
#' 
#' tr[1:3, ]
#' tr[, 1]
#' tr[[1]]
#' 
#' if (exists("porpoise")) {
#'     dim(porpoise)
#'     names(porpoise)
#'     porpoise[porpoise[["id"]] == "GUS", ]
#' }
#' 
#' 
NULL





#' Deprecated functions in trip
#' 
#' These functions will be declared defunct in a future release.
#' 
#' 
#' @aliases trip-deprecated trip.split.exact as.trip.SpatialLinesDataFrame
#' tripTransform
#' @param x see \code{\link{cut.trip}}
#' @param dates see \code{\link{cut.trip}}
#' @param from see \code{\link{as.SpatialLinesDataFrame.trip}}
#' @param crs CRS object, or PROJ.4 string accepted by \code{\link[sp]{CRS}}
#' @param \dots Further arguments to \code{\link[rgdal]{spTransform}}
#' @seealso
#' 
#' \code{\link{cut.trip}}, \code{\link{as.SpatialLinesDataFrame.trip}}
#' @keywords manip
NULL





#' Internal trip Functions
#' 
#' Internal trip functions
#' 
#' 
#' These are not to be called by the user (or in some cases are just waiting
#' for proper documentation to be written).
#' 
#' @aliases trip-internal .validTORdata .oc.col .single.trip.split .g2ow
#' .gcdist.c .intpFun .tripRbind .distances .abdali
#' @keywords internal
NULL





#' 
#' Function to handle animal track data, organized as \code{"trip"}s
#' 
#' 
#' Create an object of class \code{"trip"}, extending the basic functionality
#' of \code{\link[sp]{SpatialPointsDataFrame}} by specifying the data columns
#' that define the "TimeOrdered" quality of the records.
#' 
#' 
#' @name trip-methods
#' @aliases trip-methods trip trip,SpatialPointsDataFrame,ANY-method
#' trip,ANY,TimeOrderedRecords-method trip,trip,ANY-method
#' trip,trip,TimeOrderedRecords-method
#' @docType methods
#' @param obj A \code{\link[sp]{SpatialPointsDataFrame}}, or an object that can
#' be coerced to one, containing at least two columns with the DateTime and ID
#' data as per \code{TORnames}.  It can also be a \code{trip} object for
#' redefining \code{TORnames}.
#' @param TORnames Either a \code{TimeOrderedRecords} object, or a 2-element
#' character vector specifying the DateTime and ID column of \code{obj}
#' @return
#' 
#' A trip object, with the usual slots of a
#' \code{\link[sp]{SpatialPointsDataFrame}} and the added
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
#' 
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
#' @author Michael D. Sumner
#' @seealso
#' 
#' \code{\link{speedfilter}}, and \code{\link{tripGrid}} for simple(istic)
#' speed filtering and spatial time spent gridding.
#' @keywords methods manip
#' @examples
#' 
#' 
#' d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
#' coordinates(d) <- ~x+y
#' ## this avoids complaints later, but these are not real track data (!)
#' proj4string(d) <- CRS("+proj=laea")
#' (tr <- trip(d, c("tms", "id")))
#' 
#' %% I really don't want adehabitatMA to be loaded as a requirement here
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
#' \dontrun{
#' if (require(adehabitatLT)) {
#'     data(porpoise)
#'     porpoise <- as.trip(porpoise)
#'     proj4string(porpoise) <- CRS("+proj=utm +zone=21 +ellps=WGS84 +units=m +no_defs")
#'     summary(porpoise)
#' 
#' }
#' 
#' 
#' ## extended example to check that our projection metadata is correct
#' library(maptools)
#' data(wrld_simpl)
#' library(rgeos)
#' library(raster)
#' 
#' ## 3 degrees either side (for half a zone . . .)
#' ext <- as(extent(spTransform(porpoise, CRS(proj4string(wrld_simpl)))) + 3, "SpatialPolygons")
#' proj4string(ext) <- CRS(proj4string(wrld_simpl))
#' ## crop to the buffered tracks, and project to its native CRS
#' w <- spTransform(gIntersection(wrld_simpl[grep("United States", wrld_simpl$NAME), ], ext), CRS(proj4string(porpoise)))
#' 
#' plot(w)
#' lines(porpoise)
#' 
#' }
#' 
#' 
NULL



