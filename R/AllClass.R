# $Id: AllClass.R 81 2013-03-21 17:26:50Z sluque $

##' The TimeOrderedRecords class
##'
##' A simple class to act as a place-holder for DateTime and ID records in spatial data.
##'
##' The main use of this class and creator function is for
##' \code{\link[sp]{SpatialPointsDataFrame}}s which are used with
##' TimeOrderedRecords for the class \code{trip}.
##'                                        #'
##'@section Slots:
##'  \describe{
##'    \item{\code{TOR.columns}:}{2-element vector of class \code{"character"}}
##'  }
##'
##' @param x Character vector of 2 elements specifying the data columns of DateTimes and IDs
##' @name TimeOrderedRecords
##' @rdname TimeOrderedRecords-class
##' @aliases TimeOrderedRecords-class
##' @exportClass TimeOrderedRecords
##' @return  \code{TimeOrderedRecords} holds a 2-element character vector, naming the data columns
##' of DateTimes and IDs.
##' @note  Future versions may change significantly, this class is
##' very basic and could probably be implemented in a better
##' way. Specifying TOR columns by formula would be a useful addition.
##' @seealso \code{\link{trip}} for creating trip objects, and \code{\link{trip-class}} for that class
##' @examples
##' tor <- new("TimeOrderedRecords", TOR.columns=c("datetime", "ID"))
##' tor <- TimeOrderedRecords(c("datetime", "ID"))
setClass("TimeOrderedRecords", representation(TOR.columns="character"))

setValidity("TimeOrderedRecords", function(object) {
    if (! is.character(object@TOR.columns) |
        !is.vector(object@TOR.columns)) {
        stop("TimeOrderedRecords data names must be character vector")
        ## also support length == 1?
        if (length(object@TOR.columns) > 2)
            stop("TimeOrderedRecords data names must be of length 2")
        TRUE
    }
})

setClass("trip",
         contains=c("TimeOrderedRecords", "SpatialPointsDataFrame"))


.validTORdata <- function(object) {
    if (!is(object@data, "data.frame"))
        stop("only data frames supported for data slots")
    tid <- as.data.frame(object@data[, object@TOR.columns])
    if (length(tid) == 0)
        stop("timeIDs cannot have zero length")
    if (nrow(tid) < 1)
        stop("no timeIDs set: too few rows")
    if (ncol(tid) < 2)
        stop("no timeIDs set: too few columns")
    if (any(duplicated(as.data.frame(object))))
        stop("duplicated records within data")
    time <- tid[, 1]
    id <- tid[, 2]
    TORlevs <- levels(factor(id))
    if (!is(time, "POSIXt"))
        stop("trip only handles dates and times as POSIXt objects")
    ## mdsumner ID could not be character, because of finite test 2010-04-28
    bad1 <- c(is.na(time), !is.finite(time))
    if (any(bad1))
        return("time data contains missing or non finite values")
    if (any(is.na(id)))
        return("id data contains missing values")
    if (is.numeric(id) & any(!is.finite(id)))
        return("id data contains non-finite values")
    d <- unlist(tapply(time, id, diff))
    if (any(d < 0))
        return("date-times not in order within id")
    if (any(d == 0))
        return("date-times contain duplicates within id")
    short <- which(unlist(tapply(time, id, length)) < 3)
    ## maybe trip enforces this
    if (length(short) > 0) {
        mess <- "\n  fewer than 3 locations for ids:\n"
        mess <- paste(mess,
                      paste(TORlevs[short], collapse=","),
                      sep="")
        return(mess)
    }
    return(TRUE)
}

setValidity("trip", trip:::.validTORdata)

## We don't need an S4 class for this, but we do want S4 methods
setOldClass("summary.TORdata")

if (!isClass("ltraj")) setClass("ltraj")
