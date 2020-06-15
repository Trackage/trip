
#' Function to ensure dates and times are in order with trip ID
#'
#'
#' A convenience function, that removes duplicate rows, sorts by the date-times
#' within ID, and removes duplicates from a data frame or
#' SpatialPointsDataFrame.
#'
#'
#' @param x \code{\link{data.frame}} or
#' \code{\link[sp]{SpatialPointsDataFrame-class}}
#' @param tor character vector of names of date-times and trip ID columns
#' @return \code{\link{data.frame}} or
#' \code{\link[sp]{SpatialPointsDataFrame-class}}.
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
    crdnames <- rownames(bbox(x))
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    #crdnames <- setdiff(names(x), prenames)

  }


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


  ## checks for tooshorts must happen AFTER duplicates are removed ...
  tooshort <- tapply(x[[tor[2]]], x[[tor[2]]], function(x) length(x) < 3)
  levs <- unique(x[[tor[2]]])

  if (any(tooshort)) {
    warning(sprintf("removing trip IDs that have too few elements (<3):  \n'%s'",
                    paste(names(tooshort)[tooshort], collapse = ",")))
    x <- x[x[[tor[2]]] %in% levs[!tooshort], ]
  }
  if (is.factor(x[[tor[2]]])) x[[tor[2]]] <- factor(x[[tor[2]]])
  adjusted <- adjust.duplicateTimes(x[[tor[1]]], x[[tor[2]]])

  diffs <- abs(unclass(adjusted) - unclass(x[[tor[1]]])) > 0
  if (any(diffs)) {
    warning(sprintf("updating (%i) duplicated time records by a small adjustment", sum(diffs)))
    x[[tor[1]]] <- adjusted
  }
  if (isSpatial) {
    coordinates(x) <- crdnames
    x@proj4string@projargs <- proj
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














