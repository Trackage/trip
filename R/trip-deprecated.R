#' Deprecated functions in trip
#' 
#' These functions will be declared defunct in a future release.
#' 
#' @name trip-deprecated
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
trip.split.exact <- function(x, dates) {
  .Deprecated("cut.trip")
  cut(x, dates)
}

##' @rdname trip-deprecated
as.trip.SpatialLinesDataFrame <- function(from) {
  .Deprecated("as.SpatialLinesDataFrame.trip")
  as.SpatialLinesDataFrame.trip(from)
}

## removed depends sp, suggests rgdal deprecate this, replace with
## spTransform method below

##' @rdname trip-deprecated
tripTransform <- function(x, crs, ...) {
  .Deprecated("spTransform")
  if (! inherits(crs, "CRS")) crs <- CRS(crs)
  spTransform(x, crs, ...)
}